
# building ----------------------------------------------------------------

# update the select input field to update the choices
# with a list of existing consumption record id
observeEvent(building_table_consumption_record(), {
    # make sure that there are records in the building consumption record table
    req(
        nrow(building_table_consumption_record()) > 0 
    )
    
    # get the id list of building consumption records
    id_list <<- building_table_consumption_record() |>
        pull(id)
    
    updateSelectInput(
        session,
        "id_emission_record_building",
        choices = c(
            "Select an id" = "",
            id_list
        )
    )
})

# observe the selected id to display the asset name of the selected id
selected_building_asset <- reactiveVal(NULL)

observeEvent(input$id_emission_record_building, {
    # get the asset name of the selected id
    asset_name <- building_table_consumption_record() |> 
        filter(id == input$id_emission_record_building) |> 
        pull(asset_name)
    
    selected_building_asset(asset_name)
})

# initial table
emission_record_building <- reactiveVal(NULL)

# function to cache database
load_emission_record_building <- function() {
    data <- dbGetQuery(
        pool,
        "SELECT *
            FROM emission_record_building") |> 
        mutate(
            creation_time = 
                as_datetime(
                    creation_time, 
                    tz = tz(Sys.timezone())
                ),
            start_date = as_date(start_date),
            end_date = as_date(end_date)
        )
    
    emission_record_building(data)
}

# initialise the database at the start
observe({load_emission_record_building()})


# calculate one record ----------------------------------------------------


observeEvent(input$add_emission_record_building, {
    req(nzchar(input$id_emission_record_building))
    
    # extract the row with the selected id
    new_record <<- building_table_consumption_record() |> 
        filter(id == input$id_emission_record_building)
    
    # extract the country of that consumption record
    new_record_country <- new_record |> 
        pull(country)
    
    # get a country list in the ele grid mix table
    country_list <- emission_factor_grid() |> 
        dplyr::distinct(country) |> 
        pull(country)
    
    # check if the consumption record's country is in the country list
    if(!(new_record_country %in% country_list)) {
        
        showNotification(
            "No Grid Mix Info of the Selected country",
            type = "warning"
        )
        
        return()
    } else {}
    
    # get the grid mix ef of the selected country
    selected_country_grid_ef <- emission_factor_grid() |> 
        filter(country == new_record_country) |> 
        pull(emission_factor)
    
    # get the consumption of the selected record
    selected_record_consumption <- new_record$consumption
    
    # calculate the LB emission
    LBEmission <- selected_country_grid_ef * selected_record_consumption / 1000
    
    # format the new_record to fit the emission record table in the DB
    formatted_new_record <- new_record |> 
        select(id, asset_name, fuel_type) |> 
        mutate(LB_emission = LBEmission,
               start_date = new_record$start_date,
               end_date = new_record$end_date,
               creation_time = Sys.time()) |> 
        rename("consumption_record_id" = "id")
    
    # check if a record with the same consumption record id already exists
    # check if there are any records in the emission record table first
    if(nrow(emission_record_building()) == 0) {}
    else if(formatted_new_record$consumption_record_id %in%
            emission_record_building()$consumption_record_id) {
        showNotification(
            "Consumption record already exists",
            type = "warning"
        )
        
        return()
        
    } else {}
    
    # update the reactive value with new record
    dbWriteTable(pool,
                 "emission_record_building",
                 formatted_new_record, append = TRUE)
    
    # refresh the table
    load_emission_record_building()
    
    # clear the input fields
    updateSelectInput(
        session,
        "id_emission_record_building",
        selected = ""
    )
    
    showNotification("New building emission record added", 
                     type = "message",
                     closeButton = TRUE)
    
})


# calculate all records ---------------------------------------------------

observeEvent(input$calculate_all_emission_record, {
    req(nrow(building_table_consumption_record()) > 0) # make sure that there are 
    # records in consumption_record_building
    
    # get the full list of consumption record id in the global environment
    # and filter it by the consumption record ids that are already in the 
    # emission record table
    
    # get the id list in the emission record table first
    cons_id_in_emi_table <- emission_record_building() |> 
        pull(consumption_record_id)
    
    # filter the total id list to exclude the ids that already exist
    id_to_calculate <- setdiff(id_list, cons_id_in_emi_table)
    
    # extract the country list
    country_list_required <- building_table_consumption_record() |> 
        filter(id %in% id_to_calculate) |> 
        distinct(country) |> 
        pull(country)
    
    # see what countries are available with ele grid mix
    ef_country_list <- emission_factor_grid() |> 
        distinct(country) |> 
        pull(country)
    
    # check if the required countries are in the available country list
    if(!all(country_list_required %in% ef_country_list)) { # if any value is false
        showNotification(
            session,
            "At least one country's emission factor is not available",
            type = "warning"
        )
        return()
    } else {}
    
    # get the consumption records with the id
    selected_consumption_records <- building_table_consumption_record() |> 
        filter(id %in% id_to_calculate)
    
    # get the grid mix ef of the selected countries
    selected_country_grid_ef <- emission_factor_grid() |> 
        filter(country %in% country_list_required)
    
    # join the consumption record and the ef record by the country and calculate
    # the LB emission
    merged_emission_record_table <- selected_consumption_records |> 
        left_join(selected_country_grid_ef, by = "country") |> 
        mutate(LB_emission = consumption * emission_factor / 1000)
    
    # format the table to fit the emission record table in the DB
    formatted_new_record <- merged_emission_record_table |>
        select(`id.x`, asset_name, fuel_type, 
               LB_emission, start_date, end_date) |>
        mutate(creation_time = Sys.time()) |>
        rename("consumption_record_id" = "id.x")
    
    # update the reactive value with new record
    dbWriteTable(pool,
                 "emission_record_building",
                 formatted_new_record, append = TRUE)
    
    # refresh the table
    load_emission_record_building()
    
    showNotification(
        "All available consumption records are calculated!",
        type = "message"
    )
})


