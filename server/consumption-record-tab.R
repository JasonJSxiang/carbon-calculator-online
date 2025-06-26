# sidebar -----------------------------------------------------------------

## update input fields ####

# country

# building
observe({
    
    building_country_list <- asset_table_building() |> 
        distinct(country) |> 
        pull(country)
    
    updateSelectInput(
        session,
        "building_country_consumption_record",
        choices = c(
            "Select a country" = "",
            building_country_list
        )
    )
})

# asset names

# building
observeEvent(input$building_country_consumption_record, {
    req(nzchar(input$building_country_consumption_record))
    
    # filtered asset list
    filtered_list <- asset_table_building() |> 
        filter(country == input$building_country_consumption_record) |> 
        distinct(asset_name) |> 
        pull(asset_name)
    
    updateSelectInput(session,
                      "building_asset_consumption_record",
                      choices = c("Select an asset" = "",
                                  filtered_list))
})

# start and end dates

# building
observeEvent(input$building_year_consumption_record, {
    req(nzchar(input$building_year_consumption_record))
    
    updateDateRangeInput(session,
                         "building_date_range_consumption_record",
                         min = 
                             as.Date(
                                 paste0(input$building_year_consumption_record,
                                        "-01-01")),
                         max = as.Date(
                             paste0(input$building_year_consumption_record,
                                    "-12-31"),
                             start = Sys.Date(),
                             end = Sys.Date())
    )
    
})


# emission source

# building
observeEvent(input$building_asset_consumption_record, {
    
    # get the applicable emission sources fromt the selected asset
    emi_list <- asset_table_building() |> 
        filter(asset_name == input$building_asset_consumption_record) |> 
        distinct(applicable_emission_source) |> 
        pull(applicable_emission_source) |> 
        str_split(";") |> 
        unlist() |> 
        sort()
    
    # update the emission sources drop down menu with the selected asset
    updateSelectInput(session, "fuel_select_building_consumption_record",
                      choices = c("Select a fuel type" = "",
                                  emi_list))
    
})

## renewable energy ui ####

# pop up RE yes no question when electricity is selected as the fuel type
output$renewable_energy_ui <- renderUI({
    req(input$fuel_select_building_consumption_record == "Electricity")
    
    radioButtons("renewable_yes_no_consumption_record",
                 "Is the energy from renewable source?*",
                 choices = c("Yes",
                             "No"),
                 selected = "No")
    
})


# pop up additional fields if selected yes to the previous field
output$renewable_energy_fields_ui <- renderUI({
    req(input$renewable_yes_no_consumption_record == "Yes")
    
    tagList(
        numericInput("renewable_energy_consumption_consumption_record",
                     "Renewable Energy Consumption (kWh)*",
                     min = 0,
                     value = NA),
        selectInput("renewable_energy_type_consumption_record",
                    "Renewable Energy Type*",
                    choices = c("Select an energy type" = "",
                                renewable_energy_type))
    )
    
})



# table -------------------------------------------------------------------

# building

# initial tables
building_table_consumption_record <- reactiveVal(NULL)

# function to cache database
load_consumption_record_building <- function() {
    data <- dbGetQuery(
        pool,
        "SELECT *
              FROM consumption_record_building") |> 
        mutate(
            creation_time = 
                as_datetime(
                    creation_time, 
                    tz = tz(Sys.timezone())
                ),
            start_date = as_date(start_date),
            end_date = as_date(end_date)
        )
    
    building_table_consumption_record(data)
}

# initialise the database at the start
observe({load_consumption_record_building()})

# Add new record: Building
observeEvent(input$add_building_consumption_record, {
    
    # obtain the city from the asset record
    selected_city <- asset_table_building() |> 
        filter(
            country == input$building_country_consumption_record,
            asset_name == input$building_asset_consumption_record
        ) |> 
        dplyr::distinct(city) |> 
        pull(city)
    
    # create a new record with the submitted values
    new_record <- tibble(
        country = input$building_country_consumption_record,
        city = selected_city,
        asset_name = input$building_asset_consumption_record,
        reporting_year = input$building_year_consumption_record,
        fuel_type = input$fuel_select_building_consumption_record,
        consumption = input$building_consumption_consumption_record,
        unit = input$building_unit_consumption_record,
        is_renewable = input$renewable_yes_no_consumption_record,
        renewable_energy_consumption = 
            input$renewable_energy_consumption_consumption_record,
        renewable_energy_type = input$renewable_energy_type_consumption_record,
        start_date = input$building_date_range_consumption_record[1],
        end_date = input$building_date_range_consumption_record[2],
        additional_comment = input$building_comment_consumption_record,
        creation_time = Sys.time()
    )
    
    # case for electricity 
    if (new_record$fuel_type == "Electricity") {
        
        if (is.na(new_record$reporting_year) |
            is.na(new_record$consumption) |
            is.na(new_record$unit) |
            is.na(new_record$start_date) |
            is.na(new_record$end_date) |
            length(new_record$is_renewable) == 0) {
            showNotification("Incomplete record!",
                             type = "warning",
                             closeButton = TRUE)
            return()
        }
    } 
    else { # case for non-electricity
        
        if (is.na(new_record$reporting_year) |
            is.na(new_record$consumption) |
            is.na(new_record$unit) |
            is.na(new_record$start_date) |
            is.na(new_record$end_date)) {
            showNotification("Incomplete record!",
                             type = "warning",
                             closeButton = TRUE)
            return()
        }
        
    }
    
    
    # check for incomplete submission for renewable energy fields if fuel type
    # is electricity
    if(new_record$fuel_type == "Electricity") { # case for electricity
        if(new_record$is_renewable == "Yes") { # case for yes for Renewable?
            if(
                (is.na(new_record$renewable_energy_consumption) |
                 !nzchar(new_record$renewable_energ_type)
                )
            ) {
                showNotification(
                    "Incomplete record! (Check renewable energy fields)",
                    type = "warning",
                    closeButton = TRUE)
                return()
            }
        } 
    }
    else {} # case for non-electricity, no operations needed, proceed to 
    # the next step
    
    
    # check for duplicate record
    existing_table <- building_table_consumption_record()
    
    duplicate <- any(
        existing_table$asset_name == new_record$asset_name &
            existing_table$fuel_type == new_record$fuel_type &
            existing_table$start_date == new_record$start_date &
            existing_table$end_date == new_record$end_date
        
    )
    
    # warning message for duplicate record exists
    if (duplicate) {
        showNotification("Record already exists!", 
                         type = "warning")
        
        return()
    }
    
    # check end date strictly greater than or equal to start date
    if(new_record$end_date < new_record$start_date) {
        showNotification(
            "Start date must be smaller than or equal to end date",
            type = "warning")
        
        return()
    }
    
    # check for duration overlaps
    existing_table <- building_table_consumption_record() |> 
        filter(asset_name == new_record$asset_name,
               fuel_type == new_record$fuel_type)
    
    overlap <- any(
        
        (new_record$start_date <= existing_table$start_date &
             new_record$end_date >= existing_table$start_date),
        (new_record$start_date >= existing_table$start_date &
             new_record$end_date <= existing_table$end_date),
        (new_record$start_date <= existing_table$end_date &
             new_record$end_date >= existing_table$end_date)
        
    )
    
    
    if (overlap) {
        showNotification("Overlapping duration!", 
                         type = "warning",
                         closeButton = TRUE)
        
        return()
    }
    
    # convert POSIXct and Date variable as numeric
    new_record <- new_record |>
        mutate(across(  # 对多列同时进行修改
            # 选择所有日期时间列
            .cols = where(~ inherits(., "POSIXct") | inherits(., "Date")), 
            .fns = as.numeric  # 把这些列转换成数字
        ))
    
    # update the reactive value with new record
    dbWriteTable(pool,
                 "consumption_record_building",
                 new_record, append = TRUE)
    
    # refresh the table
    load_consumption_record_building()
    
    showNotification("New building consumption record added", 
                     type = "message",
                     closeButton = TRUE)
    
    # clear the inputs field
    updateNumericInput(session,
                       "building_consumption_consumption_record",
                       value = NA)
    
    updateRadioButtons(session,
                       "renewable_yes_no_consumption_record",
                       selected = NA)
    
    updateNumericInput(session,
                       "renewable_energy_consumption_consumption_record",
                       value = NA)
    
    updateSelectInput(session,
                      "renewable_energy_type_consumption_record",
                      selected = "")
    
    updateDateRangeInput(session,
                         "building_date_range_consumption_record",
                         start = Sys.Date(),
                         end = Sys.Date())
    
    updateTextInput(session,
                    "building_comment_consumption_record",
                    value = "")
})
