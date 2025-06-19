# building asset
observeEvent(input$country_asset_building, {
    # create the unique city list of the chosen country
    temp_city_list <- city_df |> 
        filter(country == input$country_asset_building) |> 
        dplyr::distinct(name) |> 
        arrange(name) |> 
        pull(name)
    
    # update the select input
    updateSelectInput(
        session,
        "city_asset_building",
        choices = c(
            "Select a city" = "",
            temp_city_list
        )
    )
})

# vehicle asset
observeEvent(input$country_asset_vehicle, {
    # create the unique city list of the chosen country
    temp_city_list <- city_df |> 
        filter(country == input$country_asset_vehicle) |> 
        dplyr::distinct(name) |> 
        arrange(name) |> 
        pull(name)
    
    # update the select input
    updateSelectInput(
        session,
        "city_asset_vehicle",
        choices = c(
            "Select a city" = "",
            temp_city_list
        )
    )
})

# add new record 

# building 

# initialise an empty table
asset_table_building <- reactiveVal(NULL)

# create a function that loads database into R (through updating the
# existing NULL reactive value created above)
load_asset_building <- function() {
    # first load the existing data from the database
    data <- dbGetQuery(pool,
                       "SELECT *
                           FROM asset_building") |> 
        mutate(creation_time = as_datetime(
            creation_time, 
            tz = tz(Sys.timezone())
        )
        )
    
    # then pass loaded data to the NULL reactive function just created
    asset_table_building(data)
}

# initialise the database (by running the above function when the app starts)
observe({
    load_asset_building()
})

# record-adding workflow
observeEvent(input$add_record_building_asset, {
    # Create a new record
    new_record <- tibble(
        country = input$country_asset_building,
        city = input$city_asset_building,
        asset_type = "Building",
        asset_name = input$building_asset_name_asset,
        office_floor_area = input$office_area_asset,
        area_unit = input$area_unit_asset,
        is_subleased = input$subleased_asset,
        applicable_emission_source = paste(
            input$applicable_source_asset, collapse = ";"),
        creation_time = Sys.time()
    )
    
    
    # Check for incomplete record
    if (
        !nzchar(new_record$country) |
        !nzchar(new_record$asset_name) |
        !nzchar(new_record$applicable_emission_source)
    ) {
        showNotification("Incomplete record!", 
                         type = "warning",
                         closeButton = TRUE)
        return()
    }
    
    
    
    # Check for duplicate 
    existing_data <- asset_table_building()
    duplicate <- any(existing_data$asset_name == new_record$asset_name)
    
    if (duplicate) {
        showNotification("Building asset already exists!", 
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
    
    # update the database by appending the new record
    dbWriteTable(pool, "asset_building", new_record, append = TRUE)
    showNotification("New building record added",
                     type = "message",
                     closeButton = TRUE)
    
    # refresh the table showing in R by running the loading function again
    load_asset_building()
    
    # clear inputs
    updateSelectInput(
        session,
        "country_asset_building",
        selected = ""
    )
    
    updateSelectInput(
        session,
        "city_asset_building",
        selected = ""
    )
    
    updateTextInput(session,
                    "building_asset_name_asset",
                    value = NA)
    
    updateNumericInput(session, 
                       "office_area_asset",
                       value = numeric())
    
    updateSelectInput(session,
                      "area_unit_asset",
                      selected = "")
    
    updateCheckboxInput(session,
                        "subleased_asset",
                        value = FALSE)
    
    updateSelectInput(session,
                      "applicable_source_asset",
                      selected = "")
    
})
