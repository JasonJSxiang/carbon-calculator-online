
# Combine emission and consumption tables ---------------------------------


merged_table <- reactive({
    
    emission_record_building() |> 
        left_join(
            building_table_consumption_record(),
            by = c("consumption_record_id" = "id"),
            suffix = c("_emi", "_cons")
        )
})

# Update input fields -----------------------------------------------------

observeEvent(emission_record_building(), {
    # get the unique list of countries in the merged_table
    country_list <- merged_table() |>
        distinct(country) |>
        filter(country != "NA") |>
        pull(country)
    
    updateSelectInput(
        session,
        "country_inventory",
        choices = c("Select a country" = "",
                    country_list)
    )
})

observeEvent(input$country_inventory, {
    
    # get the unique list of reporting year in the merged table
    year_list <- merged_table() |>
        filter(country == input$country_inventory) |>
        distinct(reporting_year) |>
        filter(reporting_year != "NA") |>
        pull(reporting_year)
    
    updateSelectInput(
        session,
        "year_inventory",
        choices = c("Select a reporting year" = "",
                    year_list)
    )
})

observeEvent(input$year_inventory, {
    
    # get the unique list of asset in the managed table
    asset_list <- merged_table() |>
        filter(country == input$country_inventory,
               reporting_year == input$year_inventory) |>
        distinct(asset_name_emi) |>
        filter(asset_name_emi != "NA") |>
        pull(asset_name_emi)
    
    updateSelectInput(
        session,
        "asset_inventory",
        choices = c("Select an asset" = "",
                    asset_list)
    )
})

# plots ------------------------------------------------------------------

monthly_inventory <- reactive({
    req(nzchar(input$country_inventory) & 
            nzchar(input$year_inventory) &
            nzchar(input$asset_inventory))
    
    df <- merged_table() |> 
        filter(country == input$country_inventory,
               reporting_year == input$year_inventory,
               asset_name_emi == input$asset_inventory) |> 
        mutate(Month = factor(month(start_date_emi))) |> 
        group_by(country, Month) |> 
        summarise(LB_emission = sum(LB_emission, na.rm = TRUE))
    
    plot <- ggplot(df, aes(x = Month, y = LB_emission)) +
        geom_col(aes(fill = -LB_emission), width = 0.5) + 
        geom_line(aes(group = 1), color = "darkred", linewidth = 0.5) +  # Added group = 1 for single line
        theme_minimal() +
        theme(legend.position = "none") +
        labs(x = "Month", y = "Location-Based Emission (kgCO2e)") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Add space for labels
    
    ggplotly(plot)
})


# map ---------------------------------------------------------------------

map_inventory <- reactive({
    
    if(
        # country, reporting year, and asset are selected
        nzchar(input$country_inventory) &
        nzchar(input$year_inventory) &
        nzchar(input$asset_inventory)
    ) {
        # create the df for mapping
        df <- merged_table() |> 
            filter(country == input$country_inventory,
                   reporting_year == input$year_inventory,
                   asset_name_emi == input$asset_inventory) |> 
            group_by(country, city) |> 
            summarise(LB_emission = sum(LB_emission, na.rm = TRUE))
        
        # calculate the total annual emission to be used as label in the map
        # (at city level)
        total_emission <- paste0(df$city, ": ", 
                                 round(sum(df$LB_emission)), 
                                 "kgCO2e")
        
        city_name <- df |> 
            distinct(city) |> 
            pull(city)
        
        leaflet(world.cities |> 
                    dplyr::filter(
                        country.etc == input$country_inventory,
                        name %in% city_name
                    )
        ) |> 
            addTiles() |> 
            addScaleBar() |> 
            addSearchOSM() |> 
            addMarkers(lat = ~lat, 
                       lng  = ~long,
                       label = total_emission)
        
    } else if( 
        # only country and reporting year are selected, the map will
        # display at country level but also show markers of the included cities
        nzchar(input$country_inventory) &
        nzchar(input$year_inventory)
    ) {
        
        # create the df for mapping
        df <- merged_table() |> 
            filter(country == input$country_inventory,
                   reporting_year == input$year_inventory) |> 
            group_by(country, city) |> 
            summarise(LB_emission = sum(LB_emission, na.rm = TRUE))
        
        city_name <- df |> 
            distinct(city) |> 
            pull(city)
        
        # df for markers
        df_markers <- world.cities |> 
            filter(
                country.etc == input$country_inventory,
                name %in% city_name
            ) |> 
            left_join(df, by = c("name" = "city")) |> 
            mutate(label = paste0(name, ": ",
                                  round(LB_emission),
                                  "kgCO2e"))
        
        # draw the graph
        leaflet(world.cities |> 
                    dplyr::filter(
                        country.etc == input$country_inventory,
                        name %in% city_name
                    )
        ) |> 
            addTiles() |> 
            addScaleBar() |> 
            addSearchOSM() |> 
            addMarkers(
                data = df_markers,
                lat = ~lat, 
                lng  = ~long,
                label = ~label)
        
    } else 
        if(nzchar(input$country_inventory)) {
            # only country is selected
            
            # create the df for mapping
            df <- merged_table() |> 
                filter(country == input$country_inventory) |> 
                group_by(country, city) |> 
                summarise(LB_emission = sum(LB_emission, na.rm = TRUE))
            
            city_name <- df |> 
                distinct(city) |> 
                pull(city)
            
            # df for markers
            df_markers <- world.cities |> 
                filter(
                    country.etc == input$country_inventory,
                    name %in% city_name
                ) |> 
                left_join(df, by = c("name" = "city")) |> 
                mutate(label = paste0(name, ": ",
                                      round(LB_emission),
                                      "kgCO2e"))
            
            # draw the graph
            leaflet(world.cities |> 
                        dplyr::filter(
                            country.etc == input$country_inventory,
                            name %in% city_name
                        )
            ) |> 
                addTiles() |> 
                addScaleBar() |> 
                addSearchOSM() |> 
                addMarkers(
                    data = df_markers,
                    lat = ~lat, 
                    lng  = ~long,
                    label = ~label)
            
        } 
    else {
        # none is selected, show all the countries in the emission record table
        
        # create the df for mapping
        df <- merged_table() |> 
            group_by(country) |> 
            summarise(LB_emission = sum(LB_emission, na.rm = TRUE))
        
        # obtain the available country list
        available_country <- df$country
        
        # df for markers
        df_markers <- city_df |> 
            left_join(df, by = c("country" = "country")) |> 
            filter(country %in% available_country) |> 
            mutate(label = paste0(country, ": ",
                                  round(LB_emission),
                                  "kgCO2e")) |> 
            distinct(country, .keep_all = TRUE)
        
        # draw the graph
        leaflet(city_df |> 
                    dplyr::filter(country %in% available_country) |> 
                    distinct(country, .keep_all = TRUE)
        ) |> 
            addTiles() |> 
            addScaleBar() |> 
            addSearchOSM() |> 
            addMarkers(
                data = df_markers,
                lat = ~lat, 
                lng  = ~long,
                label = ~label)
    }
})






