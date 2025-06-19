nav_panel(
    title = "Emission",
    
    page_sidebar(
        sidebar = sidebar(
            title = "Inputs",
            
            # building
            selectInput(
                "id_emission_record_building",
                "Select an Id",
                choices = 
                    c("Select an Id" = "")
            ),
            
            actionButton(
                "add_emission_record_building",
                "Calculate"
            ),
            
            actionButton(
                "calculate_all_emission_record",
                "Calculate All Consumption Reocrds"
            )
        ),
        
        card(
            heading = "Selected Asset",
            textOutput("selected_building_asset_emission_record"),
            max_height = "50px",
        ),
        
        card(
            page_navbar(
                # building
                nav_panel(
                    title = "Building",
                    icon = icon("building"),
                    DTOutput("emission_record_building")
                )
            )
        )
    )
)