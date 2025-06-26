nav_panel(
    title = "Consumption",
    
    page_sidebar(
        sidebar = sidebar(
            title = "Inputs",
            page_navbar(
                # building inputs
                nav_panel(
                    title = "Building",
                    icon = icon("building"),
                    selectInput(
                        "building_country_consumption_record",
                        "Select a country",
                        choices = ""
                    ),
                    selectInput(
                        "building_asset_consumption_record",
                        "Select asset*",
                        choices = ""),
                    selectInput(
                        "building_year_consumption_record",
                        "Select a Reporting Year*",
                        choices = c("Select a year" = "",
                                    reporting_year)),
                    selectInput(
                        "fuel_select_building_consumption_record",
                        "Select fuel type*",
                        choices = c("Select a fuel type" = "")),
                    numericInput(
                        "building_consumption_consumption_record",
                        "Energy consumption*",
                        value = NA,
                        min = 0),
                    selectInput(
                        "building_unit_consumption_record",
                        "Select a unit",
                        choices = c("Select a unit" = "",
                                    building_fuel_unit)),
                    dateRangeInput(
                        "building_date_range_consumption_record", 
                        "Date Range* (yyyy-mm-dd)"),
                    textInput(
                        "building_comment_consumption_record",
                        "Additional Comment",
                        value = ""),
                    uiOutput("renewable_energy_ui"),
                    uiOutput("renewable_energy_fields_ui"),
                    actionButton(
                        "add_building_consumption_record",
                        "Add record")
                )
            )
        ),
        
        card(
            page_navbar(
                # building table
                nav_panel(
                    title = "Building",
                    icon = icon("building"),
                    DTOutput("building_table_consumption_record")
                )
            )
        )
    )
    
)