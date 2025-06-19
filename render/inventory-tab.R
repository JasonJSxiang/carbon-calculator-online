
# monthly -----------------------------------------------------------------

output$monthly_inventory <- renderPlotly({
    monthly_inventory()
})


# annual ------------------------------------------------------------------

output$annual_inventory <- renderDT({
    datatable(
        merged_table() |> 
            filter(
                if (input$country_inventory != "") 
                    country == input$country_inventory else TRUE,
                if (input$year_inventory != "") 
                    reporting_year == input$year_inventory else TRUE,
                if (input$asset_inventory != "") 
                    asset_name_emi == input$asset_inventory else TRUE
            ),
        selection = "single"
    )
})





# map ---------------------------------------------------------------------

output$map_inventory <- renderLeaflet({
    map_inventory()
})
