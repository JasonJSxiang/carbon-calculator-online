# building table ----------------------------------------------------------
output$asset_table_building <- renderDT({
    datatable(
        asset_table_building(),
        selection = "single")
})
