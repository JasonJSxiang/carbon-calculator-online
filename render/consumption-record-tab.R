# building table ----------------------------------------------------------


output$building_table_consumption_record <- renderDT({
    datatable(building_table_consumption_record(),
              selection = "single")
})

