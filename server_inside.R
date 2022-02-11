#Start server
url<-a("Confluence Instruction", href="https://wimogames.atlassian.net/wiki/spaces/DATA/pages/5775097867/DICE+RPG")
output$instruction <- renderUI({
  helpText("Please open this like:", url)
})

dailyInput<-reactive({
  switch(input$platform_select,
         "All"=final_month_t_fun(),
         "iOS"=final_ios_t_fun(),
         "Android"=final_android_t_fun())
})
monthInput<-reactive({
  switch(input$platform_select,
         "All"=final_month_fun(),
         "iOS"=final_ios_fun(),
         "Android"=final_android_fun())
})

#month_view
output$month_view<-renderTable({monthInput()})
#date_view
output$date_view<-renderReactable({dailyInput()})
#ltv_plot
output$ltv_plot<-renderPlot(ltv())
#retention_plot
output$retention_plot<-renderPlot(retention())
#revenue_plot
output$revenue_plot<-renderPlot(revenue())
#downloadRevenue
downloadInput <- reactive({
  switch(input$dataset,
         "Total Revenue" = final_month_fun(),
         "iOS Revenue"=final_ios_fun(),
         "Android Revenue"=final_android_fun())
})

output$downloadRevenue <- downloadHandler(
  filename = function() {
    paste(input$dataset, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(downloadInput(), file, row.names = FALSE)
  }
)


#downloadData
output$downloadData <- downloadHandler(
  filename = function() {
    paste("Final Calculation Data", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(calculation_2(), file, row.names = FALSE)
  }
)
#calculation_2_table
output$calculation_2_table<-renderTable(calculation_2()) 
