#Update and save platform
#The first table
output$initial_builder<-renderRHandsontable({
  rhandsontable(initial_builder,rowHeaders = NULL) %>%
    hot_col('platform')%>%
    hot_validate_character(col='platform',choices=platform_platform,allowInvalid = TRUE)%>%
    hot_col('campaign')%>%
    hot_col('network')%>%
    hot_col('start_date',type = "date",dateFormat = "MM/DD/YYYY")%>%
    hot_col('end_date',type = "date",dateFormat = "MM/DD/YYYY")%>%
    hot_col('total_spend',format="$0,0")%>%
    hot_validate_numeric(col = 'total_spend', min = 0,allowInvalid = TRUE)%>%
    hot_col('cpi',format="$0,0.00")%>%
    hot_validate_numeric(col = 'cpi', min = 0,allowInvalid = TRUE)
})

#The second table
output$reinvest<-renderRHandsontable({
  rhandsontable(reinvest,rowHeaders = NULL) %>%
    hot_col('platform')%>%
    hot_validate_character(col='platform',choices=platform_platform,allowInvalid = TRUE)%>%
    hot_col('campaign')%>%
    hot_col('network')%>%
    hot_validate_character(col='network',choices=platform_network,allowInvalid = TRUE)%>%
    hot_col('start_date',type = "date",dateFormat = "MM/DD/YYYY")%>%
    hot_col('end_date',type = "date",dateFormat = "MM/DD/YYYY")%>%
    hot_col('reinvest_percentage',format="0%")%>%
    hot_validate_numeric(col = 'reinvest_percentage', min = 0,max=1,allowInvalid = TRUE)%>%
    hot_col('cpi',format="$0,0.00")%>%
    hot_validate_numeric(col = 'cpi', min = 0,allowInvalid = TRUE)%>%
    hot_col('network_ratio',format="0%")%>%
    hot_validate_numeric(col = 'network_ratio', min = 0,max=1,allowInvalid = TRUE)%>%
    hot_col('additional_reinvest_monthly',format="$0,0")%>%
    hot_validate_numeric(col = 'additional_reinvest_monthly', min = 0,allowInvalid = TRUE)
})
#The third table
output$ltv_input<-renderRHandsontable({
  rhandsontable(ltv_input,rowHeaders = NULL) %>%
    hot_col("platform", type = "dropdown", source = platform_platform,allowInvalid = TRUE)%>%
    hot_col('network', type = "dropdown", source = network_organic,allowInvalid = TRUE)%>%
    hot_col('a',format="0.0000")%>%
    hot_validate_numeric(col = 'a', min = 0,allowInvalid = TRUE)%>%
    hot_col('b',format="0.0000")%>%
    hot_validate_numeric(col = 'b', max = 0,allowInvalid = TRUE)%>%
    hot_col('original_value',format="0.00")%>%
    hot_validate_numeric(col = 'original_value', min = 0,allowInvalid = TRUE)
})

#The forth table
output$platform<-renderRHandsontable({
  rhandsontable(platform,rowHeaders = NULL) %>%
    hot_col("platform")%>%
    hot_validate_character(col='platform',choices=platform_platform,allowInvalid = TRUE)%>%
    hot_col("revenue_share",format="0%")%>%
    hot_validate_numeric(col = "revenue_share", min = 0, max = 1,allowInvalid = TRUE)%>%
    hot_col("ad_per_dau",format="$0,0.00")%>%
    hot_validate_numeric(col = "ad_per_dau", min = 0,allowInvalid = TRUE)
})
#The fifth table
output$retention_try<-renderRHandsontable({
  rhandsontable(retention_try,rowHeaders = NULL) %>%
    hot_col("platform", type = "dropdown", source = platform_platform,allowInvalid = TRUE)%>%
    hot_col('network', type = "dropdown", source = network_organic,allowInvalid = TRUE)%>%
    hot_col('a',format="0.0000")%>%
    hot_validate_numeric(col = "a", min = 0,allowInvalid = TRUE)%>%
    hot_col('b',format="0.0000")%>%
    hot_validate_numeric(col = "b", max = 0,allowInvalid = TRUE)%>%
    hot_col('original_value',format="0.00")%>%
    hot_validate_numeric(col = "original_value",min=1, max = 1,allowInvalid = TRUE)
})
#The sixth table
output$organic<-renderRHandsontable({
  rhandsontable(organic,rowHeaders = NULL) %>%
    hot_col("platform")%>%
    hot_validate_character(col='platform',choices=platform_platform,allowInvalid = TRUE)%>%
    hot_col('network')%>%
    hot_validate_character(col='network',choices="Organic",allowInvalid = TRUE)%>%
    hot_col('daily_users',format="0,0")%>%
    hot_validate_numeric(col = "daily_users", min = 0,allowInvalid = TRUE)%>%
    hot_col('start_date',type = "date",dateFormat = "MM/DD/YYYY")%>%
    hot_col('end_date',type = "date",dateFormat = "MM/DD/YYYY")
})
#The seventh table
output$input_variables<-renderRHandsontable({
  rhandsontable(input_variables,rowHeaders = NULL) %>%
    hot_col("platform")%>%
    hot_validate_character(col='platform',choices=platform_platform,allowInvalid = TRUE)%>%
    hot_col('network')%>%
    hot_validate_character(col='network',choices="Organic",allowInvalid = TRUE)%>%
    hot_col('original_install_decay',format="0.000")%>%
    hot_validate_numeric(col = "original_install_decay", min = 0,allowInvalid = TRUE)%>%
    hot_col('original_install_floor',format="0,0")%>%
    hot_validate_numeric(col = "original_install_floor", min = 0,allowInvalid = TRUE)%>%
    hot_col('original_install',format="0,0")%>%
    hot_validate_numeric(col = "original_install", min = 0,allowInvalid = TRUE)
})

##Save Button
observeEvent(input$save_platform, {
  x1<-hot_to_r(isolate(input$platform))
  write_xlsx(x1,'./Platform.xlsx',col_names = TRUE)
})
observeEvent(input$save_input, {
  x2<-hot_to_r(isolate(input$input_variables))
  write_xlsx(x2,'./Input Variables.xlsx',col_names = TRUE)
})

observeEvent(input$save_organic, {
  x3<-hot_to_r(isolate(input$organic))
  write_xlsx(x3,'./Organic.xlsx',col_names = TRUE)
})

observeEvent(input$save_ltv, {
  x4<-hot_to_r(isolate(input$ltv_input))
  write_xlsx(x4,'./LTV Variables.xlsx',col_names = TRUE)
})

observeEvent(input$save_reinvest, {
  x5<-hot_to_r(isolate(input$reinvest))
  write_xlsx(x5,'./Reinvest.xlsx',col_names = TRUE)
})

observeEvent(input$save_campaign_builder, {
  x6<-hot_to_r(isolate(input$initial_builder))
  write_xlsx(x6,'./Initial Builder.xlsx',col_names = TRUE)
})

observeEvent(input$save_retention, {
  x7<-hot_to_r(isolate(input$retention_try))
  write_xlsx(x7,'./Retention Variables.xlsx',col_names = TRUE)
})
