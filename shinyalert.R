observeEvent(input$save_campaign_builder, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})

observeEvent(input$save_reinvest, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})

observeEvent(input$save_ltv, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})

observeEvent(input$save_platform, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})

observeEvent(input$save_retention, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})

observeEvent(input$save_input, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})

observeEvent(input$save_organic, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})


output$ltv_input_validation<-renderText({
  val<-hot_to_r(input$ltv_input)
  validate(
    need(all(val$platform %in% platform_platform),"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
    need(all(val$network %in% network_organic),"Network values must be included in Initial Builder table or be Organic"),
    need(min(val$a)>0,"A must be greater than 0"),
    need(max(val$b)<0,"B must be smaller than 0"),
    need(min(val$original_value)>=0,"Original value must not be smaller than 0")
  )
  ""
})

output$platform_validation<-renderText({
  val<-hot_to_r(input$platform)
  validate(
    need(all(val$platform %in% platform_platform),"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
    need(min(val$revenue_share)>=0,"Revenue share must not be smaller than 0%"),
    need(max(val$revenue_share)<=1,"Revenue share must not be greater than 100%"),
    need(min(val$ad_per_dau)>=0,"Ad revenue per DAU must not be smaller than 0")
  )
  ""
})

output$initial_builder_validation<-renderText({
  val<-hot_to_r(input$initial_builder)
  start_date=as.Date(input$startdate)
  end_date=as.Date(input$enddate)
  validate(
    need(all(val$platform %in% platform_platform),"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
    need(min(val$start_date)>=start_date,"Start date is beyond project time frame"),
    need(max(val$start_date)<=end_date,"Start date is beyond project time frame"),
    need(min(val$end_date)>=start_date,"End date is beyond project time frame"),
    need(max(val$end_date)<=end_date,"End date is beyond project time frame"),
    need(min(val$total_spend)>=0,"Total spend must not be smaller than 0"),
    need(min(val$cpi)>=0,"CPI must not be smaller than 0")
  )
  ""
})

output$reinvest_validation<-renderText({
  val<-hot_to_r(input$reinvest)
  start_date=as.Date(input$startdate)
  end_date=as.Date(input$enddate)
  validate(
    need(all(val$platform %in% platform_platform),"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
    need(all(val$network %in% network_organic),"Network values must be included in Initial Builder table or be Organic"),
    need(min(val$start_date)>=start_date,"Start date is beyond project time frame"),
    need(max(val$start_date)<=end_date,"Start date is beyond project time frame"),
    need(min(val$end_date)>=start_date,"End date is beyond project time frame"),
    need(max(val$end_date)<=end_date,"End date is beyond project time frame"),
    need(min(val$reinvest_percentage)>=0,"Reinvest percentage must not be smaller than 0%"),
    need(max(val$reinvest_percentage)<=1,"Reinvest percentage must not be greater than 100%"),
    need(min(val$cpi)>=0,"CPI must not be smaller than 0"),
    need(min(val$network_ratio)>=0,"Network ratio must not be smaller than 0%"),
    need(max(val$network_ratio)<=1,"Network ratio must not be greater than 100%")
  )
  ""
})

output$retention_try_validation<-renderText({
  val<-hot_to_r(input$retention_try)
  validate(
    need(all(val$platform %in% platform_platform),"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
    need(all(val$network %in% network_organic),"Network values must be included in Initial Builder table or be Organic"),
    need(min(val$a)>0,"A must be greater than 0"),
    need(max(val$b)<0,"B must be smaller than 0"),
    need(val$original_value==1,"Retention original value must be 1")
  )
  ""
})

output$organic_validation<-renderText({
  val<-hot_to_r(input$organic)
  start_date=as.Date(input$startdate)
  end_date=as.Date(input$enddate)
  validate(
    need(all(val$platform %in% platform_platform),"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
    need(all(val$network=="Organic"),"This is only for Organic network"),
    need(min(val$start_date)>=start_date,"Start date is beyond project time frame"),
    need(max(val$start_date)<=end_date,"Start date is beyond project time frame"),
    need(min(val$end_date)>=start_date,"End date is beyond project time frame"),
    need(max(val$end_date)<=end_date,"End date is beyond project time frame"),
    need(min(val$daily_users)>=0,"Daily users must not be smaller than 0")
  )
  ""
})

output$input_variables_validation<-renderText({
  val<-hot_to_r(input$input_variables)
  validate(
    need(all(val$platform %in% platform_platform),"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
    need(all(val$network=="Organic"),"This is only for Organic network"),
    need(min(val$original_install_decay)>=0,"Original install must not be smaller than 0"),
    need(min(val$original_install_floor)>=0,"Original install floor must not be smaller than 0"),
    need(min(val$original_install)>=0,"Original install must not be smaller than 0")
  )
  ""
})

