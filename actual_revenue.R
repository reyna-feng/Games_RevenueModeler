library(RJDBC)
vDriver <- JDBC(driverClass = "com.vertica.jdbc.Driver",
                classPath = "./vertica-jdbc-10.0.0-0.jar")
vertica <- dbConnect(vDriver, "jdbc:vertica://23.20.153.208:5433/Verticadb","dbadmin", "Welcome$6060$")

sale_data<-reactive({
  sale_sql=paste("SELECT DATE(time_stamp) AS date, SUM(original_price) AS revenue FROM MB_Game_DB.V_SalesLineItem_FACT WHERE DATE(time_stamp)>='2022-01-18' GROUP BY 1 ORDER BY 1")
  sale_data <- dbGetQuery(vertica, sale_sql)
  sale_data <- as.data.frame(sale_data)
  sale_data$date<-as.Date(sale_data$date,format="%Y-%m-%d")
  sale_data<-sale_data %>% arrange(date)
  return(sale_data)
})

output$sale_actual_diff<-renderPlot({
  sale_data<-sale_data()
  final_data<-calculation_2() %>%
    select(date,platform,daily_ini_revenue)%>%
    distinct()%>%
    group_by(date) %>%
    mutate(total_iap_revenue=sum(daily_ini_revenue)) %>%
    select(date,total_iap_revenue) %>%
    distinct() %>% arrange(date)
  sale_data_join<-merge(x=sale_data,y=final_data,by=c("date"),all.x=TRUE) %>%
    mutate(revenue_diff=revenue-total_iap_revenue) %>%
    arrange(date)
  ggplot(data=sale_data_join)+
    geom_bar(aes(x=date,y=revenue_diff),stat="identity", fill="skyblue4",color="#006000")+
    labs(title= "Actual Revenue",x="Date",y="Revenue Diff")+
    scale_y_continuous(labels = dollar)
})
output$sale_actual_plot<-renderPlot({
  sale_data<-sale_data()
  sale_data<-sale_data %>%
    mutate(yearmonth=format(date,"%Y-%m")) %>%
    group_by(yearmonth) %>%
    mutate(monthly_revenue=sum(revenue)) %>%
    select(yearmonth,monthly_revenue) %>% distinct() %>% arrange(yearmonth)
  ggplot(data=sale_data)+
    geom_bar(aes(x=yearmonth,y=monthly_revenue),stat="identity", fill="skyblue4",color="#006000")+
    labs(title= "Actual Revenue",x="YearMonth",y="Revenue")+
    scale_y_continuous(labels = dollar)
})