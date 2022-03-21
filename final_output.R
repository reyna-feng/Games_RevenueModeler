#Final Output
final_date_fun<-reactive({
  final_date<-calculation_2() %>%
    mutate(year_month=paste(year,month,sep="-"))%>%
    select(year_month,date,platform,total_install,total_dau,total_ad_revenue,total_marketing_cost,total_revenue,daily_ini_revenue)%>%
    distinct()%>%
    group_by(date) %>%
    mutate(total_install=sum(total_install),
           total_iap_revenue=sum(daily_ini_revenue),
           total_ad_revenue=sum(total_ad_revenue),
           total_marketing_cost=sum(total_marketing_cost))%>%
    mutate(net_revenue=total_iap_revenue+total_ad_revenue-total_marketing_cost)%>%
    select(year_month,date,total_install,total_iap_revenue,total_ad_revenue,total_marketing_cost,net_revenue)%>%
    distinct() %>% arrange(date)
  return(final_date)
})

final_month_t_fun<-reactive({
  final_month_t<-reactable(final_date_fun(), groupBy='year_month',columns = list(
    date=colDef(name='Date'),
    total_install=colDef(aggregate='sum',name = "Total Installs",align = "center",format = colFormat(separators = TRUE, digits = 0)),
    total_iap_revenue=colDef(aggregate='sum',name = "Net IAP Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    total_ad_revenue=colDef(aggregate='sum',name = "Total Ad Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    total_marketing_cost=colDef(aggregate='sum',name = "Total Marketing Cost",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    net_revenue=colDef(aggregate='sum',name = "Net Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2))
  ),
  bordered = TRUE,
  highlight = TRUE,
  defaultPageSize = 13)
  return(final_month_t)
})

final_ios_date_fun<-reactive({
  final_ios_date<-calculation_2() %>%
    filter(platform=='iOS')%>%
    mutate(year_month=paste(year,month,sep="-"))%>%
    select(year_month,date,platform,total_install,total_dau,total_ad_revenue,total_marketing_cost,total_revenue,daily_ini_revenue)%>%
    distinct()%>%
    group_by(date) %>%
    mutate(total_install=sum(total_install),
           total_iap_revenue=sum(daily_ini_revenue),
           total_ad_revenue=sum(total_ad_revenue),
           total_marketing_cost=sum(total_marketing_cost))%>%
    mutate(net_revenue=total_iap_revenue+total_ad_revenue-total_marketing_cost)%>%
    select(year_month,date,total_install,total_iap_revenue,total_ad_revenue,total_marketing_cost,net_revenue)%>%
    distinct()
  return(final_ios_date)
})

final_ios_t_fun<-reactive({
  final_ios_t<-reactable(final_ios_date_fun(), groupBy='year_month',columns = list(
    date=colDef(name='Date'),
    total_install=colDef(aggregate='sum',name = "Total Installs",align = "center",format = colFormat(separators = TRUE, digits = 0)),
    total_iap_revenue=colDef(aggregate='sum',name = "Net IAP Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    total_ad_revenue=colDef(aggregate='sum',name = "Total Ad Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    total_marketing_cost=colDef(aggregate='sum',name = "Total Marketing Cost",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    net_revenue=colDef(aggregate='sum',name = "Net Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2))
  ),
  bordered = TRUE,
  highlight = TRUE,
  defaultPageSize = 13)
  return(final_ios_t)
})

final_android_date_fun<-reactive({
  final_android_date<-calculation_2() %>%
    filter(platform=='Android')%>%
    mutate(year_month=paste(year,month,sep="-"))%>%
    select(year_month,date,platform,total_install,total_dau,total_ad_revenue,total_marketing_cost,total_revenue,daily_ini_revenue)%>%
    distinct()%>%
    group_by(date) %>%
    mutate(total_install=sum(total_install),
           total_iap_revenue=sum(daily_ini_revenue),
           total_ad_revenue=sum(total_ad_revenue),
           total_marketing_cost=sum(total_marketing_cost))%>%
    mutate(net_revenue=total_iap_revenue+total_ad_revenue-total_marketing_cost)%>%
    select(year_month,date,total_install,total_iap_revenue,total_ad_revenue,total_marketing_cost,net_revenue)%>%
    distinct()
  return(final_android_date)
})
final_android_t_fun<-reactive({
  final_android_t<-reactable(final_android_date_fun(), groupBy='year_month',columns = list(
    date=colDef(name='Date'),
    total_install=colDef(aggregate='sum',name = "Total Installs",align = "center",format = colFormat(separators = TRUE, digits = 0)),
    total_iap_revenue=colDef(aggregate='sum',name = "Net IAP Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    total_ad_revenue=colDef(aggregate='sum',name = "Total Ad Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    total_marketing_cost=colDef(aggregate='sum',name = "Total Marketing Cost",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2)),
    net_revenue=colDef(aggregate='sum',name = "Net Revenue",align = "center",format = colFormat(prefix = "$", separators = TRUE, digits = 2))
  ),
  bordered = TRUE,
  highlight = TRUE,
  defaultPageSize = 13)
  return(final_android_t)
})

#LTV plot
ltv<-reactive({
  ggplot(data=calculation_2(),aes(x=days_aged,y=accu_ltv,color=factor(interaction(platform,network))))+
    geom_line()+
    ggtitle("LTV")+
    xlab("Account days aged")+
    ylab("LTV")+
    scale_y_continuous(labels = dollar)
})
#Retention Plot
retention<-reactive({
  ggplot(data=calculation_2(),aes(x=days_aged,y=retention,color=factor(interaction(platform,network))))+
    geom_line()+
    ggtitle("Retention Rate")+
    xlab("Account days aged")+
    ylab("Retention")+
    scale_y_continuous(labels = percent)
})

#Total Revenue rows to columns

final_month_fun<-reactive({
  final_month<-final_date_fun() %>%
    group_by(year_month) %>%
    mutate(total_install=sum(total_install),
           total_iap_revenue=sum(total_iap_revenue),
           total_ad_revenue=sum(total_ad_revenue),
           total_marketing_cost=sum(total_marketing_cost),
           net_revenue=sum(net_revenue))%>%
    select(year_month,total_install,total_iap_revenue,total_ad_revenue,total_marketing_cost,net_revenue)%>%
    distinct()
  final_month$total_install<-format(as.numeric(final_month$total_install),digit=0,big.mark=",",scientific = FALSE)
  final_month$total_iap_revenue<-dollar(final_month$total_iap_revenue)
  final_month$total_ad_revenue<-dollar(final_month$total_ad_revenue)
  final_month$total_marketing_cost<-dollar(final_month$total_marketing_cost)
  final_month$net_revenue<-dollar(final_month$net_revenue)
  final_month<-as.data.frame(t(as.data.frame(final_month)))
  names<-list('Year - Month','Total Installs','Net IAP Revenue','Total Ad Revenue','Total Marketing Cost','Net Revenue')
  rownames(final_month)<-names
  final_month<-rownames_to_column(final_month,"Metric Names")
  return(final_month)
})

#Revenue Plot
revenue<-reactive({
  final_month<-final_date_fun() %>%
    group_by(year_month) %>%
    mutate(total_install=sum(total_install),
           total_iap_revenue=sum(total_iap_revenue),
           total_ad_revenue=sum(total_ad_revenue),
           total_marketing_cost=sum(total_marketing_cost),
           net_revenue=sum(net_revenue))%>%
    mutate(running_net_revenue=cumsum(net_revenue)) %>%
    select(year_month,total_install,total_iap_revenue,total_ad_revenue,total_marketing_cost,net_revenue,running_net_revenue)%>%
    distinct()%>%
    arrange(year_month)
  ggplot(data=final_month)+
    geom_bar(aes(x=year_month,y=net_revenue),stat="identity", fill="skyblue4",color="#006000")+
    geom_line(aes(x=year_month, y=running_net_revenue),color="tomato4")+
    labs(title= "Predicted Revenue",x="YearMonth",y="Revenue")+
    scale_y_continuous(labels = dollar, sec.axis=sec_axis(trans=~.*10,name="Running Net Revenue",label=dollar))
})

final_ios_fun<-reactive({
  final_ios<-final_ios_date_fun()%>%
    group_by(year_month) %>%
    mutate(total_install=sum(total_install),
           total_iap_revenue=sum(total_iap_revenue),
           total_ad_revenue=sum(total_ad_revenue),
           total_marketing_cost=sum(total_marketing_cost),
           net_revenue=sum(net_revenue))%>%
    select(year_month,total_install,total_iap_revenue,total_ad_revenue,total_marketing_cost,net_revenue)%>%
    distinct()
  final_ios$total_install<-format(as.numeric(final_ios$total_install),digit=0,big.mark=",",scientific = FALSE)
  final_ios$total_iap_revenue<-dollar(final_ios$total_iap_revenue)
  final_ios$total_ad_revenue<-dollar(final_ios$total_ad_revenue)
  final_ios$total_marketing_cost<-dollar(final_ios$total_marketing_cost)
  final_ios$net_revenue<-dollar(final_ios$net_revenue)
  final_ios<-as.data.frame(t(as.data.frame(final_ios)))
  names<-list('Year - Month','Total Installs','Net IAP Revenue','Total Ad Revenue','Total Marketing Cost','Net Revenue')
  rownames(final_ios)<-names
  final_ios<-rownames_to_column(final_ios,"Metric Names")
  return(final_ios)
})

final_android_fun<-reactive({
  final_android<-final_android_date_fun()%>%
    group_by(year_month) %>%
    mutate(total_install=sum(total_install),
           total_iap_revenue=sum(total_iap_revenue),
           total_ad_revenue=sum(total_ad_revenue),
           total_marketing_cost=sum(total_marketing_cost),
           net_revenue=sum(net_revenue))%>%
    select(year_month,total_install,total_iap_revenue,total_ad_revenue,total_marketing_cost,net_revenue)%>%
    distinct()
  final_android$total_install<-format(as.numeric(final_android$total_install),digit=0,big.mark=",",scientific = FALSE)
  final_android$total_iap_revenue<-dollar(final_android$total_iap_revenue)
  final_android$total_ad_revenue<-dollar(final_android$total_ad_revenue)
  final_android$total_marketing_cost<-dollar(final_android$total_marketing_cost)
  final_android$net_revenue<-dollar(final_android$net_revenue)
  final_android<-as.data.frame(t(as.data.frame(final_android)))
  names<-list('Year - Month','Total Installs','Net IAP Revenue','Total Ad Revenue','Total Marketing Cost','Net Revenue')
  rownames(final_android)<-names
  final_android<-rownames_to_column(final_android,"Metric Names")
  return(final_android)
})

