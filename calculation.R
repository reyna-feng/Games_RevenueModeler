#Calculation
calculation_1<-reactive({
  #Initial Install
  cal_1<-merge_table() %>%
    mutate(initial_installs=ifelse(is.na(original_install),daily_users,ifelse(original_install-original_install*original_install_decay*days_aged<original_install_floor,original_install_floor,original_install-original_install*original_install_decay*days_aged+daily_users)))%>%
    mutate(initial_revenue=0)

  #Initial Revenue
  subsets<-split(cal_1,list(cal_1$network,cal_1$platform))
  
  test<-do.call('rbind',lapply(subsets,function(x) {
    for (i in 1:nrow(x)) {
      x[i,26]=sum(x$ltv[1:i]*x$initial_installs[i:1])
    }
    print(x)
  }
  ))
  
  cal_3<-test %>%
    group_by(date,platform) %>%
    mutate(daily_ini_revenue=sum(initial_revenue))%>%
    arrange(date)
  
  #Merge platform data
  cal_4<-sqldf(
    '
select a.*,b.revenue_share,b.ad_per_dau
from cal_3 as a
join platform as b on a.platform=b.platform

'
  ) %>%
    distinct()%>%
    mutate(daily_ini_revenue=daily_ini_revenue*revenue_share)%>%
    arrange(platform,network,date)
  return(cal_4)
})

calculation_2<-reactive({
  #Revenue loop function
  cal_5<-calculation_1() %>% 
    mutate(reinvest_installs=0,project_install=initial_installs,marketing_cost=daily_spend,revenue_increase=0,temp_daily_ini_revenue=0)
  subsets_platform<-split(cal_5,list(cal_5$platform,cal_5$network))
  
  test_1<-do.call('rbind',lapply(subsets_platform,function(x) {
    for (i in 2:nrow(x))
    {
      x[i,34]=x$daily_ini_revenue[i-1]
      if (x$cpi[i]>0) {
        x[i,30]=x$temp_daily_ini_revenue[i]*x$reinvest_percentage[i]/x$cpi[i]
      }else{
        x[i,30]=0
      }
      x[i,31]=floor(x$initial_installs[i]+x$reinvest_installs[i])
      x[i,33]=sum(x$ltv[1:i]*x$project_install[i:1])-x$initial_revenue[i]
      x[i,26]=sum(x$ltv[1:i]*x$project_install[i:1])
      x[i,27]=x$daily_ini_revenue[i]+x$revenue_increase[i]*x$revenue_share[i]
    }
    print(x)
  }
  ))
  
  #Calculate DAU
  test_2<-test_1%>%
    mutate(dau=project_install)%>%
    group_by(platform,date)%>%
    mutate(daily_ini_revenue=max(daily_ini_revenue))
  test_3<-split(test_2,list(test_2$platform,test_2$network))
  test_4<-do.call('rbind',lapply(test_3,function(x) {
    for (i in 1:nrow(x))
    {
      x[i,35]=ceiling(sum(x$retention[1:i]*x$project_install[i:1]))
    }
    print(x)
  }
  ))
  test_5<-test_4 %>%
    mutate(year=year(date),month=month(date),
           marketing_cost=ifelse(is.na(marketing_cost),0,marketing_cost+reinvest_installs*cpi))%>%
    group_by(platform,date)%>%
    mutate(total_install=sum(project_install),
           total_marketing_cost=sum(marketing_cost))%>%
    mutate(total_dau=sum(dau))%>%
    mutate(total_ad_revenue=ad_per_dau*total_dau)%>%
    mutate(total_revenue=total_ad_revenue+daily_ini_revenue)%>%
    select(year,month,date,days_aged,network,platform,retention,accu_ltv,project_install,marketing_cost,total_marketing_cost,total_install,dau,total_dau,daily_ini_revenue,total_ad_revenue,total_revenue)%>%
    distinct()
  return(test_5)
})

