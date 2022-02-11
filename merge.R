#Retention data
retentionInput<-reactive({
  switch(input$retention_type,
         "Upload a csv file"=data_file(),
         "Generate by input variables"=data_fun())
})
data_file<-reactive({
  file<-input$file1
  ext<-file_ext(file$datapath)
  req(file)
  validate(need(ext=="csv", "Please upload a csv file"))
  start_date=as.Date(input$startdate)
  end_date=as.Date(input$enddate)
  ndays<-as.numeric(end_date-start_date)
  db<-read.csv(file$datapath)
  validate(need(ncol(db)==5,"Please use the template csv file format"),
           need(colnames(db)==c("ï..row_id","days_aged","retention","platform","network"),"Please use the template csv file format"),
           need(unique(db[,4])==platform_platform,"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
           need(unique(db[,5])==network_organic,"Network values must be included in Initial Builder table or be Organic"),
           need(max(db[,2])==ndays,"Retention file days aged doesn't match project start date and end date")
           )
  return(db)
})
output$retention_data_file<-renderTable({
  file<-input$file1
  ext<-file_ext(file$datapath)
  req(file)
  validate(need(ext=="csv", "Please upload a csv file"))
  start_date=as.Date(input$startdate)
  end_date=as.Date(input$enddate)
  ndays<-as.numeric(end_date-start_date)
  db<-read.csv(file$datapath)
  validate(need(ncol(db)==5,"Please use the template csv file format"),
           need(colnames(db)==c("ï..row_id","days_aged","retention","platform","network"),"Please use the template csv file format"),
           need(unique(db[,4])==platform_platform,"Platform values can be only Android/iOS, please contact developer if need more platform parameter"),
           need(unique(db[,5])==network_organic,"Network values must be included in Initial Builder table or be Organic"),
           need(max(db[,2])==ndays,"Retention file days aged doesn't match project start date and end date")
  )
  return(head(db,n=10))
})
data_fun<-reactive({
  start_date=as.Date(input$startdate)
  end_date=as.Date(input$enddate)
  ndays<-as.numeric(end_date-start_date)
  seq<-seq(0,ndays)
  days_aged<-data.frame(seq)
  var<-read_excel('./Retention Variables.xlsx')
  var_merge<-merge(x=days_aged,y=var,all=TRUE)
  db<-var_merge%>%
    mutate(row_id=0,
           days_aged=seq,
           retention=ifelse(seq==0,original_value,a*seq^(b)))%>%
    select(row_id,days_aged,retention,platform,network)%>%
    distinct()
  return(db)
})
db<-reactive(retentionInput())
#Merge and Clean
merge_table<-reactive({
  join_1<-merge(x=db(),y=ltv_input,by=c("network","platform"),all.x=TRUE)
  join_2<-merge(x=join_1,y=input_variables,by=c("network","platform"),all.x=TRUE)
  join_3<-join_2 %>%
    group_by(platform,network)%>%
    arrange(days_aged,.by_group=TRUE)%>%
    mutate(ltv=ifelse(days_aged==0,original_value,a*days_aged^b))
  #Input Date
  start_date=as.Date(input$startdate)
  end_date=as.Date(input$enddate)
  ndays<-as.numeric(end_date-start_date)
  seqdays<-seq(0,ndays)
  date=seq(start_date,end_date,by='day')
  date_df<-data.frame(date)
  date_df<-cbind(date_df,seqdays)
  
  #Merge 2
  join_5<-sqldf(
    '
select a.seqdays,a.date,b.network,platform,campaign,start_date,end_date,total_spend,cpi,
       total_spend/(end_date-start_date+1)/cpi as daily_users,0.0 as reinvest_percentage,0.0 as network_ratio
from date_df as a
join initial_builder as b on a.date>=cast(b.start_date as date) and a.date<=cast(b.end_date as date)

union

select a.seqdays,a.date,b.network,platform,"Organic campaign" as campaign,start_date,end_date,0.0 as total_spend,0.0 as cpi,
       daily_users,0.0 as reinvest_percentage,0.0 as network_ratio
from date_df as a
join organic as b on a.date>=cast(b.start_date as date) and a.date<=cast(b.end_date as date)

union

select a.seqdays,a.date,b.network,platform,campaign,start_date,end_date,0.0 as total_spend,cpi,
       0.0 as daily_users,reinvest_percentage,network_ratio
from date_df as a
join reinvest as b on a.date>=cast(b.start_date as date) and a.date<=cast(b.end_date as date)

'
  ) %>%
    distinct()
  #Date merge days aged
  data_ltv<-join_3 %>%
    mutate(accu_ltv=ave(ltv, network, platform, FUN=cumsum))
  
  join_6<-sqldf(
    '
select a.seqdays,a.date,
       c.*,
       campaign,start_date,end_date,
       end_date-start_date+1 as days,
       case when total_spend is not null then total_spend else 0.0 end as total_spend,
       case when cpi is not null then cpi else 0.0 end as cpi,
       case when daily_users is not null then daily_users else 0 end as daily_users,
       case when reinvest_percentage is not null then reinvest_percentage*network_ratio else 0.0 end as reinvest_percentage
from date_df as a
join data_ltv as c on a.seqdays=c.days_aged
left join join_5 as b on a.date=b.date and c.network=b.network and c.platform=b.platform 
') %>%
    distinct()%>%
    mutate(daily_spend=ifelse(!is.na(days),total_spend/days,0))%>%
    group_by(platform,network)%>%
    arrange(date,.by_group=TRUE)
  return(join_6)
})