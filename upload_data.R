initial_builder<-read_excel('./Initial Builder.xlsx')
platform_platform<-c("Android","iOS")
platform_network<-unique(initial_builder$network)
network_organic<-unique(c(initial_builder$network,"Organic"))

initial_builder$start_date<-as.Date(initial_builder$start_date)
initial_builder$end_date<-as.Date(initial_builder$end_date)

reinvest<-read_excel('./Reinvest.xlsx')
reinvest$start_date<-as.Date(reinvest$start_date)
reinvest$end_date<-as.Date(reinvest$end_date)

input_variables<-read_excel('./Input Variables.xlsx')

ltv_input<-read_excel('./LTV Variables.xlsx')

platform<-read_excel('./Platform.xlsx')

retention_try<-read_excel('./Retention Variables.xlsx')

organic<-read_excel('./Organic.xlsx')
organic$start_date<-as.Date(organic$start_date)
organic$end_date<-as.Date(organic$end_date)

