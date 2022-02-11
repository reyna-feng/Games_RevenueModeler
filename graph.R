#Graph Generator
generate_ltv<-reactive({
  a<-input$a
  b<-input$b
  original_value<-input$original_value
  n<-as.numeric(as.Date(input$enddate)-as.Date(input$startdate))
  x<-as.vector(seq(from=1,to=n,by=1))
  f<-cumsum(a*x^(b))+original_value
  #h<-Vectorize(f)
  plot(x=x,y=f,xlab="Days Aged",ylab="LTV",col="blue",type="l")
  #curve(expr=h,xlab="Days Aged",ylab="LTV",xlim=c(1,n),col="blue")
})

generate_retention<-reactive({
  a<-input$a
  b<-input$b
  n<-as.numeric(as.Date(input$enddate)-as.Date(input$startdate))
  f<-function(x){
    return(a*x^(b))
  }
  curve(expr=f,xlab="Days Aged",ylab="Retention",xlim=c(1,n),col="blue")
})

graph_type<-reactive({
    switch(input$graph_generator_type,
           "LTV Plot"=generate_ltv(),
           "Retention Plot"=generate_retention())
  })

output$graph_generator<-renderPlot(graph_type())