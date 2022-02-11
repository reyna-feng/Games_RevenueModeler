server<-function(input,output,session){
  source('./upload_data.R',local=TRUE)
  source('./rhandsontable.R',local=TRUE)
  source('./shinyalert.R',local=TRUE)
  source('./graph.R',local=TRUE)
  
  source('./merge.R',local=TRUE)
  source('./calculation.R',local=TRUE)
  source('./final_output.R',local=TRUE)
  source('./server_inside.R',local=TRUE)   
}