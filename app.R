source('./ui.R',local=TRUE)
# Define server
server<-function(input,output,session){
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  source('./ui_new.R',local=TRUE)
    source('./upload_data.R',local=TRUE)
    source('./rhandsontable.R',local=TRUE)
    source('./shinyalert.R',local=TRUE)
    source('./graph.R',local=TRUE)
    source('./merge.R',local=TRUE)
    source('./calculation.R',local=TRUE)
    source('./final_output.R',local=TRUE)
    source('./server_inside.R',local=TRUE)
    source('./actual_revenue.R',local=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
