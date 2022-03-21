ui = fluidPage(
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  # Sidebar to show user info after login
  uiOutput("main_ui"),
)