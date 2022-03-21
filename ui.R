source('./packages.R',local=TRUE)
user_base <- tibble::tibble(
  user = c("cbeers", "rfeng"),
  password = sapply(c("cbeers", "rfeng"), sodium::password_store),
  permissions = c("standard","admin"),
  name = c("Craig Beers", "Reyna Feng")
)
ui = fluidPage(
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  # Sidebar to show user info after login
  uiOutput("main_ui"),
)