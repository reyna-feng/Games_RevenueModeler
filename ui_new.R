ui = tagList(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    ")),
    navbarPage(
      theme = shinythemes::shinytheme("cerulean"),
      "DICE Revenue Modeler",
      tabPanel("Input Variables",
               sidebarPanel(
                 h5("Project date input"),
                 dateInput("startdate", "Please select calculation Start Date:", value="2022-01-01"),
                 dateInput("enddate", "Please select calculation End Date:", value="2023-12-31"),
                 br(),
                 h5("Select retention data upload method:"),
                 radioButtons("retention_type","Select Retention data upload method:",
                             choices = list("Upload a csv file","Generate by input variables"),
                             selected = "Upload a csv file"),
                 width = 3,
                 style = "font-size:75%"
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Campaign Builder",
                            helpText('This is campaign and ad network builder. Update value on the table untill non cells are red and click Save. After updating all input values, restart the tool to refresh result.'),
                            helpText('Right click can add/delete a row.'),
                            helpText('Only support Android/iOS. Please contact developer if need other platforms.'),
                            h3("Network Initial Builder"),
                            rHandsontableOutput("initial_builder"),
                            helpText("Please save Initial Builder table first then refresh the page to update data validation for all tables below."),
                            textOutput("initial_builder_validation"),
                            br(),
                            actionButton("save_campaign_builder", "Save Initial Builder",icon("save")),
                            br(),
                            br(),
                            br(),
                            h3("Network Reinvest Builder"),
                            rHandsontableOutput("reinvest"),
                            textOutput("reinvest_validation"),
                            br(),
                            actionButton("save_reinvest", "Save Reinvest",icon("save"))
                   ),
                   tabPanel("Platform Variables",
                            helpText('This is platform input variables table. Update value on the table and click Save, after updating all input values, restart the tool to refresh result.'),
                            h3("Generate LTV"),
                            helpText("Use Graph Generator page to try input variables."),
                            rHandsontableOutput("ltv_input"),
                            textOutput("ltv_input_validation"),
                            br(),
                            actionButton("save_ltv", "Save LTV",icon("save")),
                            br(),
                            br(),
                            br(),
                            h3("Platform Variables"),
                            rHandsontableOutput("platform"),
                            textOutput("platform_validation"),
                            br(),
                            actionButton("save_platform", "Save Platform",icon("save")),
                            br(),
                            br(),
                            br(),
                            h3("Generate Retention"),
                            conditionalPanel(condition="input.retention_type=='Upload a csv file'",
                                             fileInput("file1", "Retention data csv file upload:",accept = ".csv"),
                                             h5("Sample data from file uploaded"),
                                             tableOutput("retention_data_file")),
                            conditionalPanel(condition="input.retention_type=='Generate by input variables'",
                                             helpText("Use Graph Generator page to try input variables"),
                                             rHandsontableOutput("retention_try"),
                                             textOutput("retention_try_validation"),
                                             br(),
                                             actionButton("save_retention", "Save Retention",icon("save")))
                   ),
                   tabPanel("Organic Variables", 
                            helpText('This is organic network variables table. Update value on the table and click Save, after updating all input values, restart the tool to refresh result.'),
                            h3("Organic Network Builder"),
                            rHandsontableOutput("organic"),
                            textOutput("organic_validation"),
                            br(),
                            actionButton("save_organic", "Save Organic Builder",icon("save")),
                            br(),
                            br(),
                            br(),
                            h3("Organic Install Decaying Variables"),
                            rHandsontableOutput("input_variables"),
                            textOutput("input_variables_validation"),
                            br(),
                            actionButton("save_input", "Save Organic Install Variables",icon("save"))
                   ),
                   tabPanel("Graph Generator", 
                            helpText('Input value a/b to generate power function curve.'),
                            selectInput("graph_generator_type","Select which variable you want to plot",
                                        choices = list("LTV Plot","Retention Plot"),
                                        selected = "LTV Plot"),
                            numericInput("a","Input a: ",value=1),
                            numericInput("b","Input b: ",value=1),
                            numericInput("original_value","Input original value: ",value=1),
                            plotOutput("graph_generator")
                            )
                 ),
                 width = 9,
                 style = "font-size:80%"
               )
      ),
      tabPanel("Revenue Result",
               sidebarPanel(
                 helpText("Select a platform to display iOS/Android result"),
                 radioButtons("platform_select", 
                              h5("Choose a platform"), 
                              choices = list("All", 
                                             "iOS", 
                                             "Android"),
                              selected = "All"),
                 selectInput("dataset", "Choose a dataset to download:",
                             choices = c("Total Revenue", "iOS Revenue","Android Revenue")),
                 
                 downloadButton("downloadRevenue", "Download"),
                 
                 width = 3,
                 style = "font-size:75%"
               ),
               mainPanel(
                 h3("Revenue Result"),
                 tabsetPanel(type = "tabs",
                             tabPanel("Revenue by Month",tableOutput("month_view")%>% withSpinner(type=4,color="#0dc5c1",size=1)),
                             tabPanel("Revenue by Date",reactableOutput("date_view")%>% withSpinner(type=4,color="#0dc5c1",size=1)),
                             tabPanel("LTV plot", plotOutput("ltv_plot")%>% withSpinner(type=4,color="#0dc5c1",size=1)),
                             tabPanel("Retention plot", plotOutput("retention_plot")%>% withSpinner(type=4,color="#0dc5c1",size=1))
                 ),
                 width = 9,
                 style = "font-size:80%"
               )
      ),
      tabPanel("Data",
               fluidPage(
                 helpText("This is the final calculation data"),
                 downloadButton("downloadData", "Download"),
                 width = 12, tableOutput("calculation_2_table")%>% withSpinner(type=4,color="#0dc5c1",size=1)
               )
      )
    )
  )