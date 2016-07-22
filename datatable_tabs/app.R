#########################################################################
### Title:  Example of single tab that outputs data in configruation events 
###          table and makes it available for download
###
### Author: Hauke Licht
### Data:   June 22, 2016
### produced under R version 3.2.3

## Only for first run in R-session, source script that gets data from server 
# source("../databaseAccessHauke.R")

# define working directory
path <- setwd("~/Documents/Humboldt/Electoral_Vulnerability/Projects/vaps-dashboard_public/datatable_tabs")
if ( sub(".*/","",getwd()) != "datatable_tabs" ) setwd(path) ## set path to vaps-dashboard_public here ##
rm(path)

if (!require(shiny)) install.packages("shiny")
if (!require(DT)) install.packages("DT")

# Prerequisites: 
  
  # (a) join country information on configuration events table by ctr_id
  configuration_events <- merge(country[,1:3],mv_configuration_events,by="ctr_id",all.y=T)
    # order table by ctr_id and sdate
    configuration_events <- configuration_events[order(configuration_events$ctr_id, configuration_events$sdate), ]
    rownames(configuration_events) <- NULL
  
  # (b) define list of variables in dataframe, optional choices for column selector (checkboxGroupInput) in user interface
  var_choices <- append(colLabsList$country [colLabsList$country %in% colnames(country)[1:3] ], 
                        colLabsList$mv_configuration_events[ -grep("ctr_id",colLabsList$mv_configuration_events) ] ) 
  
  # (c) define list of countries in dataframe, optional choices for country selector (selectInput) in user interface
  countries_in_data <- countrySelectorList  
  
    # NOTE: lists colLabsList and countrySelectorList are defined in databaseAccessHauke.R 
  
  # (d) define date range in dataframe, used to set up date selector (dateRangeInput) in user interface 
  min_sdate <- min(configuration_events$sdate)
  max_sdate <- max(configuration_events$sdate)

  
# Produce actual User Interface  
ui <- fluidPage(
#   navbarPage(  # navbarPage defintion
#     title = "Configuration Events Data",
#     tabPanel(  # defintion of 1st tabPanel
      sidebarLayout(  # sidebarLayout defintion
        sidebarPanel(  # sidebarPanel definition
              tags$h2("Configuration Events Data"),
                tags$hr(),
                tags$h4("The configuration events data table sequences changes in the political-institutional configurations of a country by start date."),
                tags$h6("A new political configuration is recorded when one of the following changes occurs:"),
                  tags$ul(
                    tags$li("a change in cabinet composition (rows in table Cabinet, identified by variable cab_id)"),
                    tags$li("a change in lower house composition (rows in rable Lower House, identified by variable lh_id)"),
                    tags$li("a change in upper house composition, if an upper house exists in country's political system (rows in table Upper House, identified by variable uh_id)"),
                    tags$li("a change in presidency, if president exists in country's political system (rows in table Presidential Election, identified by variable prselc_id)"),
                    tags$li("a change in a veto institutions constitutional entitlment of veto rights (i.e., change in veto power of one out of seven veto institutions recorded per country in table Veto Points)")
                  ), # close unordered list
                #tags$p("Please select variable Type of configuration change (type_of_change) below to report from which institution changed in composition and hence induced a change in the political configuration of this country"),
                tags$hr(),  #end tag list
            selectInput(inputId = "countries_in_data", 
                        label = "Select countries", 
                        choices = countries_in_data , selected = countries_in_data['All countries'],
                        multiple = T),
            dateRangeInput(inputId = "date_range", 
                           label = "Select time period (date format is yyyy-mm-dd)",
                           start = min_sdate, end = max_sdate,
                           min = min_sdate, max = max_sdate,
                           separator = "to"),
           checkboxGroupInput(inputId = "var_choices", 
                              label = "Columns in configurations event data:",
                              choices = var_choices, selected = var_choices[-c(2)],
                              inline=F),
           width = 4), # end of sidebarPanel definition 
         mainPanel(  # mainPanel definition
           tags$h4("Clicking on the up or down arrow in the header of the table sorts data by entries in that column."),
           tags$h5("If you want to sort by multiple columns, click on them one after another in the header row while holding shift."),
           tags$hr(),
           dataTableOutput(outputId = "configuration_events"),
           tags$hr(),
           tags$div(downloadButton("download_configuration_events", "Click to download selected data"), style="float:right"),
         width=8)  # end of mainPanel definition
      )  # end of sidebarLayout defintion 
    # )  # end of tabPanel defintion
 # )  # end of navbarPage defintion
)  # ui <- fluidPage( ... ends here

# Define Server
server <- function(input, output) {
  
  configuration_eventsInput <- reactive({
    if ("All" %in% unlist(input$countries_in_data)) {
      subset(configuration_events, 
             subset = (configuration_events$sdate %in% unlist(input$date_range)[1]:unlist(input$date_range)[2]), 
             select=unlist(input$var_choices))
    } else {
      subset(configuration_events, 
             subset = (configuration_events$sdate %in% unlist(input$date_range)[1]:unlist(input$date_range)[2] & configuration_events$ctr_ccode %in% unlist(input$countries_in_data)) , 
             select=unlist(input$var_choices))
    }
  })
    
  output$configuration_events <- renderDataTable({ configuration_eventsInput() },
    options = list(autoWidth = T,
                   orderClasses = T,
                   columns.orderData = T,
                   lengthMenu = list(c(5, 15, 30, 45, -1), c("5", "15", "30", "45", "All")), 
                   pageLength = 30, 
                   searching = FALSE,
                   scrollY = 800,
                   scrollX = T,
                   rownames = FALSE)
    # see https://datatables.net/manual/styling/classes for DT::DataTable further options, e.g. dom = 'tip' 
  )
  
  output$download_configuration_events <- downloadHandler(
    filename = function() { paste0(input$configuration_events, '.csv') },
    content = function(file) {
      write.csv(configuration_eventsInput(), file)
    }
  )
}

shinyApp (ui = ui, server = server)
