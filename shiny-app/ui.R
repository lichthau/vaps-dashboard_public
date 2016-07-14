library(leaflet)
library(plotly)

#packages_ui <- c(
#  "plotly",
#  "leaflet"
#)

#lapply(packages_ui, install.packages, character.only = TRUE)
#lapply(packages_ui, library, character.only = TRUE)

countries_in_data <- list("Australia" = "AUS",
  "Austria" = "AUT",
  "Belgium" = "BEL",
  "Canada" = "CAN",
  "Switzerland" = "CHE",
  "Czech Republic" = "CZE",
  "Germany" = "DEU",
  "Denmark" = "DNK",
  "Spain" = "ESP",
  "Estonia" = "EST",
  "Finland" = "FIN",
  "France" = "FRA",
  "United Kingdom" = "GBR",
  "Greece" = "GRC",
  "Ireland" = "IRL",
  "Iceland" = "ISL",
  "Israel" = "ISR",
  "Italy" = "ITA",
  "Japan" = "JPN",
  "Luxembourg" = "LUX",
  "Netherlands" = "NLD",
  "Norway" = "NOR",
  "New Zealand" = "NZL",
  "Poland" = "POL",
  "Portugal" = "PRT",
  "Slovenia" = "SVN",
  "Sweden" = "SWE",
  "United States" = "USA"
)

shinyUI(fluidPage(

  tags$head(
    tags$h1("VAPS-Dashboard"),
    tags$link(href="css/vaps_dashboard.css", rel="stylesheet", type="text/css")
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Please select variable(s) of interest!"),

      conditionalPanel(condition = "input.tabs != 4",
        selectInput("country", h5("Country"),
          choices = countries_in_data
        )
      ),

      conditionalPanel(condition = "input.tabs == 1",
        selectInput("variable_veto", h5("Variable"),
          choices = list("Veto Point President" = "vto_prs",
            "Veto Point Head of Government" = "vto_hog",
            "Veto Point Lower House" = "vto_lh",
            "Veto Point Upper House" = "vto_uh",
            "judicial Veto Point" = "vto_jud",
            "electoral Veto Point" = "vto_elct",
            "territorial Veto Point" = "vto_terr",
            "Sum of open Veto Points" = "vto_pts"
          )
        )
      ),
      conditionalPanel(condition = "input.tabs == 2",
        selectInput("variable_bivar1", h5("Variable x-axis"),
          choices = list("LH Disproportionality" = "lhelc_lsq",
            "effective number of parties" = "lh_enpp",
            "cabinet seat share" = "cab_lh_sts_shr",
            "seat A volatitlity" = "lhelc_vola_sts",
            "seat B volatitlity" ="lhelc_volb_sts",
            "vote A volatitlity" = "lhelc_vola_vts",
            "vote B volatitlity" = "lhelc_volb_vts"
            #"Sum of open Veto Points" = "vto_pts"
          ),
          selected = "lhelc_lsq"
        ),
        selectInput("variable_bivar2", h5("Variable y-axis"),
          choices = list("LH Disproportionality" = "lhelc_lsq",
            "effective number of parties" = "lh_enpp",
            "cabinet seat share" = "cab_lh_sts_shr",
            "seat A volatitlity" = "lhelc_vola_sts",
            "seat B volatitlity" ="lhelc_volb_sts",
            "vote A volatitlity" = "lhelc_vola_vts",
            "vote B volatitlity" = "lhelc_volb_vts"
            #"Sum of open Veto Points" = "vto_elct"
          ),
          selected = "lh_enpp"
        ),
        radioButtons("axis_scale", label = h5("Range axes"), 
          choices = list("Adjusted" = 2, "Original" = 1),
          selected = 2
        ),
        radioButtons("prediction_box", label = h5("Add prediction"), 
          choices = list("None" = 0, "Linear" = 1, "Local" = 2),
          selected = 0
        )
      ),
      conditionalPanel(condition = "input.tabs == 3",
        selectInput("variable_barplot", h5("Variable"),
          choices = list("Lower house seat share" = "pty_lhelc_sts_shr",
            "Upper house seat share" = "pty_uh_sts_shr"
          )
        ),
        checkboxInput("label_barplot", "Add percentage labels"),
        checkboxInput("flip_barplot", "Flip graph"),
        radioButtons("threshold_barplot", label = h5("Threshold graph inclusion"), 
          choices = list("2.5%" = 1, "5%" = 2, "10%" = 3),
          selected = 2
        )
      ),
      conditionalPanel(condition = "input.tabs == 4",
        selectInput("variable_map", h5("Variable"),
          choices = list("Average Cabinet Lower House Seat Share" = "Average Cabinet Lower House Seat Share")
        )
      ),

      conditionalPanel(condition = "input.tabs < 4",
        sliderInput(
          "year_range",
          label = h5("Year range"),
          min = 1940,
          max = 2016,
          value = c(1940, 2016),
          sep = ""
        )
      )
    ),

    mainPanel(

      tabsetPanel(id ="tabs",
        tabPanel("Veto Points",
          value = 1,
          tableOutput("information_veto"),
          h5(textOutput("polltitle_veto", inline=TRUE)),
          plotlyOutput("lineplot_veto"),
          downloadButton('downloadVetoPlot', 'Download graph'),
          tableOutput("summary_veto"),
          downloadButton('downloadVetoTable', 'Download table')
        ),
        tabPanel("Electoral System Associations",
          value = 2,
          h5(textOutput("polltitle_bivar", inline=TRUE)),
          plotlyOutput("plot_bivar"),
          downloadButton('downloadBivariatePlot', 'Download graph'),
          tableOutput("summary_bivar"),
          downloadButton('downloadBivariateTable', 'Download table')
        ),
        tabPanel("Party Seat Shares",
          value = 3,
          plotlyOutput("plot_bar"),
          downloadButton('downloadBarPlot', 'Download graph'),
          tableOutput("summary_barplot"),
          downloadButton('downloadBarTable', 'Download table')
        ),
        tabPanel("Map",
          value = 4,
          leafletOutput("plot_map", height="600px")
        )
      )
    )
  ),

  tags$footer(
    tags$a(target="_blank", href="https://www.sowi.hu-berlin.de/de/lehrbereiche/comppol", "Chair Comparative Politics at Humboldt-Universität zu Berlin"),
    tags$span(" · "),
    tags$a(target="_blank", href="https://welfarestatefutures.org", " Welfare State Futures - Norface")
  )

))