# Define UI for random distribution app ----

library(shinydashboard)
library(plotly)
library(leaflet)
#library(leafpop)
library(plotly)
library(shinyalert)


if(system("hostname", intern = TRUE) == "atlasmortalidad.uclm.es") {
  library("leafpop",
    lib.loc = "/home/atlas/R/x86_64-redhat-linux-gnu-library/3.5")
} else {
  library("leafpop")
}


# Header con logo
dbHeader <- dashboardHeader(title = "EXCESS MORTALITY APP",
 tags$li(a(href = '', #'https://www.uclm.es',
   #img(src = 'Logo_uclm.png',
   img(src = '',
   #title = "Universidad de Castilla-La Mancha", height = "30px"),
   title = "", height = "30px"),
   style = "padding-top:10px; padding-bottom:10px;"),
   class = "dropdown"
 )
)

ui <- dashboardPage(title = "Excess Mortality App",
    skin = "red", 
  dbHeader,
  dashboardSidebar(
    useShinyalert(),  # Set up shinyalert
    sidebarMenu(id = "tabs",
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      menuItem("Spatial Analysis", tabName = "spatial", icon = icon("map"),
        menuSubItem("Summary", tabName = "spsummary"),
        menuSubItem("Excess Mortality", tabName = "spexcess")
      ),
      menuItem("Temporal Analysis", tabName = "temporal", icon = icon("calendar", lib = "glyphicon"),
        menuSubItem("Summary", tabName = "tmpsummary"),
        #menuSubItem("Mortality", tabName = "tmpmortality"),
        menuSubItem("Excess Mortality", tabName = "tmpexcess"),
        menuSubItem("Observed and Excess Mortality", tabName = "tmpexcess2")#,
        #menuSubItem("Post. Prob.", tabName = "tmppprob")
      ),
      menuItem("Spatio-Temporal Analysis", tabName = "st", icon = icon("globe"),
        menuSubItem("Summary", tabName = "stsummary"),
        menuSubItem("Excess mortality", tabName = "stexcess")
      ),
      #menuItem("Help", tabName = "help", icon = icon("question")),
      menuItem("Contact", tabName = "contact", icon = icon("envelope"))
    ),
    hr(),
    selectInput(
      "variable",
      "Variable", 
      list("Relative excess mortality" = "REM",
        "Number of excess deaths" = "NED")),
    selectInput(
      "statistic",
      "Statistic", 
      list("Median" = "median", "Posterior probability" = "pprob")
    ),
    selectInput("country", "Country", c("England", "Greece", "Italy", "Spain", "Switzerland"),
      selected = "Switzerland"),
    selectInput("gender", "Gender", list("Both" = "B", "Females" = "F", "Males" = "M"),
      selected = "Females"),
    #selectInput("agegroup", "Age Group", c("All", "40<", "40-59", "60-69", "70-79", "80+"),
    # 40< results now shown because of small observed cases
    # however, they are included in 'All'
    selectInput("agegroup", "Age Group", c("All", "40-59", "60-69", "70-79", "80+"),
      selected = "All"),
    selectInput("aggregation", "Aggregation", list("National"  = "country", "Region (NUTS2)" = "region", "Province (NUTS3)" = "province"), selected = "province")
  ),
  dashboardBody(
    tags$head(includeHTML(("google-analytics.html"))),
    tabItems(
      tabItem(tabName = "introduction", fluidPage(includeMarkdown("introduction.Rmd"))),
      tabItem(tabName = "spsummary",
        fluidPage(
        h4("Summary"),
          #dataTableOutput('tabspsummary')
          tableOutput('tabspsummary'),
          fluidPage(includeMarkdown("legend.Rmd"))
        )
      ),
      tabItem(tabName = "spexcess",
        fluidPage(
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
          h4("Excess mortality"),
tags$style(type = "text/css", "#leafletexcess {height: calc(100vh - 110px) !important;}"),
            shinycssloaders::withSpinner(leafletOutput("leafletexcess"),
              type = 6, color = "#990033")
        )
      ),
#      tabItem(tabName = "sppprob",
#        fluidPage(
##https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
#          h4("Posterior probabilities"),
#tags$style(type = "text/css", "#leafletpprob {height: calc(100vh - 110px) !important;}"),
#            shinycssloaders::withSpinner(leafletOutput("leafletpprob"),
#              type = 6, color = "#990033")
#        )
#      ),
#      tabItem(tabName = "tasas",
#        fluidPage(
#          h4(uiOutput("titulotasas")),
#          tags$style(type = "text/css", "#leaflettasas {height: calc(100vh - 110px) !important;}"),
#            shinycssloaders::withSpinner(leafletOutput("leaflettasas"),
#              type = 6, color = "#990033")
#        )
#      ),
      tabItem(tabName = "temporal", 
        fluidPage(
          h4("Excess Mortality"),
          tags$style(type = "text/css", "#leaflettseries {height: calc(100vh - 230px) !important;}"),
          shinycssloaders::withSpinner(leafletOutput("leaflettseries"),
            type = 6, color = "#990033"),
        )),
#      tabItem(tabName = "Rt",
#        fluidPage(
#          h4(uiOutput("tituloRt")),
#          #h4("Número reproductivo básico instantaneo"),
#          h5(strong("Pincha en un municipio para ver la evolución del número reproductivo básico instantaneo")),
#          tags$style(type = "text/css", "#leafletRt {height: calc(100vh - 230px) !important;}"),
#          shinycssloaders::withSpinner(leafletOutput("leafletRt"),
#            type = 6, color = "#990033"),
#        )),
#
     tabItem(tabName = "tmpsummary",
       fluidPage(
       h4("Summary (national level)"),
         #dataTableOutput('tabspsummary')
         tableOutput('tabtmpsummary'),
         h5("This summary only provides statistics of national estimates."),
         fluidPage(includeMarkdown("legend.Rmd")) 
       )
    ),
    tabItem(tabName = "tmpmortality",
      fluidPage(
        h4("Mortality"),
        plotlyOutput("mortalityplot")
      )
    ),
     tabItem(tabName = "tmpexcess",
       fluidPage(
         h4("Excess Mortality"),
         plotlyOutput("excessplot"),
         fluidPage(includeMarkdown("legend_temp.Rmd"))
         )
     ),
     tabItem(tabName = "tmpexcess2",
       fluidPage(
         h4("Observed and Estimated Number of Deaths"),
         #plotOutput("excessplot2"),
         imageOutput("excessplot2"),
         HTML("<B>Please, select an <I>age</I> and <I>gender</I> group different from 'All' and 'Both', respectively</B>. Actual number of deaths are represented by the dark green line while estimated number of deaths (under the counterfactual of no pandemic) are shown in the black line. Credible intervals at different levels are shown using the shaded regions. Check paper for details.")
         )
     ),
     tabItem(tabName = "tmppprob",
       fluidPage(
         h4("Excess Mortality"),
         plotlyOutput("pprobplot")
         )
     ),
     tabItem(tabName = "stsummary",
       fluidPage(
       h4("Summary"),
         #dataTableOutput('tabspsummary')
         tableOutput('tabstsummary'),
         fluidPage(includeMarkdown("legend.Rmd")) 
       )
    ),
     tabItem(tabName = "stexcess",
       fluidPage(
         h4("Spatio-Temporal Analysis"),
         tags$style(type = "text/css", "#leaflettseries {height: calc(100vh - 230px) !important;}"),
          shinycssloaders::withSpinner(leafletOutput("spacetimeplot"),
            type = 6, color = "#990033"),
          HTML("The map shows the estimates of the variable for the whole of 2020. <b>Click on a region</b> to see the weekly values of the variable.")
         )
     ),
#     tabItem(tabName = "ine",
#       fluidPage(
#         h4(uiOutput("tituloINE")),
#         h5(strong("Datos de estimación de defunciones semanales (Instituto Nacional de Estadística)")),
#         plotlyOutput("ineplot"),
#           fluidRow(
#             selectInput("provinciaine", "Provincia", c("Todas", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo"),
#               choices = list(Todas = "Todas", Albacete = "AB", "Ciudad Real" = "CR", Cuenca = "CU", Guadalajara = "GU", Toledo = "TO")),
#             selectInput("sexoine", "Sexo", list(Todos = "Todos", Hombres = "Hombre", Mujeres = "Mujer"),
#               selected = "Todos"),
#             selectInput("gedadine", "Grupo de edad",
#                c("Todos", "Edad < 65", "Edad 65-74", "Edad > 75"),
#                selected = "Todos"),
#           )
#         )
#     ),

#     tabItem(tabName = "rebrote",
#       fluidPage(
#         h4(uiOutput("titulorebrote")),
#tags$style(type = "text/css", "#leafletrebrote {height: calc(100vh - 230px) !important;}"),
#         shinycssloaders::withSpinner(leafletOutput("leafletrebrote"),
#            type = 6, color = "#990033")
#       )
#     ),


#      tabItem(tabName = "umbral",
#        fluidPage(
#          h4(uiOutput("tituloumbral")),
#tags$style(type = "text/css", "#leafletumbral {height: calc(100vh - 230px) !important;}"),
#          shinycssloaders::withSpinner(leafletOutput("leafletumbral"),
#            type = 6, color = "#990033"),
#          br(),
#          sliderInput("sliderumbral",
#            label = "Umbral de confinamiento (IA14 por 100.000 habs.)",
#            width = "325px",
#            value = 500,
#            min = 0,
#            max = 2000,
#            step = 10,
#            animate = FALSE)
#        )
#      ),
#      tabItem(tabName = "temporalcasos",
#        fluidPage(
#          h4(uiOutput("titulotemporalcasos")),
#tags$style(type = "text/css", "#leaflettemporalcasos {height: calc(100vh - 230px) !important;}"),
#          shinycssloaders::withSpinner(leafletOutput("leaflettemporalcasos"),
#            type = 6, color = "#990033"),
#          br(),
#          sliderInput("slidersemanacasos",
#            label = "Semana",
#            value = 34,
#            min = 34,
#            max = ultima.semana,
#            step = 1,
#            animate = animationOptions(interval = 8000))
#        )
#      ),
#      tabItem(tabName = "temporaltasas",
#        fluidPage(
#          h4(uiOutput("titulotemporaltasas")),
#tags$style(type = "text/css", "#leaflettemporaltasas {height: calc(100vh - 230px) !important;}"),
#          shinycssloaders::withSpinner(leafletOutput("leaflettemporaltasas"),
#            type = 6, color = "#990033"),
#          br(),
#          sliderInput("slidersemanatasas",
#            label = "Semana",
#            value = 34,
#            min = 34,
#            max = ultima.semana,
#            step = 1,
#            animate = animationOptions(interval = 8000))
#        )
#      ),
#      tabItem(tabName = "observatorio",
#        fluidRow(
#          htmlOutput("obscovid19ab"),
#          tags$style(type = "text/css", "#ABOBSiframe {height: calc(100vh - 230px) !important;}")
#        )
#      ),
      #tabItem(tabName = "help", fluidPage(withMathJax(includeMarkdown("help.Rmd")))),
      tabItem(tabName = "contact", fluidPage(includeMarkdown("contact.Rmd")))

    )
  )
)

