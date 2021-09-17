library(sp)
library(xts)
library(spacetime)
library(RColorBrewer)
#library(leafpop)
library(plotly)
library(grid)
library(png)
library(shinyalert)
library(readxl)



if(system("hostname", intern = TRUE) == "atlasmortalidad.uclm.es") {
  library("leafpop",
    lib.loc = "/home/atlas/R/x86_64-redhat-linux-gnu-library/3.5")
} else {
  library("leafpop")
}


# Load some functions
source("utils.R")

# Check
print("Reading server.R")

# Source week data
source("read_eurostat.R")

#Load
#load("extract_data.RData")

#country <- "Italy"
#country <- "Greece"
#country <- "Switzerland"
#load(paste0("data/", country, ".RData"))
#load(paste0("data/", country, "_p_list.RData"))

print("----------DATA LOADED-----------")

# LEaflet tiles
leaflet_tiles <- "CartoDB.Positron"
leaflet_tiles <- "Stamen.Watercolor"
leaflet_tiles <- "OpenStreetMap"

# HIghlight option
h_options <- highlightOptions(color = "white", weight = 2,
  bringToFront = TRUE)


# Define server logic for random distribution app ----
server <- function(input, output, session) {

   shinyalert(title = "Welcome", type = "info", html = TRUE,
     text = includeMarkdown("info.Rmd"))


  # Reactive data to be loaded when a country is selected
  rv <- reactiveValues(
    data = NULL, # Main data
    tmp_plots = NULL # Plots of temporal trends
  )

  # Load data
  observeEvent(input$country, {
    print("Load data")
    rv$data <- mget(load(paste0("data/", input$country, ".RData")))
    rv$tmp_plots <- mget(load(paste0("data/", input$country, "_p_list.RData")))[[1]]

  }) # End: Load data

  # Observe what tab is being selected
  # This is used to set the values in the selection
  observe({
    tabs <- input$tabs

    # 'Temporal Analysis' group
    if(tabs %in% c("tmpsummary", "tmpexcess", "tmpexcess2")) {
      # National level
      updateSelectInput(session, "aggregation", "Aggregation",
       list("National"  = "country", "Region (NUTS2)" = "region",
       "Province (NUTS3)" = "province"), selected = "country")
    }

    if(tabs == "tmpexcess2") {
      # Set number of deaths
      updateSelectInput(session, "variable", "Variable",
        list("Relative excess mortality" = "REM",
        "Number of excess deaths" = "NED"), selected = "NED"
    )

    # Set median 
    updateSelectInput(session, "statistic", "Statistic",
      list("Median" = "median", "Posterior probability" = "pprob"),
      selected = "median"
    )

   updateSelectInput(session, "gender", "Gender",
     list("Both" = "B", "Females" = "F", "Males" = "M"), selected = "F")
   updateSelectInput(session, "agegroup", "Age Group", 
     #c("All", "40<", "40-59", "60-69", "70-79", "80+"),
     #selected = "40<")
     # 40< not shown but are included in 'All'
     c("All", "40-59", "60-69", "70-79", "80+"),
     selected = "40-59")
    }

  })

  # Obtain index for age_sex_group
  # Returns a vector of two: grouping and specific stratum
  get_age_sex_data <- function(weekly = FALSE) {

    if(!weekly)
      tab <- rv$data$d
    else
      tab <- rv$data$d_week

    # Main subset
    aux <- tab[[input$aggregation]]

    if(input$gender == "B" & input$agegroup == "All")
      return(aux[[1]])

    if(input$gender == "B") 
      return(aux[["age"]][[input$agegroup]])

    if(input$agegroup == "All")
      return(aux[["sex"]][[input$gender]])

    return(aux[["agesex"]][[paste0(input$gender, input$agegroup)]])
  }

  # Create a summary table from an sf object
  # st_tab: sf object with the data
  summary_table <- function(st_tab) {
    if("sf" %in% class(st_tab))
      tab <- st_drop_geometry(st_tab)
    else 
      tab <- st_tab

    tab <- tab[, c("mean.excess", "median.excess", "mean.excess.deaths", "median.excess.deaths")]

    res <- apply(tab, 2, function(X) {
      c(mean(X), median(X), quantile(X, c(0.025, 0.975)))
  
    })
    row.names(res) <- c("Mean", "Median", "0.025 q.", "0.975 q.")
    colnames(res)<- c("REM (mean)", "REM (median)", "NED (mean)", "NED (median)")

    return(res)
  }



  output$tabspsummary <- renderTable({
    #print("Show table")
    tab <- summary_table(get_age_sex_data()) #st_drop_geometry(get_age_sex_data())
    #print(tab)
   }, rownames = TRUE) #options = list(autoWidth = TRUE, scrollX = TRUE))

#  output$tabspsummary <- renderDataTable({
#    print("Show table")
#    tab <- st_drop_geometry(get_age_sex_data())
#     data.frame("Relative excess mortality (median)" = mean(tab$median.excess))
#   }, options = list(autoWidth = TRUE, scrollX = TRUE))


  # Create pop-ups for maps
  # mymap: Spatial information
  # tab: table for temporal pop-ups

  create_popup <- function(mymap, tab = NULL) {

    if(is.null(tab)) {
      popup <- paste0(
        #paste("<b>ID_PE</b> ", mymap$ID_PE, sep = ""), "<br>",
        paste(paste0("<b>Name:</b> ", mymap$NAME, "<br>")),
        #paste(paste0("<b>Deaths:</b> ", mymap$deaths, "<br>")),
        paste(paste0("<b>Relative Excess Deaths (median):</b> ", round(mymap$median.excess, 2)), "<br>"),
        paste(paste0("<b>Relative Excess Deaths (95% c.i.):</b> ", paste0("(", round(mymap$low.excess, 2), ", ", round(mymap$upp.excess, 2), ")")), "<br>"),
        paste(paste0("<b>Relative Excess Deaths (post. prob.):</b> ", round(mymap$ExProb, 2), "<br>")),
        paste(paste0("<b>Number of Excess Deaths (median):</b> ", round(mymap$median.excess.deaths, 2)), "<br>"),
        paste(paste0("<b>Number of Excess Deaths (95% c.i.):</b> ", paste0("(", round(mymap$low.excess.deaths, 2), ", ", round(mymap$upp.excess.deaths, 2), ")")), "<br>"),
        paste(paste0("<b>Number of Excess Deaths (post. prob.):</b> ", round(mymap$ExProb.deaths, 2), "<br>")),
        paste(paste0("<b>Age Group:</b> ", input$agegroup, "<br>")),
        paste(paste0("<b>Gender:</b> ", 
          ifelse(input$gender == "B", "Both", ifelse(input$gender == "F", "Females", "Males")), "<br>"))
        #paste(paste0("<b>Population:</b> ", round(mymap$population, 2)))
      )
    } else { # Temporal pop-up

      # Set variables for plot
      if(input$variable == "REM") {
        if(input$statistic == "median") {
          y_var <- "median.excess"
          y_low <- "low.excess"
          y_upp <- "upp.excess"
          y_lab <- "Relative excess mortality (median)"
        } else {
          y_var <- "ExProb"
          y_low <- NULL
          y_upp <- NULL
          y_lab <- "Relative excess mortality (post. prob.)"
       }
      } else {
        if(input$statistic == "median") {
          y_var <- "median.excess.deaths"
          y_low <- "low.excess.deaths"
          y_upp <- "upp.excess.deaths"
          y_lab <- "Number of excess deaths (median)"
        } else {
          y_var <- "ExProb.deaths"
          y_low <- NULL
          y_upp <- NULL
          y_lab <- "Number of excess deaths (post. prob.)"
       }
      }


      if(input$aggregation == "country") geo.var <- "COUNTRY"
      if(input$aggregation == "region") geo.var <- "RegionID"
      if(input$aggregation == "province") geo.var <- "ID_space"

      # Produce plot
      plts <- lapply(unlist(st_drop_geometry(mymap[, geo.var])), function(ID) {
        aux <- filter(tab, .data[[geo.var]] == ID)

        res <- ggplot(aux) + geom_line(aes_string(x = "Week", y = y_var)) +
          xlab("Week") +
          ylab(y_lab) +
          scale_x_continuous(breaks = euro_xaxis$weekID,
            labels = euro_xaxis$month.an,expand = c(0, 0)) 

       if(y_var %in% c("median.excess", "median.excess.deaths")) {
         res <- res +
            geom_hline(yintercept = 0, col ="red", linetype = "dashed")
       }
          if(!is.null(y_low) )
            res <- res + geom_ribbon(aes_string(x = "Week", ymin = y_low, ymax = y_upp), alpha = 0.25) 
          return(res)
      })

      #print(length(plts))

      popup <- popupGraph(plts)
    }

  return(popup)
  }


  map_REM_mean <- function() {
    return(NULL)
  }

  # tab: Tab for popups
  map_REM_median <- function(tab = NULL) {

    mymap <- get_age_sex_data()

    #colors <- c(rev(brewer.pal(n = 6, name = "RdBu")))
    # Colours as in paper
    colors <- rev(brewer.pal(n = 10, name = "RdBu")[1:6])
    labels <- levels(mymap$Median.cat)
    factpal <- colorFactor(colors, mymap$Median.cat)

    popup <- create_popup (mymap, tab)
    #popup <- NULL

    res <- leaflet(mymap) %>% 
      addProviderTiles(leaflet_tiles,
          options = providerTileOptions(opacity = 0.99))  %>%
      addPolygons(fillColor = ~ factpal(Median.cat), weight = 1, opacity = 1, color = "black", fillOpacity = 0.75, popup = popup, highlightOptions = h_options) %>%
      leaflet::addLegend("topright", colors = colors, labels = labels,
         values = values)

    return(res)
  }

  # tab: Tab for popups
  map_REM_pprob <- function(tab = NULL) {

    mymap <- get_age_sex_data()

    popup <- create_popup (mymap, tab)
  
    #colors <- rev(brewer.pal(n = 3, name = "RdBu"))
    # Colours as in paper
    colors <- rev(brewer.pal(n = 11, name = "RdBu")[c(2, 4, 6, 8, 10)])
    labels <- levels(mymap$ex.cat)
    factpal <- colorFactor(colors, mymap$ex.cat)

    res <- leaflet(mymap) %>%
      addProviderTiles(leaflet_tiles,
          options = providerTileOptions(opacity = 0.99))  %>%
      addPolygons(fillColor = ~ factpal(ex.cat), weight = 1, opacity = 1, color = "black", fillOpacity = 0.75, popup = popup, highlightOptions = h_options) %>%
      leaflet::addLegend("topright", colors = colors, labels = labels,
         values = values)

    return(res)
  }


  map_NED_mean <- function() {
    return(NULL)
  }

  # tab: Tab for popups
  map_NED_median <- function(tab = NULL) {
 mymap <- get_age_sex_data()
    
    colors <- c(rev(brewer.pal(n = 8, name = "RdBu")))
    labels <- levels(mymap$Median.deaths.cat)
    factpal <- colorFactor(colors, mymap$Median.deaths.cat)
    
    popup <- create_popup (mymap, tab)
    #popup <- NULL
    
    res <- leaflet(mymap) %>% 
      addProviderTiles(leaflet_tiles,
          options = providerTileOptions(opacity = 0.99))  %>% 
      addPolygons(fillColor = ~ factpal(Median.deaths.cat), weight = 1, opacity = 1, color = "black", fillOpacity = 0.75, popup = popup, highlightOptions = h_options) %>%   
      leaflet::addLegend("topright", colors = colors, labels = labels,
         values = values)
    

  }

  # tab: Tab for popups
  map_NED_pprob <- function(tab = NULL) {
    mymap <- get_age_sex_data()


    popup <- create_popup (mymap, tab)

    #colors <- rev(brewer.pal(n = 3, name = "RdBu"))
    # Colours as in paper
    colors <- rev(brewer.pal(n = 11, name = "RdBu")[c(2, 4, 6, 8, 10)])
    labels <- levels(mymap$ex.deaths.cat)
    factpal <- colorFactor(colors, mymap$ex.deaths.cat)

    res <- leaflet(mymap) %>%
      addProviderTiles(leaflet_tiles,
          options = providerTileOptions(opacity = 0.99))  %>%
      addPolygons(fillColor = ~ factpal(ex.deaths.cat), weight = 1, opacity = 1, color = "black", fillOpacity = 0.75, popup = popup, highlightOptions = h_options) %>%
      leaflet::addLegend("topright", colors = colors, labels = labels,
         values = values)

    return(res)
  }


  output$leafletexcess <- renderLeaflet({

    if(input$variable == "REM") {
      the_map <- switch(input$statistic,
        mean = map_REM_mean(),
        median = map_REM_median(),
        pprob = map_REM_pprob()
      )
    } else {
      the_map <- switch(input$statistic,
        mean = map_NED_mean(),
        median = map_NED_median(),
        pprob = map_NED_pprob()
      )
    }

    return(the_map)
  })


#  output$leafletpprob <- renderLeaflet({
#
#    
#   mymap <- get_age_sex_data()
#   
#   popup <- create_popup (mymap)
#   #popup <- NULL
#   
#   #warning("reproject data before loading the App")
#   #mymap <- st_transform(mymap, '+proj=longlat +datum=WGS84')
#   
#   colors <- rev(brewer.pal(n = 3, name = "RdBu"))
#   labels <- levels(mymap$ex.cat)
#   factpal <- colorFactor(colors, mymap$ex.cat)
#    
#    res <- leaflet(mymap) %>% 
#      addProviderTiles(leaflet_tiles,
#          options = providerTileOptions(opacity = 0.99))  %>% 
#      addPolygons(fillColor = ~ factpal(ex.cat), weight = 1, opacity = 1, color = "black", fillOpacity = 0.75, popup = popup, highlightOptions = h_options) %>%
#      leaflet::addLegend("topright", colors = colors, labels = labels,
#         values = values)
#    
#    return(res)
#  })

  output$tabtmpsummary <- renderTable({

    tab <- summary_table(get_age_sex_data(weekly = TRUE)) #st_drop_geometry(get_age_sex_data())
    #print(tab)
   }, rownames = TRUE) #options = list(autoWidth = TRUE, scrollX = TRUE))



  plot_weekly_REM_mean <- function(tab) {
    return(NULL)
  }

  plot_weekly_REM_median <- function(tab) {
    plt <- ggplot(tab) +
      geom_line(aes(x = Week, y = median.excess)) +
      geom_ribbon(aes(x = Week, ymin = low.excess, ymax = upp.excess), alpha = 0.25) + 
      ylab("Relative excess mortality (median)") +
      scale_x_continuous(breaks = euro_xaxis$weekID,
        labels = euro_xaxis$month.an,expand = c(0, 0)) +
      geom_hline(yintercept = 0, col ="red", linetype = "dashed")

      

    return(plt)
  }

  plot_weekly_REM_pprob <- function(tab) {
    plt <- ggplot(tab) +
     geom_line(aes(x = Week, y = ExProb)) +
     ylab("Relative excess mortality (posterior probability)") + 
     scale_x_continuous(breaks = euro_xaxis$weekID,
        labels = euro_xaxis$month.an,expand = c(0, 0))

   return(plt)
  }


  plot_weekly_NED_mean <- function(tab) {
    return(NULL)
  }

  plot_weekly_NED_median <- function(tab) {
    plt <- ggplot(tab) +
      geom_line(aes(x = Week, y = median.excess.deaths)) +
      geom_ribbon(aes(x = Week, ymin = low.excess.deaths, ymax = upp.excess.deaths), alpha = 0.25) +
      ylab("Number of excess deaths (median)") +
      scale_x_continuous(breaks = euro_xaxis$weekID,
        labels = euro_xaxis$month.an,expand = c(0, 0)) +
      geom_hline(yintercept = 0, col ="red", linetype = "dashed")


    return(plt)
  }

  plot_weekly_NED_pprob <- function(tab) {
    plt <- ggplot(tab) +
     geom_line(aes(x = Week, y = ExProb.deaths)) +
     ylab("Number of excess deaths (posterior probability)") +
     scale_x_continuous(breaks = euro_xaxis$weekID,
        labels = euro_xaxis$month.an,expand = c(0, 0))
   
   return(plt)
  }


  

#  output$mortalityplot <- renderPlotly({
#    tab <- get_age_sex_data(weekly = TRUE)
#    tab$Week <- as.integer(substr(tab$EURO_LABEL, 7,8))
#
#    if(input$aggregation == "country")
#    plt <- ggplot(tab) +
#     geom_line(aes(x = Week, y = observed))
#
#     if(input$aggregation == "region")
#       plt <- ggplot(tab) + geom_line(aes(x = Week, y = observed, group = RegionID, col = RegionID))
#
#     if(input$aggregation == "province")
#       plt <- ggplot(tab) + geom_line(aes(x = Week, y = observed, group = ID_space, col = ID_space))
#
#
#    ggplotly(plt)
#  })


  # IMPORTANT: Only NATIONAL DATA is shown here
  output$excessplot <- renderPlotly({

    tab <- get_age_sex_data(weekly = TRUE)
    tab$Week <- as.integer(substr(tab$EURO_LABEL, 7,8))

    if(input$variable == "REM") {
      the_plot <- switch(input$statistic,
        mean = plot_weekly_REM_mean(tab),
        median = plot_weekly_REM_median(tab),
        pprob = plot_weekly_REM_pprob(tab)
      )
    } else {
      the_plot <- switch(input$statistic,
        mean = plot_weekly_NED_mean(tab),
        median = plot_weekly_NED_median(tab),
        pprob = plot_weekly_NED_pprob(tab)
      )
    }

#    if(input$aggregation == "country") {
#    plt <- ggplot(tab) +
#     geom_line(aes(x = Week, y = mean.excess)) +
#     geom_ribbon(aes(x = Week, ymin = low.excess, ymax = upp.excess), alpha = 0.25)
#   }

   #if(input$aggregation == "region") {
   #  plt <- ggplot(tab) +
   #    geom_line(aes(x = Week, y = mean.excess, group = RegionID, col = RegionID)) 
   #}
   #
   #if(input$aggregation == "province") {
   #  plt <- ggplot(tab) +
   #    geom_line(aes(x = Week, y = mean.excess, group = ID_space, col = ID_space))
#
#   }

    ggplotly(the_plot)
  })

  output$excessplot2 <- renderImage({

    # Set index
    # Ordering is F 40>, F 40-..., etc.
    idx <- 0
    if(input$gender == "B" | input$agegroup == "All") return(NULL)

    if(input$gender == "M") idx <- 5

    idx <- idx + switch(input$agegroup,
      "40<" = 1,
      "40-59" = 2,
      "60-69" = 3,
      "70-79" = 4,
      "80+" = 5
    )

    outfile <- tempfile(fileext = '.png')

    writePNG(rv$tmp_plots[[idx]], outfile)

    list(src = outfile,
         contentType = 'image/png',
         width = 600,
         height = 400,
         alt = "Observed and excess mortality.")
  }, deleteFile = TRUE)

#   output$pprobplot <- renderPlotly({
#
#   # Set national for this case
#   #updateSelectInput(session, "inSelect",
#   #   label = paste("Select input label", length(x)),
#   #   choices = x,
#   #   selected = tail(x, 1)
#   # )
#
#
#    tab <- get_age_sex_data(weekly = TRUE)
#    tab$Week <- as.integer(substr(tab$EURO_LABEL, 7,8))
#
#    if(input$aggregation == "country") {
#    plt <- ggplot(tab) +
#     geom_line(aes(x = Week, y = ExProb))
#   }
#
#   if(input$aggregation == "region") {
#     plt <- ggplot(tab) +
#       geom_line(aes(x = Week, y = ExProb, group = RegionID, col = RegionID))
#   }
#
#   if(input$aggregation == "province") {
#     plt <- ggplot(tab) +
#       geom_line(aes(x = Week, y = ExProb, group = ID_space, col = ID_space))
#   
#   }
#
#    ggplotly(plt)
#
#  })
#


  output$tabstsummary <- renderTable({
    #print("Show table")
    tab <- summary_table(get_age_sex_data(weekly = TRUE)) #st_drop_geometry(get_age_sex_data())
    print(tab)
   }, rownames = TRUE) #options = list(autoWidth = TRUE, scrollX = TRUE))

  

  output$spacetimeplot <- renderLeaflet({
    require(leafpop)

    # Map with data
    map <- get_age_sex_data()
    # Time series
    tab <- get_age_sex_data(weekly = TRUE)
    tab$Week <- as.integer(substr(tab$EURO_LABEL, 7,8))


#    if(input$aggregation == "country") geo.var <- "COUNTRY"
#    if(input$aggregation == "region") geo.var <- "RegionID"
#    if(input$aggregation == "province") geo.var <- "ID_space"
#
#    # Produce plot
#    plts <- lapply(unlist(st_drop_geometry(map[, geo.var])), function(ID) {
#      aux <- filter(tab, .data[[geo.var]] == ID)
#
#      ggplot(aux) + geom_line(aes(x = Week, y = median.excess))+
#        geom_ribbon(aes(x = Week, ymin = low.excess, ymax = upp.excess), alpha = 0.25) +
#      xlab("Week") + 
#      ylab("Relative excess mortality (median)")
#    })
#
#    print(length(plts))
#
#    plts <- popupGraph(plts)


  if(input$variable == "REM") {
      the_plot <- switch(input$statistic,
        mean = map_REM_mean(tab),
        median = map_REM_median(tab),
        pprob = map_REM_pprob(tab)
      )
    } else {
      the_plot <- switch(input$statistic,
        mean = map_NED_mean(tab),
        median = map_NED_median(tab),
        pprob = map_NED_pprob(tab)
      )
    }

    return(the_plot)


     kk <-  map_REM_median(tab)
     return(kk)

     plts <- create_popup(map, tab)

     colors <- c(rev(brewer.pal(n = 6, name = "RdBu")))
     labels <- levels(map$Median.cat)
     factpal <- colorFactor(colors, map$Median.cat)
    
      res <- leaflet(map) %>% 
      addProviderTiles(leaflet_tiles,
          options = providerTileOptions(opacity = 0.99))  %>% 
      addPolygons(fillColor = ~ factpal(Median.cat), weight = 1, opacity = 1, color = "black", fillOpacity = 0.75, popup = plts, highlightOptions = h_options) %>%   
      leaflet::addLegend("topright", colors = colors, labels = labels,
         values = values)
    
    return(res)



  })



}
