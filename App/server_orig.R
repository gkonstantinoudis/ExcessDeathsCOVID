library(sp)
library(xts)
library(spacetime)
library(RColorBrewer)
#library(leafpop)
library(plotly)
library(grid)





if(system("hostname", intern = TRUE) == "atlasmortalidad.uclm.es") {
  library("leafpop",
    lib.loc = "/home/atlas/R/x86_64-redhat-linux-gnu-library/3.5")
} else {
  library("leafpop")
}


# Última semana de datos
#source("ultima_semana.R")
load("ultima_semana.RData")

#source("utils.R")

# Check
print("Reading server.R")

# Load data
load("covid19-CLM.RData")
load("tseries.RData")
load("Rt.RData")
load("momo.RData")
load("INE_mortalidad.RData")

# Semanas
dia1 <- index(stfdf@time)
dia2 <- index(stfdf@time) + 60 * 60 * 24 * 6

dias <- paste(format(dia1, format = "%d-%m-%Y"), 
  format(dia2, format = "%d-%m-%Y"), sep = " a ")

# Crear categorías
stfdf$CASOSCAT <- mycut(stfdf$CASOS, c(0, 5, 10, 50, 100, 200, max(stfdf$CASOS, na.rm = TRUE)))
stfdf$TASACAT <- mycut(stfdf$TASA, c(0, 1, 5, 10, 50, 100, 500, max(stfdf$TASA, na.rm = TRUE)))
stfdf$IA14CAT <- mycut(stfdf$IA14, c(0, 1, 5, 10, 50, 100, 500, max(stfdf$TASA, na.rm = TRUE)))

# Define server logic for random distribution app ----
server <- function(input, output, session) {


    output$leafletcasos <- renderLeaflet({

      # Índice de la semana
      semanaidx <- as.integer(input$semana) - 33
      # Mapa
      mapa <- stfdf[, semanaidx]
      # reclasificar niveles
      mapa$CASOSCAT <- factor(mapa$CASOSCAT, levels(stfdf$CASOSCAT))
  
      # Título
      output$titulocasos <- renderUI(paste0("Casos totales (Semana ", input$semana, ", ", dias[semanaidx], ")"))


      # Paleta de colores
      colors <- c("gray", brewer.pal(nlevels(stfdf$CASOSCAT) - 1, "Reds"))
      labels <- levels(stfdf$CASOSCAT)
      factpal <- colorFactor(colors, stfdf$CASOSCAT)

      # Pop-up
      popup <- paste0(
        paste("<b>Municipio:</b> ", mapa$MUNICIPIO, sep = ""), "<br>",
        paste(paste0("<b>Semana:</b> ", input$semana, "<br>")),
        paste(paste0("<b>Fecha:</b> ", dias[semanaidx], "<br>")),
        paste(paste0("<b>Casos:</b> ", mapa$CASOS, "<br>")),
        paste(paste0("<b>IA7:</b> ", signif(mapa$TASA, 5), "<br>")),
        paste(paste0("<b>IA14:</b> ", signif(mapa$IA14, 5), "<br>")),
        paste(paste0("<b>Población:</b> ", mapa$POBLACION))
      )

      leaflet(mapa) %>%
        addProviderTiles("CartoDB.Positron",
          options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(fillColor = ~factpal(CASOSCAT), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, layerId = 1, popup = popup) %>%
        leaflet::addLegend("bottomright", colors = colors,
          labels = labels, values = values) %>%
        addTiles(urlTemplate = "http://atlasmortalidad.uclm.es", attribution = '| <a href = \"https://www.uclm.es\">Universidad de Castilla-La Mancha</a>')

})

    # Mapa de las TASAS
    output$leaflettasas <- renderLeaflet({
      semanaidx <- as.integer(input$semana) - 33
      mapa <- stfdf[, semanaidx]
      # reclasificar niveles
      mapa$TASACAT <- factor(mapa$TASACAT, levels(stfdf$TASACAT))
  
      # Título
      output$titulotasas <- renderUI(paste0("Tasas por 100.000 habitantes (IA7, Semana ", input$semana, ", ", dias[semanaidx], ")"))

      # Paleta de colores
      colors <- c("gray", brewer.pal(nlevels(stfdf$TASACAT) - 1, "Reds"))
      labels <- levels(stfdf$TASACAT)
  

      factpal <- colorFactor(colors, stfdf$TASACAT)

      # Pop-up
      popup <- paste0(
        paste("<b>Municipio:</b> ", mapa$MUNICIPIO, sep = ""), "<br>",
        paste(paste0("<b>Semana:</b> ", input$semana, "<br>")),
        paste(paste0("<b>Fecha:</b> ", dias[semanaidx], "<br>")),
        paste(paste0("<b>Casos:</b> ", mapa$CASOS, "<br>")),
        paste(paste0("<b>IA7:</b> ", signif(mapa$TASA, 5), "<br>")),
        paste(paste0("<b>IA14:</b> ", signif(mapa$IA14, 5), "<br>")),
        paste(paste0("<b>Población:</b> ", mapa$POBLACION))
      )

      leaflet(mapa) %>%
        addProviderTiles("CartoDB.Positron",
          options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(fillColor = ~factpal(TASACAT), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, layerId = 1, popup = popup) %>%
        leaflet::addLegend("bottomright", colors = colors,
          labels = labels, values = values) %>%
        addTiles(urlTemplate = "http://atlasmortalidad.uclm.es", attribution = '| <a href = \"https://www.uclm.es\">Universidad de Castilla-La Mancha</a>')

})


    # Mapa de las SERIES TEMPORALES
    output$leaflettseries <- renderLeaflet({
      semanaidx <- as.integer(input$semana) - 33
      mapa <- stfdf[, semanaidx]
      # reclasificar niveles
      mapa$TASACAT <- factor(mapa$TASACAT, levels(stfdf$TASACAT))
  
      # Título
      output$titulotseries <- renderUI(paste0("Tasas por 100.000 habitantes (IA7, Semana ", input$semana, ", ", dias[semanaidx], ")"))

      # Paleta de colores
      colors <- c("gray", brewer.pal(nlevels(stfdf$TASACAT) - 1, "Reds"))
      labels <- levels(stfdf$TASACAT)
  

      factpal <- colorFactor(colors, stfdf$TASACAT)

      # Pop-up
      popup <- paste0(
        paste("<b>Municipio:</b> ", mapa$MUNICIPIO, sep = ""), "<br>",
        paste(paste0("<b>Semana:</b> ", input$semana, "<br>")),
        paste(paste0("<b>Fecha:</b> ", dias[semanaidx], "<br>")),
        paste(paste0("<b>Casos:</b> ", mapa$CASOS, "<br>")),
        paste(paste0("<b>IA7:</b> ", signif(mapa$TASA, 5), "<br>")),
        paste(paste0("<b>IA14:</b> ", signif(mapa$IA14, 5), "<br>")),
        paste(paste0("<b>Población:</b> ", mapa$POBLACION))
      )

      leaflet() %>%
        addProviderTiles("CartoDB.Positron",
          options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(data = mapa, fillColor = ~factpal(TASACAT), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, layerId = 1, popup = tseries.popup) %>%
        leaflet::addLegend("bottomright", colors = colors,
          labels = labels, values = values) %>%
        addTiles(urlTemplate = "http://atlasmortalidad.uclm.es", attribution = '| <a href = \"https://www.uclm.es\">Universidad de Castilla-La Mancha</a>')

})


    # Mapa de los valores de Rt
    output$leafletRt <- renderLeaflet({
      semanaidx <- as.integer(input$semana) - 33
      mapa <- stfdf[, semanaidx]

      # Indicamos con este nivel donde no hay gráfica
      levels(stfdf$TASACAT)[1] <- "Sin gráfica"
      # reclasificar niveles
      mapa$TASACAT <- factor(mapa$TASACAT, levels(stfdf$TASACAT))

      # Mostrar solo los valores para los que hay datos
      mapa$TASACAT [!Rt.grafica] <- levels(mapa$TASACAT)[1] # Sin datos
  
      # Título
      output$tituloRt <- renderUI(paste0("Número reproductivo básico instantaneo (Tasas por 100.000 habitantes, IA7, Semana ", input$semana, ", ", dias[semanaidx], ")"))

      # Paleta de colores
      colors <- c("gray", brewer.pal(nlevels(stfdf$TASACAT) - 1, "Reds"))
      labels <- levels(stfdf$TASACAT)
  

      factpal <- colorFactor(colors, stfdf$TASACAT)

      # Pop-up
      popup <- paste0(
        paste("<b>Municipio:</b> ", mapa$MUNICIPIO, sep = ""), "<br>",
        paste(paste0("<b>Semana:</b> ", input$semana, "<br>")),
        paste(paste0("<b>Fecha:</b> ", dias[semanaidx], "<br>")),
        paste(paste0("<b>Casos:</b> ", mapa$CASOS, "<br>")),
        paste(paste0("<b>IA7:</b> ", signif(mapa$TASA, 5), "<br>")),
        paste(paste0("<b>IA14:</b> ", signif(mapa$IA14, 5), "<br>")),
        paste(paste0("<b>Población:</b> ", mapa$POBLACION))
      )

      leaflet() %>%
        addProviderTiles("CartoDB.Positron",
          options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(data = mapa, fillColor = ~factpal(TASACAT), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, layerId = 1, popup = Rt.popup) %>%
        leaflet::addLegend("bottomright", colors = colors,
          labels = labels, values = values) %>%
        addTiles(urlTemplate = "http://atlasmortalidad.uclm.es", attribution = '| <a href = \"https://www.uclm.es\">Universidad de Castilla-La Mancha</a>')

})

    # Exceso de mortalidad MoMo
    output$momoplot <- renderPlotly({

      # Título
      lab.sexo <- ifelse(input$sexomomo == "Todos", "Ambos sexos",
        input$sexomomo)
      lab.gedad <- ifelse(input$sexomomo == "Todos", "Todas las edades",
        input$gedadmomo)
      output$tituloMoMo <- renderUI(paste0("Exceso de mortalidad en Castilla-La Mancha (",
        lab.sexo, ", ", lab.gedad, ")")) 


      # Seleccionar datos
      momo.todos <- subset(momo, nombre_sexo == tolower(input$sexomomo) &
        nombre_gedad == tolower(input$gedadmomo))

      # Calcular exceso de mortalidad
      difOE <- momo.todos$defunciones_observadas - momo.todos$defunciones_esperadas

      # Exceso desde el 1 de marzo de 2020
      idx <- momo.todos$fecha_defuncion >= "2020-03-01"
      exceso01032020 <- sum(difOE[idx])

      # Exceso desde el 1 de julio de 2020
      idx <- momo.todos$fecha_defuncion >= "2020-07-01"
      exceso01072020 <- round(sum(difOE[idx]))

      # Exceso desde el 1 de enero de 2021
      idx <- momo.todos$fecha_defuncion >= "2021-01-01"
      exceso01012021 <- round(sum(difOE[idx]))


      output$exceso01032020 <- renderUI(paste0(("Exceso desde 01-03-2020: "), round(exceso01032020), " fallecidos."))
      output$exceso01072020 <- renderUI(paste0(("Exceso desde 01-07-2020: "), round(exceso01072020), " fallecidos."))
      output$exceso01012021 <- renderUI(paste0(("Exceso desde 01-01-2021: "), round(exceso01012021), " fallecidos."))


      momo.plot <- ggplot(momo.todos, aes(x = fecha_defuncion,
           y = defunciones_observadas)) +
        geom_line(colour = "#fb8072") +
        geom_ribbon(aes(ymin = defunciones_esperadas_q01,
          ymax = defunciones_esperadas_q99), fill = "gray", alpha = 0.7) +
        geom_line(aes(x = fecha_defuncion, y = defunciones_esperadas),
          colour = "#8dd3c7") +
        xlab("Fecha") +
        ylab("Defunciones")# +
        #annotation_custom(grobTree(textGrob("Defunciones observadas", x = 0.1, y = 0.95, hjust = 0, gp = gpar(col = "black"))))

ggplotly(momo.plot)

})


    # Exceso de mortalidad INE
    output$ineplot <- renderPlotly({

      # Título
      lab.provincia <- switch(input$provinciaine,
        "Todas" = "Castilla-La Mancha",
        "AB" = "Albacete",
        "CR" = "Ciudad Real",
        "CU" = "Cuenca",
        "GU" = "Guadalajara",
        "TO" = "Toledo"
      )
      lab.sexo <- switch(input$sexoine,
        Todos =  "Ambos sexos",
        Hombre = "Hombres",
        Mujer = "Mujeres"
      )
      lab.gedad <- ifelse(input$gedadine == "Todos", "Todas las edades",
        input$gedadine)
      output$tituloINE <- renderUI(paste0("Estimación de la mortalidad semanal (",
        lab.provincia, ", ", lab.sexo, ", ", lab.gedad, ")")) 


      # Seleccionar datos
      ine.todos <- INE_mortalidad
      # provincia
      if(input$provinciaine != "Todas") {
        ine.todos <- subset(ine.todos, Provincia == input$provinciaine)
      }
      # Sexo
      if(input$sexoine != "Todos") {
        ine.todos <- subset(ine.todos, sexo == input$sexoine)
      }
      # Edad
      if(input$gedadine != "Todos") {
          if(input$gedadine == "Edad < 65") {
            idx <- c("0-4", "10-14", "15-19", "20-24", "25-29", "30-34",
              "35-39", "40-44", "45-49", "5-9", "50-54", "55-59", "60-64")
          } else {
            if(input$gedadine == "Edad 65-74") {
              idx <- c("65-69", "70-74")
            } else {#"Edad > 75"
              idx <- c("75-79", "80-84", "85-89", "90+")
            }
          }
        #print(idx)

        ine.todos <- subset(ine.todos, gedad %in% idx)
      }

      # Agregar
      ine.agg <- aggregate(mortalidad ~ Semana + Anyo, data = ine.todos, FUN = sum)

      ine.plot <- ggplot() +
        geom_line(data = ine.agg, aes(x = Semana, y = mortalidad,
          color = Anyo)) +
        labs(colour = "Año") +
        ylab("Mortalidad") +
        scale_color_manual(values=c("#8dd3c7", "#fb8072", "#B3DE69"))

      ggplotly(ine.plot)

    })


    # Mapa del riesgo de rebrote
    output$leafletrebrote <- renderLeaflet({
      # Semana 
      semanaidx <- as.integer(input$semana) - 33

      # Seleccionar semana
      mapa <- stfdf[, semanaidx]
    
  
      # Título
      output$titulorebrote <- renderUI(paste0("Riesgo de rebrote (Semana ", input$semana, ", ", dias[semanaidx], ")"))

      # Paleta de colores (11-class RdYlGn)
      colors <- c("gray", brewer.pal(11, "RdYlGn")[c(7, 6, 4, 2)])
      labels <- levels(stfdf$REBROTE)


      factpal <- colorFactor(colors, stfdf$REBROTE)

      # Pop-up
      popup <- paste0(
        paste("<b>Municipio:</b> ", mapa$MUNICIPIO, sep = ""), "<br>",
        paste(paste0("<b>Semana:</b> ", input$semana, "<br>")),
        paste(paste0("<b>Fecha:</b> ", dias[semanaidx], "<br>")),
        paste(paste0("<b>Casos:</b> ", mapa$CASOS, "<br>")),
        paste(paste0("<b>IA7:</b> ", signif(mapa$TASA, 5), "<br>")),
        paste(paste0("<b>IA14:</b> ", signif(mapa$IA14, 5), "<br>")),
        paste(paste0("<b>Población:</b> ", mapa$POBLACION, "<br>")),
        paste(paste0("<b>Rebrote:</b> ", mapa$REBROTE))

      )

      leaflet(mapa) %>%
        addProviderTiles("CartoDB.Positron",
          options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(fillColor = ~factpal(REBROTE), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, layerId = 1, popup = popup) %>%
        leaflet::addLegend("bottomright", colors = colors,
          labels = labels, values = values) %>%
        addTiles(urlTemplate = "http://atlasmortalidad.uclm.es", attribution = '| <a href = \"https://www.uclm.es\">Universidad de Castilla-La Mancha</a>')

})
    


    # Mapa del UMBRAL de confinamiento
    output$leafletumbral <- renderLeaflet({
      # reclasificar municipios según el umbral con IA14
      #stfdf$UMBRALCAT <- as.character(stfdf$TASA > input$sliderumbral)
      stfdf$UMBRALCAT <- as.character(stfdf$IA14 > input$sliderumbral)
      # Gestionar NA's y tal
      stfdf$UMBRALCAT[is.na(stfdf$UMBRALCAT)] <- "Sin datos"
      stfdf$UMBRALCAT <- factor(stfdf$UMBRALCAT)
      stfdf$UMBRALCAT <- factor(stfdf$UMBRALCAT, c("Sin datos", "FALSE", "TRUE"))      
      levels(stfdf$UMBRALCAT) <- c("Sin datos", "No confinado", "Confinado")

      # Semana 
      semanaidx <- as.integer(input$semana) - 33

      # Seleccionar semana
      mapa <- stfdf[, semanaidx]
    
  
      # Título
      output$tituloumbral <- renderUI(paste0("Municipios confinados (IA14 > ", input$sliderumbral, ", Semana ", input$semana, ", ", dias[semanaidx], ")"))

      # Paleta de colores
      colors <- c("gray", "white", "red")
      labels <- levels(stfdf$UMBRALCAT)


      factpal <- colorFactor(colors, stfdf$UMBRALCAT)

      # Pop-up
      popup <- paste0(
        paste("<b>Municipio:</b> ", mapa$MUNICIPIO, sep = ""), "<br>",
        paste(paste0("<b>Semana:</b> ", input$semana, "<br>")),
        paste(paste0("<b>Fecha:</b> ", dias[semanaidx], "<br>")),
        paste(paste0("<b>Casos:</b> ", mapa$CASOS, "<br>")),
        paste(paste0("<b>IA7:</b> ", signif(mapa$TASA, 5), "<br>")),
        paste(paste0("<b>IA14:</b> ", signif(mapa$IA14, 5), "<br>")),
        paste(paste0("<b>Población:</b> ", mapa$POBLACION, "<br>")),
        paste(paste0("<b>Confinamiento:</b> ", mapa$UMBRALCAT))

      )

      leaflet(mapa) %>%
        addProviderTiles("CartoDB.Positron",
          options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(fillColor = ~factpal(UMBRALCAT), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, layerId = 1, popup = popup) %>%
        leaflet::addLegend("bottomright", colors = colors,
          labels = labels, values = values) %>%
        addTiles(urlTemplate = "http://atlasmortalidad.uclm.es", attribution = '| <a href = \"https://www.uclm.es\">Universidad de Castilla-La Mancha</a>')

})

    output$leaflettemporalcasos <- renderLeaflet({

      # Índice de la semana
      semanaidx <- as.integer(input$slidersemanacasos) - 33
      # Mapa
      mapa <- stfdf[, semanaidx]
      # reclasificar niveles
      mapa$CASOSCAT <- factor(mapa$CASOSCAT, levels(stfdf$CASOSCAT))
  
      # Título
      output$titulotemporalcasos <- renderUI(paste0("Casos totales (Semana ", input$slidersemanacasos, ", ", dias[semanaidx], ")"))


      # Paleta de colores
      colors <- c("gray", brewer.pal(nlevels(stfdf$CASOSCAT) - 1, "Reds"))
      labels <- levels(stfdf$CASOSCAT)
      factpal <- colorFactor(colors, stfdf$CASOSCAT)

      # Pop-up
      popup <- paste0(
        paste("<b>Municipio:</b> ", mapa$MUNICIPIO, sep = ""), "<br>",
        paste(paste0("<b>Semana:</b> ", input$semana, "<br>")),
        paste(paste0("<b>Fecha:</b> ", dias[semanaidx], "<br>")),
        paste(paste0("<b>Casos:</b> ", mapa$CASOS, "<br>")),
        paste(paste0("<b>IA7:</b> ", signif(mapa$TASA, 5), "<br>")),
        paste(paste0("<b>IA14:</b> ", signif(mapa$IA14, 5), "<br>")),
        paste(paste0("<b>Población:</b> ", mapa$POBLACION))
      )

      leaflet(mapa) %>%
        addProviderTiles("CartoDB.Positron",
          options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(fillColor = ~factpal(CASOSCAT), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, layerId = 1, popup = popup) %>%
        leaflet::addLegend("bottomright", colors = colors,
          labels = labels, values = values) %>%
        addTiles(urlTemplate = "http://atlasmortalidad.uclm.es", attribution = '| <a href = \"https://www.uclm.es\">Universidad de Castilla-La Mancha</a>')

})

    # Mapa TEMPORAL de las TASAS
    output$leaflettemporaltasas <- renderLeaflet({
      semanaidx <- as.integer(input$slidersemanatasas) - 33
      mapa <- stfdf[, semanaidx]
      # reclasificar niveles
      mapa$TASACAT <- factor(mapa$TASACAT, levels(stfdf$TASACAT))
  
      # Título
      output$titulotemporaltasas <- renderUI(paste0("Tasas por 100.000 habitantes (IA7, Semana ", input$slidersemanatasas, ", ", dias[semanaidx], ")"))

      # Paleta de colores
      colors <- c("gray", brewer.pal(nlevels(stfdf$TASACAT) - 1, "Reds"))
      labels <- levels(stfdf$TASACAT)

      factpal <- colorFactor(colors, stfdf$TASACAT)

      # Pop-up
      popup <- paste0(
        paste("<b>Municipio:</b> ", mapa$MUNICIPIO, sep = ""), "<br>",
        paste(paste0("<b>Semana:</b> ", input$semana, "<br>")),
        paste(paste0("<b>Fecha:</b> ", dias[semanaidx], "<br>")),
        paste(paste0("<b>Casos:</b> ", mapa$CASOS, "<br>")),
        paste(paste0("<b>IA7:</b> ", signif(mapa$TASA, 5), "<br>")),
        paste(paste0("<b>IA14:</b> ", signif(mapa$IA14, 5), "<br>")),
        paste(paste0("<b>Población:</b> ", mapa$POBLACION))
      )

      leaflet(mapa) %>%
        addProviderTiles("CartoDB.Positron",
          options = providerTileOptions(opacity = 0.99)) %>%
        addPolygons(fillColor = ~factpal(TASACAT), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, layerId = 1, popup = popup) %>%
        leaflet::addLegend("bottomright", colors = colors,
          labels = labels, values = values) %>%
        addTiles(urlTemplate = "http://atlasmortalidad.uclm.es", attribution = '| <a href = \"https://www.uclm.es\">Universidad de Castilla-La Mancha</a>')

})



# Observatorio COVID19

output$obscovid19ab <- renderUI({
    web <- "https://obscovidalba.com/boletines-informativos-2/"
    tags$iframe(src = web, width = "100%", height = "100%", #height = 600, width = 535, 
      frameborder = "no", id = "ABOBSiframe")
  })

}


