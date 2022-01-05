#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(rio)
library(sf)
library(xts)

setwd("~/GitHub/ODS")

ODSSpr = import("https://github.com/ChiaraZamoraM/ODS/raw/main/ODSSpr.RDS")

ui <- fluidPage(

    # Application title
    titlePanel("Avance de los Objetivos de Desarollo Sostenible"),

    sidebarLayout(
        sidebarPanel(
            tags$a(href="http://ods.inei.gob.pe/ods/objetivos-de-desarrollo-sostenible", 
                   "Referencia de los datos",
                   target= "_blank"),
                   h5("Este es un Shiny App de prueba."),
            selectInput("ano",
                        "Seleccione una fecha",
                        choices = unique(ODSSpr$Ano)
            )
            ),
        mainPanel(
            tabsetPanel(
                tabPanel("1.1.1",leafletOutput("pobrezaex", height = 650,width=605)),
                tabPanel("1.2.1",leafletOutput("pobrezatot", height = 650,width=605)),
                tabPanel("1.3.1",leafletOutput("pension", height = 650,width=605)),
                tabPanel("1.4.1",leafletOutput("servbasicos", height = 650,width=605))
                )
            )
        )
)
        
# Define server logic required to draw a histogram

server <- function(input, output) {

     ano_ODSSpr <- reactive({
        y    <- ODSSpr %>% filter(Ano == input$ano)
        return(y)
     })
     
     output$pobrezaex = renderLeaflet({
         pal1 = colorNumeric(palette = "Blues", domain = ODSSpr$ODS1_1_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(label= paste(ano_ODSSpr()$DEPARTAMEN,':', ano_ODSSpr()$ODS1_1_1),
                         stroke = FALSE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal1(ano_ODSSpr()$ODS1_1_1),
                         highlightOptions = highlightOptions(weight = 5,
                                                             fillOpacity= 1,
                                                             color = "black",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             leaflet::addLegend("bottomright",
                       pal = pal1,
                       values = ~ODS1_1_1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) %>%
             addControl("Incidencia de la pobreza extrema", 
                        position = "bottomleft", className="map-title")
     })
     output$pobrezatot = renderLeaflet({
         pal2 = colorNumeric(palette = "Blues", domain = ODSSpr$ODS1_2_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(label= ano_ODSSpr()$DEPARTAMEN,
                         stroke = FALSE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal2(ano_ODSSpr()$ODS1_2_1),
                         highlightOptions = highlightOptions(weight = 5,
                                                             fillOpacity= 1,
                                                             color = "black",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             addLegend("bottomright",
                       pal = pal2,
                       values = ~ODS1_2_1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
     output$pension = renderLeaflet({
         pal3 = colorNumeric(palette = "Blues", domain = ODSSpr$ODS1_3_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(label= ano_ODSSpr()$DEPARTAMEN,
                         stroke = FALSE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal3(ano_ODSSpr()$ODS1_3_1),
                         highlightOptions = highlightOptions(weight = 5,
                                                             fillOpacity= 1,
                                                             color = "black",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             addLegend("bottomright",
                       pal = pal3,
                       values = ~ODS1_3_1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
     output$servbasicos = renderLeaflet({
         pal4 = colorNumeric(palette = "Blues", domain = ODSSpr$ODS1_4_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(label= ano_ODSSpr()$DEPARTAMEN,
                         stroke = FALSE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal4(ano_ODSSpr()$ODS1_4_1),
                         highlightOptions = highlightOptions(weight = 5,
                                                             fillOpacity= 1,
                                                             color = "black",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             addLegend("bottomright",
                       pal = pal4,
                       values = ~ODS1_4_1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
