#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(rio)
library(sf)
library(xts)
library(crosstalk)
library(DT)
library(shinyWidgets)
library(RColorBrewer)

ODSSpr = import("https://github.com/ChiaraZamoraM/ODS/raw/main/ODSSpr.RDS")

download.file("https://github.com/ChiaraZamoraM/ODS/raw/main/mapa2_Peru/Provincias_Peru.zip", 
              destfile = "Provincias_Peru.zip" , mode='wb')
unzip("Provincias_Peru.zip", exdir = ".")
file.remove("Provincias_Peru.zip")
mapa_prov <- st_read("PROVINCIAS.shp")
mapa_prov$DEPARTAMEN <- ifelse(mapa_prov$PROVINCIA == "LIMA", "LIMA METROPOLITANA", mapa_prov$DEPARTAMEN)
mapa_prov$DEPARTAMEN <- ifelse(mapa_prov$PROVINCIA == "LIMA", "LIMA PROVINCIAS", mapa_prov$DEPARTAMEN)
mapa_prov$DEPARTAMEN <- ifelse(mapa_prov$PROVINCIA == "LIMA METROPOLITANA", "LIMA", mapa_prov$DEPARTAMEN)

subtitulo = {'background-color: #34282C; 
            color: white;
            font-size: 20px;
            font-weight: bold;
            text-align: center;
            padding-top: 15px;
            padding-bottom: 15px;'}

ui <-  dashboardPage(
    dashboardHeader(title = "Observatorio EGP"),
    
    
     dashboardSidebar(
        h5("Este es un Shiny App de prueba."),
        selectInput("ano",
                    "Seleccione una fecha",
                    choices = unique(ODSSpr$Ano)
            ),
        tags$a(href="http://ods.inei.gob.pe/ods/objetivos-de-desarrollo-sostenible",
               "Referencia de los datos",
               target= "_blank")
        ),
        
    dashboardBody(tags$head(tags$style(HTML('
      .skin-blue .main-header .logo {
        font-family: "Source Sans Pro",sans-serif;
        font-weight: bold;
      }
      .skin-blue .main-header .logo:hover {
          background-color: #34282C;
        }
      '))),
        tabsetPanel(
            tabPanel(h4("ODS 1: Fin de la pobreza"),
            tabsetPanel(
                tabPanel("1.1.1",
                         titlePanel(h2("Indicador 1.1.1. Incidencia de la pobreza extrema",
                                       style= subtitulo)
                         ),
                         fluidRow(column(6,leafletOutput("pobrezaex", height = 650)),
                                  column(6,dataTableOutput("table1_1_1", height = 650)))),
                tabPanel("1.2.1",
                         titlePanel(h2("Indicador 1.2.1. Incidencia de la pobreza total",
                                       style= subtitulo)
                         ),
                         fluidRow(column(6,leafletOutput("pobrezatot", height = 650)),
                                  column(6,dataTableOutput("table1_2_1", height = 650)))),
                tabPanel("1.3.1",
                         titlePanel(h2("Indicador 1.3.1. Población de 14 a más con seguro de pensión",
                                       style= subtitulo)
                         ),
                         fluidRow(column(6,leafletOutput("pension", height = 650)),
                                  column(6,dataTableOutput("table1_3_1", height = 650)))),
                tabPanel("1.4.1",
                         titlePanel(h2("Indicador 1.4.1. Población en hogares con acceso a servicios básicos de infraestructura",
                                       style= subtitulo)
                         ),
                         fluidRow(column(6,leafletOutput("servbasicos", height = 650)),
                                  column(6,dataTableOutput("table1_4_1", height = 650))))
                )
            ),
            tabPanel(h4("ODS 2: Hambre Cero"),
                     tabsetPanel(
                         tabPanel("2.2.1",
                                  titlePanel(h2("Indicador 2.2.1. Tasa de desnutrición crónica entre las niñas y niños menores de 5 años",
                                                style= subtitulo)
                                  ),
                                  fluidRow(column(6,leafletOutput("desnutricronica", height = 650)),
                                           column(6,dataTableOutput("table2_2_1", height = 650)))),
                         tabPanel("2.2.2",
                                  titlePanel(h2("Indicador 2.2.2. Tasa de desnutrición aguda entre las niñas y niños menores de 5 años",
                                                style= subtitulo)
                                  ),
                                  fluidRow(column(6,leafletOutput("desnutriaguda", height = 650)),
                                           column(6,dataTableOutput("table2_2_2", height = 650))))
                     )
            )
        )
))
        
# Define server logic required to draw a histogram

server <- function(input, output) {

     ano_ODSSpr <- reactive({
        y    <- ODSSpr %>% filter(Ano == input$ano)
        return(y)
     })
     
     output$pobrezaex = renderLeaflet({
         pal1 = colorNumeric(palette = "Reds", domain = ODSSpr$ODS1_1_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(data = mapa_prov, 
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0,
                         weight = 0.5,
                         color= "black") %>%
             addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS1_1_1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         color= "grey",
                         weight = 0.5,
                         fillColor = ~pal1(ano_ODSSpr()$ODS1_1_1),
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE)) %>%
             leaflet::addLegend("bottomright",
                       pal = pal1,
                       values = ~ODS1_1_1,
                       title= "Porcentaje (%)",
                       opacity= 0.7)
     })
     
     output$table1_1_1 = renderDT({
         datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS1_1_1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1_1_1'),
                   filter = 'top',
                   options = list(pageLength = 13
                                  ))})
     
     
     output$pobrezatot = renderLeaflet({
         pal2 = colorNumeric(palette = "Reds", domain = ODSSpr$ODS1_2_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron")%>%
             addPolygons(data = mapa_prov, 
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0,
                         weight = 0.5,
                         color= "black") %>%
             addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS1_2_1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         color= "grey",
                         weight = 0.5,
                         fillColor = ~pal2(ano_ODSSpr()$ODS1_2_1),
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             leaflet::addLegend("bottomright",
                       pal = pal2,
                       values = ~ODS1_2_1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
     output$table1_2_1 = renderDT({
         datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS1_2_1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1_2_1'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     output$pension = renderLeaflet({
         pal3 = colorNumeric(palette = "Reds", domain = ODSSpr$ODS1_3_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(data = mapa_prov, 
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0,
                         weight = 0.5,
                         color= "black")%>%
             addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS1_3_1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal3(ano_ODSSpr()$ODS1_3_1),
                         color="grey",
                         weight = 0.5,
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             leaflet::addLegend("bottomright",
                       pal = pal3,
                       values = ~ODS1_3_1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
     output$table1_3_1 = renderDT({
         datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS1_4_1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1_4_1'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     output$servbasicos = renderLeaflet({
         pal4 = colorNumeric(palette = "Reds", domain = ODSSpr$ODS1_4_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(data = mapa_prov, 
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0,
                         weight = 0.5,
                         color= "black")%>%
             addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS1_4_1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal4(ano_ODSSpr()$ODS1_4_1),
                         color= "grey",
                         weight = 0.5,
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "black",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             leaflet::addLegend("bottomright",
                       pal = pal4,
                       values = ~ODS1_4_1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
     output$table1_4_1 = renderDT({
         datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS1_4_1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1_4_1'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     output$desnutricronica = renderLeaflet({
         pal5 = colorNumeric(palette = "YlOrBr", domain = ODSSpr$ODS2_2_1)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(data = mapa_prov, 
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0,
                         weight = 0.5,
                         color= "black") %>%
             addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS2_2_1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         color= "grey",
                         weight = 0.5,
                         fillColor = ~pal5(ano_ODSSpr()$ODS2_2_1),
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE)) %>%
             leaflet::addLegend("bottomright",
                                pal = pal5,
                                values = ~ODS2_2_1,
                                title= "Porcentaje (%)",
                                opacity= 0.7)
     })
     
     output$table2_2_1 = renderDT({
         datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS2_2_1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS2_2_1'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     output$desnutriaguda = renderLeaflet({
         pal5 = colorNumeric(palette = "YlOrBr", domain = ODSSpr$ODS2_2_2)
         
         ano_ODSSpr()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(data = mapa_prov, 
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0,
                         weight = 0.5,
                         color= "black") %>%
             addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS2_2_2,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         color= "grey",
                         weight = 0.5,
                         fillColor = ~pal5(ano_ODSSpr()$ODS2_2_2),
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE)) %>%
             leaflet::addLegend("bottomright",
                                pal = pal5,
                                values = ~ODS2_2_2,
                                title= "Porcentaje (%)",
                                opacity= 0.7)
     })
     
     output$table2_2_2 = renderDT({
         datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS2_2_2")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS2_2_2'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
}

# Run the application 
shinyApp(ui = ui, server = server)
