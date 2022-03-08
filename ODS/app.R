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
library(stringi)

ODSShiny = import("https://github.com/ChiaraZamoraM/ODS/raw/main/ODSShiny.RDS")

#ODSShiny2 = import("https://github.com/ChiaraZamoraM/ODS/raw/main/ODSShinySinGeom.RDS")

#ODSShiny$DEPARTAMENTO= gsub("Provincias de Lima","LIMA PROVINCIAS",ODSShiny$DEPARTAMENTO)

download.file("https://github.com/ChiaraZamoraM/ODS/raw/main/Departamentos_Peru/mapa_depas_Lima.zip", 
             destfile = "mapa_depas_Lima.zip" , mode='wb')
unzip("mapa_depas_Lima.zip", exdir = ".")
file.remove("mapa_depas_Lima.zip")

mapa <- st_read("mapa_depas_Lima.shp")

#ODSShiny$DEPARTAMENTO= stri_trans_general(str = toupper(ODSShiny$DEPARTAMENTO), id = "Latin-ASCII")

ODSShiny = merge(mapa, ODSShiny,
                by.x='DEPARTAMEN',by.y="DEPARTAMENTO")


#download.file("https://github.com/ChiaraZamoraM/ODS/raw/main/Provincias_Peru/Provincias_Peru.zip", 
 #             destfile = "Provincias_Peru.zip" , mode='wb')
#unzip("Provincias_Peru.zip", exdir = ".")
#file.remove("Provincias_Peru.zip")
#mapa_prov <- st_read("PROVINCIAS.shp")
#mapa_prov$DEPARTAMEN <- ifelse(mapa_prov$PROVINCIA == "LIMA", "LIMA METROPOLITANA", mapa_prov$DEPARTAMEN)
#mapa_prov$DEPARTAMEN <- ifelse(mapa_prov$PROVINCIA == "LIMA", "LIMA PROVINCIAS", mapa_prov$DEPARTAMEN)
#mapa_prov$DEPARTAMEN <- ifelse(mapa_prov$PROVINCIA == "LIMA METROPOLITANA", "LIMA", mapa_prov$DEPARTAMEN)

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
        h5("
           Este es un Shiny App de prueba.
           
           "),
        selectInput("ano",
                    "Seleccione una fecha",
                    choices = unique(ODSShiny$Ano)
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
                                  column(6,dataTableOutput("table1.2.1", height = 650)))),
                tabPanel("1.3.1",
                         titlePanel(h2("Indicador 1.3.1. Población de 14 a más con seguro de pensión",
                                       style= subtitulo)
                         ),
                         fluidRow(column(6,leafletOutput("pension", height = 650)),
                                  column(6,dataTableOutput("table1.3.1", height = 650)))),
                tabPanel("1.4.1",
                         titlePanel(h2("Indicador 1.4.1. Población en hogares con acceso a servicios básicos de infraestructura",
                                       style= subtitulo)
                         ),
                         fluidRow(column(6,leafletOutput("servbasicos", height = 650)),
                                  column(6,dataTableOutput("table1.4.1", height = 650))))
                )
            ),
            tabPanel(h4("ODS 2: Hambre Cero"),
                     tabsetPanel(
                         tabPanel("2.2.1",
                                  titlePanel(h2("Indicador 2.2.1. Tasa de desnutrición crónica entre las niñas y niños menores de 5 años (OMS)",
                                                style= subtitulo)
                                  ),
                                  fluidRow(column(6,leafletOutput("desnutricronica", height = 650)),
                                           column(6,dataTableOutput("table2.2.1", height = 650)))),
                         tabPanel("2.2.2",
                                  titlePanel(h2("Indicador 2.2.2. Tasa de desnutrición aguda entre las niñas y niños menores de 5 años",
                                                style= subtitulo)
                                  ),
                                  fluidRow(column(6,leafletOutput("desnutriaguda", height = 650)),
                                           column(6,dataTableOutput("table2.2.2", height = 650))))
                     )
            ),
            tabPanel(h4("ODS 3: Salud y Bienestar"),
                     tabsetPanel(
                       tabPanel("3.1.2",
                                titlePanel(h2("Indicador 3.1.2. Proporción de partos atendidos por personal sanitario especializado",
                                              style= subtitulo)
                                ),
                                fluidRow(column(6,leafletOutput("partos", height = 650)),
                                         column(6,dataTableOutput("table3.1.2", height = 650)))),
                       tabPanel("3.2.1",
                                titlePanel(h2("Indicador 3.2.1. Tasa de mortalidad de niños menores de 5 años de edad",
                                              style= subtitulo)
                                ),
                                fluidRow(column(6,leafletOutput("mortalidad", height = 650)),
                                         column(6,dataTableOutput("table3.2.1", height = 650)))),
                       tabPanel("3.2.2",
                                titlePanel(h2("Indicador 3.2.2. Tasa de mortalidad neonatal",
                                              style= subtitulo)
                                ),
                                fluidRow(column(6,leafletOutput("mortalidadneo", height = 650)),
                                         column(6,dataTableOutput("table3.2.2", height = 650)))),
                       tabPanel("3.3.1",
                                titlePanel(h2("Indicador 3.3.1. Tasa de notificación de casos del Virus de la Inmunodeficiencia Humana (VIH) por cada 100 mil habitantes",
                                              style= subtitulo)
                                ),
                                fluidRow(column(6,leafletOutput("VIH", height = 650)),
                                         column(6,dataTableOutput("table3.3.1", height = 650)))),
                       tabPanel("3.3.2",
                                titlePanel(h2("Indicador 3.3.2. Incidencia de la tuberculosis por cada 100 mil habitantes",
                                              style= subtitulo)
                                ),
                                fluidRow(column(6,leafletOutput("tuberculosis", height = 650)),
                                         column(6,dataTableOutput("table3.3.2", height = 650)))),
                       tabPanel("3.3.3",
                                titlePanel(h2("Indicador 3.3.3. Incidencia de la malaria por cada 100 mil habitantes",
                                              style= subtitulo)
                                ),
                                fluidRow(column(6,leafletOutput("malaria", height = 650)),
                                         column(6,dataTableOutput("table3.3.3", height = 650)))),
                       tabPanel("3.3.4",
                                titlePanel(h2("Indicador 3.3.4. Incidencia de la hepatitis B por cada 100 mil habitantes",
                                              style= subtitulo)
                                ),
                                fluidRow(column(6,leafletOutput("hepatitis", height = 650)),
                                         column(6,dataTableOutput("table3.3.4", height = 650))))
                     )
            )
        )
))
        
# Define server logic required to draw a histogram

server <- function(input, output) {

     ano_ODSShiny <- reactive({
        y    <- ODSShiny %>% filter(Ano == input$ano)
        return(y)
     })
     
     output$pobrezaex = renderLeaflet({
         pal1 = colorNumeric(palette = "Reds", domain = c(0,100))
         
         ano_ODSShiny()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
            # addPolygons(data = mapa_prov, 
             #            stroke = TRUE, 
              #           smoothFactor =  .5,
               #          opacity = 1,
                #         fillOpacity = 0,
                 #        weight = 0.5,
                  #       color= "black") %>%
             addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS1.1.1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         color= "grey",
                         weight = 0.5,
                         fillColor = ~pal1(ano_ODSShiny()$ODS1.1.1),
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE)) %>%
             leaflet::addLegend("bottomright",
                       pal = pal1,
                       values = ~ODS1.1.1,
                       title= "Porcentaje (%)",
                       opacity= 0.7)
     })
     
     output$table1_1_1 = renderDT({
         datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS1.1.1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1.1.1'),
                   filter = 'top',
                   options = list(pageLength = 13
                                  ))})
     
     
     output$pobrezatot = renderLeaflet({
         pal2 = colorNumeric(palette = "Reds", domain = c(0,100))
         
         ano_ODSShiny()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron")%>%
           #  addPolygons(data = mapa_prov, 
            #             stroke = TRUE, 
             #            smoothFactor =  .5,
              #           opacity = 1,
               #          fillOpacity = 0,
                #         weight = 0.5,
                 #        color= "black") %>%
             addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS1.2.1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         color= "grey",
                         weight = 0.5,
                         fillColor = ~pal2(ano_ODSShiny()$ODS1.2.1),
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             leaflet::addLegend("bottomright",
                       pal = pal2,
                       values = ~ODS1.2.1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
     output$table1.2.1 = renderDT({
         datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS1.2.1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1.2.1'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     output$pension = renderLeaflet({
         pal3 = colorNumeric(palette = "Reds", domain = c(0,100))
         
         ano_ODSShiny()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
            # addPolygons(data = mapa_prov, 
             #            stroke = TRUE, 
              #           smoothFactor =  .5,
               #          opacity = 1,
                #         fillOpacity = 0,
                 #        weight = 0.5,
                  #       color= "black")%>%
             addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS1.3.1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal3(ano_ODSShiny()$ODS1.3.1),
                         color="grey",
                         weight = 0.5,
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             leaflet::addLegend("bottomright",
                       pal = pal3,
                       values = ~ODS1.3.1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
     output$table1.3.1 = renderDT({
         datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS1.4.1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1.4.1'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     output$servbasicos = renderLeaflet({
         pal4 = colorNumeric(palette = "Reds", domain = c(0,100))
         
         ano_ODSShiny()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
            # addPolygons(data = mapa_prov, 
             #            stroke = TRUE, 
              #           smoothFactor =  .5,
               #          opacity = 1,
                #         fillOpacity = 0,
                 #        weight = 0.5,
                  #       color= "black")%>%
             addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS1.4.1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal4(ano_ODSShiny()$ODS1.4.1),
                         color= "grey",
                         weight = 0.5,
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "black",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             leaflet::addLegend("bottomright",
                       pal = pal4,
                       values = ~ODS1.4.1,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
     output$table1.4.1 = renderDT({
         datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS1.4.1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1.4.1'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     output$desnutricronica = renderLeaflet({
         pal5 = colorNumeric(palette = "YlOrBr", domain = c(0,100))
         
         ano_ODSShiny()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
           #  addPolygons(data = mapa_prov, 
            #             stroke = TRUE, 
             #            smoothFactor =  .5,
              #           opacity = 1,
               #          fillOpacity = 0,
                #         weight = 0.5,
                 #        color= "black") %>%
             addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS2.2.1,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         color= "grey",
                         weight = 0.5,
                         fillColor = ~pal5(ano_ODSShiny()$ODS2.2.1),
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE)) %>%
             leaflet::addLegend("bottomright",
                                pal = pal5,
                                values = ~ODS2.2.1,
                                title= "Porcentaje (%)",
                                opacity= 0.7)
     })
     
     output$table2.2.1 = renderDT({
         datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS2.2.1")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS2.2.1'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     output$desnutriaguda = renderLeaflet({
         pal5 = colorNumeric(palette = "YlOrBr", domain = c(0,100))
         
         ano_ODSShiny()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
          #   addPolygons(data = mapa_prov, 
           #              stroke = TRUE, 
            #             smoothFactor =  .5,
             #            opacity = 1,
              #           fillOpacity = 0,
               #          weight = 0.5,
                #         color= "black") %>%
             addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS2.2.2,"%"),
                         stroke = TRUE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         color= "grey",
                         weight = 0.5,
                         fillColor = ~pal5(ano_ODSShiny()$ODS2.2.2),
                         highlightOptions = highlightOptions(weight = 2,
                                                             fillOpacity= 1,
                                                             color = "grey",
                                                             opacity = 1,
                                                             bringToFront = TRUE)) %>%
             leaflet::addLegend("bottomright",
                                pal = pal5,
                                values = ~ODS2.2.2,
                                title= "Porcentaje (%)",
                                opacity= 0.7)
     })
     
     output$table2.2.2 = renderDT({
         datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS2.2.2")], 
                   colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS2.2.2'),
                   filter = 'top',
                   options = list(pageLength = 13
                   ))})
     
     
     output$partos = renderLeaflet({
       pal5 = colorNumeric(palette = "YlOrBr", domain = c(0,100))
       
       ano_ODSShiny()  %>% 
         st_transform(crs= "+init=epsg:4326") %>%
         leaflet() %>%
         addProviderTiles(provider= "CartoDB.Positron") %>%
   #      addPolygons(data = mapa_prov, 
    #                 stroke = TRUE, 
     #                smoothFactor =  .5,
      #               opacity = 1,
       #              fillOpacity = 0,
        #             weight = 0.5,
         #            color= "black") %>%
         addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS3.1.2,"%"),
                     stroke = TRUE, 
                     smoothFactor =  .5,
                     opacity = 1,
                     fillOpacity = 0.7,
                     color= "grey",
                     weight = 0.5,
                     fillColor = ~pal5(ano_ODSShiny()$ODS3.1.2),
                     highlightOptions = highlightOptions(weight = 2,
                                                         fillOpacity= 1,
                                                         color = "grey",
                                                         opacity = 1,
                                                         bringToFront = TRUE)) %>%
         leaflet::addLegend("bottomright",
                            pal = pal5,
                            values = ~ODS3.1.2,
                            title= "Porcentaje (%)",
                            opacity= 0.7)
     })
     
     output$table3.1.2 = renderDT({
       datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS3.1.2")], 
                 colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS3.1.2'),
                 filter = 'top',
                 options = list(pageLength = 13
                 ))})
     
     
     output$mortalidadneo = renderLeaflet({
       pal5 = colorNumeric(palette = "YlOrBr", domain = c(0,100))
       
       ano_ODSShiny()  %>% 
         st_transform(crs= "+init=epsg:4326") %>%
         leaflet() %>%
         addProviderTiles(provider= "CartoDB.Positron") %>%
       #  addPolygons(data = mapa_prov, 
        #             stroke = TRUE, 
         #            smoothFactor =  .5,
          #           opacity = 1,
           #          fillOpacity = 0,
            #         weight = 0.5,
             #        color= "black") %>%
         addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS3.2.2,"%"),
                     stroke = TRUE, 
                     smoothFactor =  .5,
                     opacity = 1,
                     fillOpacity = 0.7,
                     color= "grey",
                     weight = 0.5,
                     fillColor = ~pal5(ano_ODSShiny()$ODS3.2.2),
                     highlightOptions = highlightOptions(weight = 2,
                                                         fillOpacity= 1,
                                                         color = "grey",
                                                         opacity = 1,
                                                         bringToFront = TRUE)) %>%
         leaflet::addLegend("bottomright",
                            pal = pal5,
                            values = ~ODS3.2.2,
                            title= "Porcentaje (%)",
                            opacity= 0.7)
     })
     
     output$table3.2.2 = renderDT({
       datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS3.2.2")], 
                 colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS3.2.2'),
                 filter = 'top',
                 options = list(pageLength = 13
                 ))})
     
     
     output$VIH = renderLeaflet({
       pal5 = colorNumeric(palette = "YlOrBr", domain = c(0,100))
       
       ano_ODSShiny()  %>% 
         st_transform(crs= "+init=epsg:4326") %>%
         leaflet() %>%
         addProviderTiles(provider= "CartoDB.Positron") %>%
      #   addPolygons(data = mapa_prov, 
       #              stroke = TRUE, 
        #             smoothFactor =  .5,
         #            opacity = 1,
          #           fillOpacity = 0,
           #          weight = 0.5,
            #         color= "black") %>%
         addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS3.3.1,"%"),
                     stroke = TRUE, 
                     smoothFactor =  .5,
                     opacity = 1,
                     fillOpacity = 0.7,
                     color= "grey",
                     weight = 0.5,
                     fillColor = ~pal5(ano_ODSShiny()$ODS3.3.1),
                     highlightOptions = highlightOptions(weight = 2,
                                                         fillOpacity= 1,
                                                         color = "grey",
                                                         opacity = 1,
                                                         bringToFront = TRUE)) %>%
         leaflet::addLegend("bottomright",
                            pal = pal5,
                            values = ~ODS3.3.1,
                            title= "Porcentaje (%)",
                            opacity= 0.7)
     })
     
     output$table3.3.1 = renderDT({
       datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS3.3.1")], 
                 colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS3.3.1'),
                 filter = 'top',
                 options = list(pageLength = 13
                 ))})
     
     
     output$tuberculosis = renderLeaflet({
       pal5 = colorNumeric(palette = "YlOrBr", domain = c(0,100))
       
       ano_ODSShiny()  %>% 
         st_transform(crs= "+init=epsg:4326") %>%
         leaflet() %>%
         addProviderTiles(provider= "CartoDB.Positron") %>%
       #  addPolygons(data = mapa_prov, 
        #             stroke = TRUE, 
         #            smoothFactor =  .5,
          #           opacity = 1,
           #          fillOpacity = 0,
            #         weight = 0.5,
             #        color= "black") %>%
         addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS3.3.2,"%"),
                     stroke = TRUE, 
                     smoothFactor =  .5,
                     opacity = 1,
                     fillOpacity = 0.7,
                     color= "grey",
                     weight = 0.5,
                     fillColor = ~pal5(ano_ODSShiny()$ODS3.3.2),
                     highlightOptions = highlightOptions(weight = 2,
                                                         fillOpacity= 1,
                                                         color = "grey",
                                                         opacity = 1,
                                                         bringToFront = TRUE)) %>%
         leaflet::addLegend("bottomright",
                            pal = pal5,
                            values = ~ODS3.3.2,
                            title= "Porcentaje (%)",
                            opacity= 0.7)
     })
     
     output$table3.3.2 = renderDT({
       datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS3.3.2")], 
                 colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS3.3.2'),
                 filter = 'top',
                 options = list(pageLength = 13
                 ))})
     
     
     output$malaria = renderLeaflet({
       pal5 = colorNumeric(palette = "YlOrBr", domain = c(0,100))
       
       ano_ODSShiny()  %>% 
         st_transform(crs= "+init=epsg:4326") %>%
         leaflet() %>%
         addProviderTiles(provider= "CartoDB.Positron") %>%
      #   addPolygons(data = mapa_prov, 
       #              stroke = TRUE, 
        #             smoothFactor =  .5,
         #            opacity = 1,
          #           fillOpacity = 0,
           #          weight = 0.5,
            #         color= "black") %>%
         addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS3.3.3,"%"),
                     stroke = TRUE, 
                     smoothFactor =  .5,
                     opacity = 1,
                     fillOpacity = 0.7,
                     color= "grey",
                     weight = 0.5,
                     fillColor = ~pal5(ano_ODSShiny()$ODS3.3.3),
                     highlightOptions = highlightOptions(weight = 2,
                                                         fillOpacity= 1,
                                                         color = "grey",
                                                         opacity = 1,
                                                         bringToFront = TRUE)) %>%
         leaflet::addLegend("bottomright",
                            pal = pal5,
                            values = ~ODS3.3.3,
                            title= "Porcentaje (%)",
                            opacity= 0.7)
     })
     
     output$table3.3.3 = renderDT({
       datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS3.3.3")], 
                 colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS3.3.3'),
                 filter = 'top',
                 options = list(pageLength = 13
                 ))})
     
     
     output$hepatitis = renderLeaflet({
       pal5 = colorNumeric(palette = "YlOrBr", domain = c(0,100))
       
       ano_ODSShiny()  %>% 
         st_transform(crs= "+init=epsg:4326") %>%
         leaflet() %>%
         addProviderTiles(provider= "CartoDB.Positron") %>%
     #    addPolygons(data = mapa_prov, 
      #               stroke = TRUE, 
       #              smoothFactor =  .5,
        #             opacity = 1,
         #            fillOpacity = 0,
          #           weight = 0.5,
           #          color= "black") %>%
         addPolygons(label= paste0(ano_ODSShiny()$DEPARTAMEN,': ', ano_ODSShiny()$ODS3.3.4,"%"),
                     stroke = TRUE, 
                     smoothFactor =  .5,
                     opacity = 1,
                     fillOpacity = 0.7,
                     color= "grey",
                     weight = 0.5,
                     fillColor = ~pal5(ano_ODSShiny()$ODS3.3.4),
                     highlightOptions = highlightOptions(weight = 2,
                                                         fillOpacity= 1,
                                                         color = "grey",
                                                         opacity = 1,
                                                         bringToFront = TRUE)) %>%
         leaflet::addLegend("bottomright",
                            pal = pal5,
                            values = ~ODS3.3.4,
                            title= "Porcentaje (%)",
                            opacity= 0.7)
     })
     
     output$table3.3.4 = renderDT({
       datatable(ano_ODSShiny()[c("DEPARTAMEN","ODS3.3.4")], 
                 colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS3.3.4'),
                 filter = 'top',
                 options = list(pageLength = 13
                 ))})
}

# Run the application 
shinyApp(ui = ui, server = server)
