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

setwd("~/GitHub/ODS")

subset1_1_1 = import("https://github.com/ChiaraZamoraM/ODS/raw/main/subset_1_1_1.RDS")

ui <- fluidPage(

    # Application title
    titlePanel("Objetivos de Desarollo Sostenible"),

    sidebarLayout(
        sidebarPanel(
            tags$a(href="http://ods.inei.gob.pe/ods/objetivos-de-desarrollo-sostenible", 
                   "Referencia de los datos",
                   target= "_blank"),
                   h5("Todos los datos"),
            selectInput("ano",
                        "Seleccione una fecha",
                        choices = unique(subset1_1_1$Ano)
            )
            ),
        mainPanel(
            tabsetPanel(
                tabPanel("1.1.1",leafletOutput("pobrezaex")),
                tabPanel("1.2.1",leafletOutput("pobrezatot"))
                )
            )
        )
)
        
# Define server logic required to draw a histogram

server <- function(input, output) {

     ano_subset1_1_1 <- reactive({
        y    <- subset1_1_1 %>% filter(Ano == input$ano)
        return(y)
     })
     
     output$pobrezaex = renderLeaflet({
         pal1 = colorNumeric(palette = "Blues", domain = subset1_1_1$Valor)
         
         ano_subset1_1_1()  %>% 
             st_transform(crs= "+init=epsg:4326") %>%
             leaflet() %>%
             addProviderTiles(provider= "CartoDB.Positron") %>%
             addPolygons(label= ano_subset1_1_1()$DEPARTAMEN,
                         stroke = FALSE, 
                         smoothFactor =  .5,
                         opacity = 1,
                         fillOpacity = 0.7,
                         fillColor = ~pal1(ano_subset1_1_1()$Valor),
                         highlightOptions = highlightOptions(weight = 5,
                                                             fillOpacity= 1,
                                                             color = "black",
                                                             opacity = 1,
                                                             bringToFront = TRUE))%>%
             
             addLegend("bottomright",
                       pal = pal1,
                       values = ~Valor,
                       title= "Porcentaje (%)",
                       opacity= 0.7) 
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
