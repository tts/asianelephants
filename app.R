library(shiny)
library(leaflet)
library(htmltools)
library(sf)
library(leaflet.extras)

el_map <- readRDS("el_map.RDS")
el_heatmap <- readRDS("el_heatmap.RDS")

ui <- fluidPage(
  
  tags$h2(
    HTML("Asian elephants")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #333333;
        color: white;
      },
      .shiny-input-container {
        color: snow;
      }
      label.control-label {
        color: #5f9ea0;
      }"
    ))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Heatmap", 
               leafletOutput("heat", height = 800)),
      tabPanel("Observations", 
               leafletOutput("map", height = 800))
    ),
    width = 12
  )
)

server <- function(input, output, session) {
  
  output$heat <- renderLeaflet({
    
    el_heatmap
    
  })
  
  output$map <- renderLeaflet({
    
    el_map
    
  })
  
 
}

shinyApp(ui, server)
