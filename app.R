library(shiny)
library(leaflet)
library(htmltools)
library(sf)
library(leaflet.extras)

el <- readRDS("./data/el.RDS")
yunnan <- readRDS("./data/yunnan.RDS")
kunming <- readRDS("./data/kunming.RDS")
el_div <- readRDS("./data/el_div.RDS")
my_el <- readRDS("./data/my_el.RDS")

# https://stackoverflow.com/a/52226825
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    font-weight: bold;
    font-size: 12px;
  }
"))

labelstyle <- list(
  "color" = "black",
  "font-family" = "serif",
  "font-style" = "italic",
  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
  "font-size" = "12px",
  "border-color" = "rgba(0,0,0,0.5)"
)

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
    
    title <- tags$div(
      tag.map.title, HTML("<p>Distribution | by @ttso https://github.com/tts/asianelephants</p>")
    )  
    
    mh <- leaflet() %>%
      addTiles(
        group = "OpenStreetMap"
      ) %>% 
      addProviderTiles(
        "Stamen.TerrainBackground", group = "Terrain"
      ) %>% 
      addProviderTiles(
        "CartoDB.DarkMatter", group = "Dark"
      ) %>% 
      addTiles(
        urlTemplate = "", attribution = 'Data: GBIF and AsESG/Gajah'
      ) %>% 
      addLayersControl(
        baseGroups = c("Terrain", "Dark", "OpenStreetMap"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      setView(
        lat = mean(el$decimalLatitude), 
        lng = mean(el$decimalLongitude), 
        zoom = 3
      ) %>% 
      addControl(
        title, position = "topleft", className="map-title"
      ) %>% 
      addPolygons(
        data = yunnan,
        weight = 3,
        opacity = 0.3,
        color = "tomato",
        label = ~ADM1_EN,
        labelOptions = labelOptions(
          style = labelstyle)
      ) %>% 
      addPolygons(
        data = kunming,
        weight = 3,
        opacity = 0.6,
        color = "snow",
        label = ~paste0(ADM2_EN, ", ", Adm2_CAP),
        labelOptions = labelOptions(
          style = labelstyle)
      ) %>% 
      addPolygons(
        data = el_div,
        weight = 2,
        color = "red",
        fillOpacity = 0.1,
        label = ~Division,
        labelOptions = labelOptions(
          style = labelstyle)
      ) %>% 
      addHeatmap(
        data = el,
        lat = ~decimalLatitude,
        lng = ~decimalLongitude,
        blur = 20, max = 0.05, radius = 15
      ) 
    
    mh
    
  })
  
  output$map <- renderLeaflet({
    
    title <- tags$div(
      tag.map.title, HTML("<p>Observations | by @ttso https://github.com/tts/asianelephants</p>")
    )  
    
    elCol <- colorFactor(palette = 'viridis', el$basisOfRecord)
    
    m <- leaflet() %>%
      addTiles(
        group = "OpenStreetMap"
      ) %>% 
      addProviderTiles(
        providers$Stamen.TerrainBackground, group = "Terrain"
      ) %>% 
      addProviderTiles(
        providers$CartoDB.DarkMatter, group = "Dark"
      ) %>% 
      addTiles(
        urlTemplate = "", attribution = 'Data: GBIF and AsESG/Gajah'
      ) %>% 
      addLayersControl(
        baseGroups = c("Terrain", "Dark", "OpenStreetMap"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      setView(
        lat = mean(el$decimalLatitude), 
        lng = mean(el$decimalLongitude), 
        zoom = 3
      ) %>% 
      addControl(
        title, position = "topleft", className="map-title"
      ) %>% 
      addPolygons(data = yunnan,
                  weight = 3,
                  opacity = 0.3,
                  color = "tomato",
                  label = ~ADM1_EN,
                  labelOptions = labelOptions(
                    style = labelstyle
                  )) %>% 
      addPolygons(data = kunming,
                  weight = 3,
                  opacity = 0.6,
                  color = "snow",
                  label = ~paste0(ADM2_EN, ", ", Adm2_CAP),
                  labelOptions = labelOptions(
                    style = labelstyle
                  )) %>% 
      addPolygons(data = el_div,
                  weight = 2,
                  color = "red",
                  label = ~Division,
                  labelOptions = labelOptions(
                    style = labelstyle
                  )) %>% 
      addCircleMarkers(data = el,
                       lat = ~decimalLatitude,
                       lng = ~decimalLongitude,
                       group = "elobs_circle",
                       popup = ~paste("<b>Basis of record:</b> ", basisOfRecord, "<br/>",
                                      "<b>Scientific name:</b> ", acceptedScientificName, "<br/>", 
                                      "<b>State/Province:</b> ", stateProvince, "<br/>",
                                      "<b>Year:</b> ", year, "<br/>", 
                                      "<b>Recorded by:</b> ", recordedBy, "<br/>", 
                                      "<b>References:</b> ", references),
                       color = ~elCol(basisOfRecord),
                       radius = 6,
                       weight = 2, 
                       opacity = 1) %>% 
      # Search is not working with circleMarkers. 
      # Adding tiny double markers to be the search target instead.
      # https://stackoverflow.com/a/53546892
      addMarkers(
        data = el, 
        lat = ~decimalLatitude, 
        lng = ~decimalLongitude, 
        label = ~paste0(acceptedScientificName, " ", stateProvince, " ", year),
        group = 'elobs', 
        icon = makeIcon( 
          iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
          iconWidth = 1, iconHeight = 1
        )
      ) %>%
      addMarkers(
        data = my_el,
        lat = ~latitude,
        lng = ~longitude,
        label = "My observation of a wild Elephas maximus borneensis",
        popup = ~paste0(popup_img, "<br/><b>Taken at: </b>", datetaken)
      ) %>%
      addSearchFeatures(
        targetGroups = "elobs", 
        options = searchFeaturesOptions(
          zoom = 5, openPopup = TRUE, 
          firstTipSubmit = TRUE, textPlaceholder = "Type species, year, or place",
          autoCollapse = FALSE, hideMarkerOnCollapse = TRUE)
      ) %>% 
      addLegend(
        pal = elCol, values = el$basisOfRecord, 
        title = "Observation type", position = "bottomright"
      ) %>% 
      addMeasure(
        primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters")
    
    m
    
    
  })
  
  
}

shinyApp(ui, server)
