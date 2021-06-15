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
  
  sidebarPanel(
    HTML("<div><span style='color:black'>
         <p>A herd of wild <a href='https://en.wikipedia.org/wiki/Asian_elephant'>Asian elephants</a> has been wandering in China since 2020, gathering a lot of <a href='https://www.theguardian.com/world/2021/jun/02/herd-of-escaped-elephants-leave-500km-trail-of-destruction-in-south-west-china'>headlines recently</a>.</p>
         <p></p>
         <p>Most of the elephants in China live in the <a href='https://en.wikipedia.org/wiki/Xishuangbanna_Dai_Autonomous_Prefecture'>Xishuangbanna Dai Autonomous Prefecture</a>, the southernmost region of the <a href='https://en.wikipedia.org/wiki/Yunnan'>Yunnan province</a>.</p>
         <p></p>
         <p>This week, the herd has been seen near the city of <a href='https://en.wikipedia.org/wiki/Kunming'>Kunming</a>, the capital of Yunnan.</p>
         <p></p>
         <p>Elephant data presented in these maps come mainly from two sources:</p>
         <ul>
           <li><a href='https://gbif.org'>GBIF</a>, global Biodiversity Information Facility via the <a href='https://github.com/ropensci/rgbif'>rgbif R library</a></li>
           <li><a href='https://www.asesg.org/PDFfiles/2012/35-43-Zhang.pdf'>Li Zhang: Current Status of Asian Elephants in China. <i>Gajah 35 (2011) 43-46</i> (PDF)</a></li>
         </ul>
         <p>In addition, there is one personal data marker: a sighting from <a href='https://www.tabinwildlife.com.my/'>Tabin Wildlife Resort</a>, Sabah, Malaysian Borneo.</p>
         <p>Yunnan polygons are from <a href='https://ttdata.humdata.org'>Humanitarian Data Exchange</a></p>
         <p></p>
         <p>Tuija Sonkkila <a href='https://twitter.com/ttso?lang=en'>@ttso</a></p>
         <p><a href='https://github.com/tts/asianelephants'>https://github.com/tts/asianelephants</a></p>
         </span></div>"),
    width = 3
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Heatmap", 
               leafletOutput("heat", height = 800)),
      tabPanel("Observations", 
               leafletOutput("map", height = 800))
    ),
    width = 9
  )
)

server <- function(input, output, session) {
  
  output$heat <- renderLeaflet({
    
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
