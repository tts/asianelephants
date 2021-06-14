library(tidyverse)
library(rgbif)
library(googlesheets4)
library(sf)
library(leaflet)
library(leaflet.extras)
library(raster)
library(rgdal)

#---------------------------------------------------
# Data 1: 
# Global Biodiversity Information Facility (GBIF)
#---------------------------------------------------

# rgbif code snippet by https://github.com/AChase44/FISH-504
species <- occ_search(scientificName = "Elephas maximus", limit = 5000)
species_1 <- as.data.frame(species$data)
el <- species_1[,c("acceptedScientificName","stateProvince", "year", "recordedBy", 
                   "basisOfRecord", "references", "decimalLatitude","decimalLongitude")]

el <- el %>% 
  filter(decimalLongitude > 60, acceptedScientificName != "BOLD:AAF0248") %>% # few bad data points
  mutate(stateProvince = ifelse(is.na(stateProvince), "-", stateProvince),
         year = ifelse(is.na(year), "-", year),
         recordedBy = ifelse(is.na(recordedBy), "-", recordedBy),
         references = ifelse(is.na(references), "-", 
                             ifelse(grepl("https?://", references), paste0("<a href='", references, "'>Link</a>"), 
                                    references)),
         basisOfRecord = gsub("_", " ", str_to_title(basisOfRecord)),
         basisOfRecord = ifelse(basisOfRecord == "Living specimen", "Human observation", basisOfRecord)) # one row of these


write_rds(el, "el.RDS")
el <- readRDS("el.RDS")

#------------------------------------------------------------------------------
# Data 2:
# Li Zhang: Current Status of Asian Elephants in China. Gajah 35 (2011) 43-46
# https://www.asesg.org/PDFfiles/2012/35-43-Zhang.pdf
# 
# Animal counts mentioned in the article manually typed in a Google Sheet.
# Note that there seems to be a typo: Mengyang county is mentioned twice.
# I assume that the numbers of the latter are from Mengla county instead.
# Anyway, I couldn't find any geolocation for counties so this level of data
# is not used in the map.
#------------------------------------------------------------------------------

sheet <- "https://docs.google.com/spreadsheets/d/1dAqxuEnazMQMIAwsudU54_7_t8VQtUdzA0P74-YtBCc/edit?usp=sharing"
el_gd_data <- read_sheet(sheet)

# China's region polygons from Humanitarian Data Exchange
url <- "https://data.humdata.org/dataset/17a2aaa2-dea9-4a2e-8b3f-92d1bdfb850c/resource/9e67ddf9-ce26-4b7a-82b1-51e5ca0714c8/download/chn_adm_ocha_2020_shp.zip"
temp <- tempfile()
download.file(url, temp)
unzip(zipfile = temp, exdir = "china")
unlink(temp)

st_layers("china") # 4 layers
ch_provinces <- st_read("china", layer = "chn_admbnda_adm1_ocha_2020")
ch_divisions <- st_read("china", layer = "chn_admbnda_adm2_ocha_2020")

yunnan <- ch_provinces %>% 
  filter(ADM1_EN == "Yunnan Province") %>% 
  dplyr::select(ADM1_EN, geometry)

# Kunming, the capital of the Yunnan province, is the most Northern
# point where the famous elephant herd has wandered so far
kunming <- ch_divisions %>% 
  filter(ADM2_EN == "Kunming") %>% 
  dplyr::select(ADM2_EN, Adm2_CAP, geometry)

el_div <- merge(el_gd_data, ch_divisions, by.x = "Division", by.y = "ADM2_EN")
el_div <- el_div %>% 
  dplyr::select(Division, Min, Max, geometry)
el_div <- st_as_sf(el_div)


#-------------------------------------------------------------------------------
# Data 3:
# My own observation. Still waiting for mandatory agree votes at iNaturalist 
# so not available from GBIF
#-------------------------------------------------------------------------------

flickr_geocoded <- readRDS("../flickr/flickr_geocoded.RDS")
my_el <- flickr_geocoded %>% 
  filter(title == "Hammock session") %>% 
  mutate(popup_img = "<a href='https://flic.kr/p/MND8dD'>Video frame by JH</a>",
         datetaken = as.Date(datetaken)) %>% 
  dplyr::select(latitude, longitude, datetaken, popup_img)

rm(flickr_geocoded)
gc()

#------------
# Map
#------------

elCol <- colorFactor(palette = 'viridis', el$basisOfRecord)

m <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>% 
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, 
                   options = providerTileOptions(opacity = 0.8),
                   group = "Dark") %>% 
  addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
  addLayersControl(baseGroups = c("OpenStreetMap", "Dark", "Toner", "Topo"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  setView(lat = mean(el$decimalLatitude), 
          lng = mean(el$decimalLongitude), 
          zoom = 3) %>% 
  addPolygons(data = yunnan,
              weight = 3,
              opacity = 0.3,
              color = "tomato",
              label = ~ADM1_EN) %>% 
  addPolygons(data = kunming,
              weight = 3,
              opacity = 0.6,
              color = "snow",
              label = ~paste0(ADM2_EN, ", ", Adm2_CAP)) %>% 
  addPolygons(data = el_div,
              weight = 2,
              opacity = 0.6,
              color = "red",
              fillColor = max,
              label = ~Division) %>% 
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
  addMarkers(data = my_el,
             lat = ~latitude,
             lng = ~longitude,
             label = "My observation of a wild Elephas maximus borneensis",
             popup = ~paste0(popup_img, "<br/><b>Taken at: </b>", datetaken)) %>%
  addSearchFeatures(targetGroups = "elobs", 
                    options = searchFeaturesOptions(
                      zoom = 5, openPopup = TRUE, 
                      firstTipSubmit = TRUE, textPlaceholder = "Type species, year, or place",
                      autoCollapse = FALSE, hideMarkerOnCollapse = TRUE)
                    ) %>% 
  addLegend(pal = elCol, values = el$basisOfRecord, title = "Observation type", position = "bottomright") %>% 
  addMeasure(primaryLengthUnit = "kilometers")

htmlwidgets::saveWidget(m, "el_map.html")

