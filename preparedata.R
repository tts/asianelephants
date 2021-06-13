library(tidyverse)
library(rgbif)
library(googlesheets4)
library(tidygeocoder)
library(rnaturalearth)
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
# Counts mentioned in the article were manually typed in a Google Sheet.
# Note that there seems to be a typo: Mengyang is mentioned twice.
# Assuming that the numbers of the latter are from Mengla instead.
#------------------------------------------------------------------------------

sheet <- "https://docs.google.com/spreadsheets/d/1dAqxuEnazMQMIAwsudU54_7_t8VQtUdzA0P74-YtBCc/edit?usp=sharing"
data <- read_sheet(sheet)

# Yunnan polylines from https://sedac.ciesin.columbia.edu/ftpsite/pub/data/China/adm_bnd/CTSAR90.bnd90/province/
# NASA/SEDAC. Registration needed.
yunnan <- readOGR("t5390/t5390.e00")
yunnan_sf <- st_as_sf(yunnan)

rm(yunnan)
gc()

# In Lambert Conformal Conic projection
yunnan_sf <- yunnan_sf %>%
  st_transform(crs = 4326)

write_rds(yunnan_sf, "yunnan_sf.RDS")
yunnan_sf <- readRDS("yunnan_sf.RDS")

#-------------------------------------------------------------------------------
# Data 3:
# My own observation. Still waiting for mandatory agree votes at iNaturalist 
# so not available from GBIF
#-------------------------------------------------------------------------------

flickr_geocoded <- readRDS("../flickr/flickr_geocoded.RDS")
my_el <- flickr_geocoded %>% 
  filter(title == "Hammock session") %>% 
  mutate(popup_img = paste("<a href='", url_z,"'><img src='", url_m, "'></a>"),
         datetaken = as.Date(datetaken)) %>% 
  select(latitude, longitude, datetaken, popup_img)

rm(flickr_geocoded)
gc()

# Plot
asia <- ne_countries(continent = "asia", scale = "medium", returnclass = "sf")

plot <- ggplot() +
  geom_sf(data = asia, fill = "snow") +
  geom_point(data = el, aes(x = decimalLongitude, y = decimalLatitude, fill = scientificName),
             pch = 21, size = 2.5, alpha = 0.4) +
  scale_fill_viridis_d(option = "inferno") +
  guides(fill = guide_legend(title = "")) +
  theme_void()

# Map
elCol <- colorFactor(palette = 'viridis', el$basisOfRecord)

leaflet() %>%
  addProviderTiles(providers$OpenTopoMap) %>%
  setView(lat = mean(el$decimalLatitude), 
          lng = mean(el$decimalLongitude), 
          zoom = 3) %>% 
  addPolylines(data = yunnan_sf,
               weight = 3,
               opacity = 0.3,
               color = "red") %>% 
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
             group = "my_elobs",
             lat = ~latitude,
             lng = ~longitude,
             label = ~paste0("My observation of a wild Elephas maximus borneensis ", datetaken)
             ) %>%
  addSearchFeatures(targetGroups = "elobs", 
                    options = searchFeaturesOptions(
                      zoom = 5, openPopup = TRUE, 
                      firstTipSubmit = TRUE, textPlaceholder = "Type species, year, or place",
                      autoCollapse = FALSE, hideMarkerOnCollapse = TRUE)
                    ) %>% 
  addLegend(pal = elCol, values = el$basisOfRecord, title = "Basis of record",
            position = "bottomright")

