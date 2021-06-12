library(tidyverse)
library(rgbif)
library(rnaturalearth)
library(sf)
library(leaflet)

# Asian elephant subspecies
#
# rgbif code by https://github.com/AChase44/FISH-504
species <- occ_search(scientificName = "Elephas maximus indicus", limit = 1000)
species_1 <- as.data.frame(species$data)
el_indicus <- species_1[,c("scientificName","stateProvince", "year", "recordedBy", "references", "decimalLatitude","decimalLongitude")]

species <- occ_search(scientificName = "Elephas maximus maximus", limit = 1000)
species_1 <- as.data.frame(species$data)
el_maximus <- species_1[,c("scientificName","stateProvince", "year", "recordedBy", "references", "decimalLatitude","decimalLongitude")]

species <- occ_search(scientificName = "Elephas maximus sumatranus", limit = 1000)
species_1 <- as.data.frame(species$data)
el_sumatranus <- species_1[,c("scientificName","stateProvince", "year", "recordedBy", "references", "decimalLatitude","decimalLongitude")]

el <- rbind(el_indicus, el_maximus, el_sumatranus)

el <- el %>% 
  filter(decimalLongitude > 60) %>% # few bad data points
  mutate(stateProvince = ifelse(is.na(stateProvince), "-", stateProvince),
         year = ifelse(is.na(year), "-", year),
         recordedBy = ifelse(is.na(recordedBy), "-", recordedBy),
         references = ifelse(is.na(references), "-", references))

write_rds(el, "el.RDS")

# el <- readRDS("el.RDS")

asia <- ne_countries(continent = "asia", scale = "medium", returnclass = "sf")

plot <- ggplot() +
  geom_sf(data = asia, fill = "snow") +
  geom_point(data = el, aes(x = decimalLongitude, y = decimalLatitude, fill = scientificName),
             pch = 21, size = 2.5, alpha = 0.4) +
  scale_fill_viridis_d(option = "inferno") +
  guides(fill = guide_legend(title = "Subspecies")) +
  theme_void()

elCol <- colorFactor(palette = 'RdYlGn', el$scientificName)

leaflet() %>%
  addTiles() %>% 
  setView(lat = mean(el$decimalLatitude), 
          lng = mean(el$decimalLongitude), 
          zoom = 3) %>% 
  # addPolygons(data = asia,
  #             fillColor = "snow",
  #             stroke = TRUE,
  #             color = "grey",
  #             weight = 1) %>%
  addCircleMarkers(data = el,
                   lat = ~decimalLatitude,
                   lng = ~decimalLongitude,
                   popup = ~paste("<b>Scientific name:</b> ", scientificName, "<br/>", 
                                  "<b>State/Province:</b> ", stateProvince, "<br/>",
                                  "<b>Year:</b> ", year, "<br/>", 
                                  "<b>Recorded by:</b> ", recordedBy, "<br/>", 
                                  "<b>References:</b> ", references),
                   color = ~elCol(scientificName),
                   weight = 2, 
                   opacity = 0.4) 
