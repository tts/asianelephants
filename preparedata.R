library(tidyverse)
library(rgbif)
library(rnaturalearth)
library(sf)
library(leaflet)

# Asian elephant
#
# rgbif code snippet by https://github.com/AChase44/FISH-504
species <- occ_search(scientificName = "Elephas maximus", limit = 5000)
species_1 <- as.data.frame(species$data)
el <- species_1[,c("acceptedScientificName","stateProvince", "year", "recordedBy", "basisOfRecord", "references", "decimalLatitude","decimalLongitude")]
write_rds(el, "el.RDS")

el <- el %>% 
  filter(decimalLongitude > 60, 
         acceptedScientificName != "BOLD:AAF0248") %>% # few bad data points
  mutate(stateProvince = ifelse(is.na(stateProvince), "-", stateProvince),
         year = ifelse(is.na(year), "-", year),
         recordedBy = ifelse(is.na(recordedBy), "-", recordedBy),
         references = ifelse(is.na(references), "-", 
                             ifelse(grepl("https?://", references), paste0("<a href='", references, "'>Link</a>"), 
                                    references)))

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
elCol <- colorFactor(palette = 'RdYlGn', el$basisOfRecord)

leaflet() %>%
  addTiles() %>% 
  setView(lat = mean(el$decimalLatitude), 
          lng = mean(el$decimalLongitude), 
          zoom = 3) %>% 
  addCircleMarkers(data = el,
                   lat = ~decimalLatitude,
                   lng = ~decimalLongitude,
                   popup = ~paste("<b>Basis of record:</b> ", basisOfRecord, "<br/>",
                                  "<b>Scientific name:</b> ", acceptedScientificName, "<br/>", 
                                  "<b>State/Province:</b> ", stateProvince, "<br/>",
                                  "<b>Year:</b> ", year, "<br/>", 
                                  "<b>Recorded by:</b> ", recordedBy, "<br/>", 
                                  "<b>References:</b> ", references),
                   color = ~elCol(basisOfRecord),
                   weight = 2, 
                   opacity = 0.4) %>% 
  addLegend(pal = elCol, values = el$basisOfRecord, title = "Basis of record",
            position = "bottomright")

