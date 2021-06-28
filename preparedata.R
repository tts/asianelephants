library(tidyverse)
library(rgbif)
library(googlesheets4)
library(sf)
library(raster)
library(rgdal)

#---------------------------------------------------
# Data 1: 
# Global Biodiversity Information Facility (GBIF)
#---------------------------------------------------

species <- occ_search(scientificName = "Elephas maximus", limit = 5000)
species_1 <- as.data.frame(species$data)

el <- species_1[,c("acceptedScientificName","stateProvince", "year", "recordedBy", 
                   "basisOfRecord", "references", "decimalLatitude","decimalLongitude", "datasetKey")]

el <- el %>% 
  filter(decimalLongitude > 60, acceptedScientificName != "BOLD:AAF0248") %>% # few bad data points
  mutate(stateProvince = ifelse(is.na(stateProvince), "-", stateProvince),
         year = ifelse(is.na(year), "-", year),
         recordedBy = ifelse(is.na(recordedBy), "-", recordedBy),
         references = ifelse(is.na(references), "-", 
                             ifelse(grepl("https?://", references), paste0("<a href='", references, "'>Link</a>"), 
                                    references)),
         basisOfRecord = gsub("_", " ", str_to_title(basisOfRecord)),
         basisOfRecord = ifelse(basisOfRecord == "Living specimen", "Human observation", basisOfRecord)) # I assume these are ~the same

# Dataset counts for the GBIF derived dataset
dataset_stats <- el %>% 
  group_by(datasetKey) %>% 
  summarize(count = n())

write.table(dataset_stats, "dataset_count.csv", sep = ",",  col.names = FALSE, row.names = FALSE)

el <- el %>% 
  select(-datasetKey)

write_rds(el, "data/el.RDS")

#------------------------------------------------------------------------------
# Data 2:
# Li Zhang: Current Status of Asian Elephants in China. Gajah 35 (2011) 43-46
# https://www.asesg.org/PDFfiles/2012/35-43-Zhang.pdf
# 
# Animal counts mentioned in the article manually typed in a Google Sheet.
#
# Note that there seems to be a typo: Mengyang county is mentioned twice.
# I assume that the numbers of the latter are from Mengla county instead.
# Anyway, I couldn't find any geolocation for counties, so data of this level
# is not used in these maps.
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

# Yunnan and its centroid (for leaflet setView)
yunnan <- ch_provinces %>% 
  filter(ADM1_EN == "Yunnan Province") %>% 
  dplyr::select(ADM1_EN, geometry)

yunnan_pol <- st_cast(yunnan, "POLYGON")
yunnan_centroid <- st_centroid(yunnan_pol)
write_rds(yunnan, "data/yunnan.RDS")
write_rds(yunnan_centroid, "data/yunnan_centroid.RDS")

# Kunming, the capital of the Yunnan province, is the most Northern
# point where the famous elephant herd has wandered so far in 2020-2021
kunming <- ch_divisions %>% 
  filter(ADM2_EN == "Kunming") %>% 
  dplyr::select(ADM2_EN, Adm2_CAP, geometry)

write_rds(kunming, "data/kunming.RDS")

el_div <- merge(el_gd_data, ch_divisions, by.x = "Division", by.y = "ADM2_EN")
el_div <- el_div %>% 
  dplyr::select(Division, County, Min, Max, geometry)
el_div <- st_as_sf(el_div)

write_rds(el_div, "data/el_div.RDS")


#-----------------------------
el <- readRDS("data/el.RDS")
yunnan <- readRDS("data/yunnan.RDS")
yunnan_centroid <- readRDS("data/yunnan_centroid.RDS")
kunming <- readRDS("data/kunming.RDS")
el_div <- readRDS("data/el_div.RDS")



