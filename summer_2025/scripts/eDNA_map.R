
#######################################
# code reworked from ESM 206 hw 1 and lab 3 (oct 16)

#clean the environment
rm(list=ls())

library(janitor)
library(here)
library(tidyverse)
library(tidyr)
library(tidygeocoder)
library(dplyr)
library(stringr)
#Spatial libraries
library(sf) #This helps with plotting boundaries and lots of other things
library(rnaturalearth) #base commands and some maps
library(ggspatial) #north arrow and scale
library(ggrepel) #labels
library(maptiles)
library(sf)
library(ggplot2)
library(leaflet)
library(hms)

##read in data
edna_raw <- read_csv(here("summer_2025/data", "eDNA_sites25.csv")) |>
  clean_names()

#thermistor sites
therm_sites <- read_csv(here("summer_2025/data", "MCR_thermistor_sites.csv")) |>
  clean_names()


# Convert GPS data to sf format
gps_sf <- st_as_sf(edna_raw, coords = c("longitude_dd", "latitude_dd"), crs = 4326)


therm_site_sf <- st_as_sf(
  therm_sites,
  coords = c("lon_3", "lat_2"),
  crs = 4326,
  remove = FALSE
)

# Filter therm_site_sf to sites starting with "B"
therm_site_sf_B <- therm_site_sf %>%
  filter(grepl("^B", site))  # ^B means "starts with B"

# Define bounding box manually for Moorea
#i don't think this worked
#moorea_bbox <- st_bbox(c(xmin = -150, xmax = -149.75, ymin = -17.6, ymax = -17.45), crs = st_crs(4326))
#moorea_basemap <- get_tiles(moorea_bbox, provider = "OpenStreetMap", crop = TRUE, zoom = 13)



gps_sf <- gps_sf %>%
  mutate(color = ifelse(time < as_hms("12:00:00"), "blue", "yellow"))

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # gps_sf points, colored by time
  addCircleMarkers(
    data = gps_sf,
    color = ~color,
    radius = 2
  ) %>%
  
  # therm_sites in a third color (red)
  addCircleMarkers(
    data = therm_site_sf_B,
    color = "red",
    radius = 3,
    label = ~site,   # use the site column for labels
    labelOptions = labelOptions(noHide = TRUE)
  ) %>%
  
  setView(lng = -149.82, lat = -17.53, zoom = 13)

















