
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
edna_raw <- read_csv(here("summer_2025/data", "eDNA_sites_2026.csv")) |>
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

therm_site_sf_B_filtered <- therm_site_sf_B %>%
  filter(site %in% c("B01", "B06", "B32", "B09", "B35"))

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
    data = therm_site_sf_B_filtered,
    color = "red",
    radius = 3,
    label = ~site,   # use the site column for labels
    labelOptions = labelOptions(noHide = TRUE)
  ) %>%
  
  setView(lng = -149.82, lat = -17.53, zoom = 13)






#trying to fix labels
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # gps_sf points, colored by time
  addCircleMarkers(
    data = gps_sf,
    color = ~color,
    radius = 2
  ) %>%
  
  # therm_sites points
  addCircleMarkers(
    data = therm_site_sf_B_filtered,
    color = "red",
    radius = 3
  ) %>%
  
  # add labels separately with connecting lines
  addLabelOnlyMarkers(
    data = therm_site_sf_B_filtered,
    label = ~site,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "auto",
      textsize = "12px",
      style = list(
        "color" = "white",
        "font-weight" = "bold",
        "text-shadow" = "1px 1px 2px black",
        "background-color" = "rgba(0,0,0,0.4)",
        "padding" = "2px 4px",
        "border-radius" = "4px",
        "box-shadow" = "0 0 3px rgba(0,0,0,0.5)"
      ),
      textOnly = TRUE,   # ensures no arrow/connector
      opacity = 1,
      sticky = FALSE,    # turns off arrow/leader line
      offset = c(0, -30) # offset label from marker
    )
  ) %>%
  
  setView(lng = -149.82, lat = -17.53, zoom = 13)
################################################

#####updated with 2026 data and colors######

####################


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
library(lubridate)
library(hms)

##read in data
edna_raw <- read_csv(here("summer_2025/data", "eDNA_sites_2026.csv")) |>
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

therm_site_sf_B_filtered <- therm_site_sf_B %>%
  filter(site %in% c("B01", "B06", "B32", "B09", "B35"))

gps_sf <- gps_sf %>%
  mutate(color = ifelse(time < as_hms("12:00:00"), "blue", "yellow"))

gps_sf <- gps_sf %>%
  mutate(
    # extract year from date column
    year = str_sub(date, 7, 10),  # "xx/xx/xxxx" format
    
    # assign colors based on year and time
    color = case_when(
      year == "2026" & time < as_hms("12:00:00")  ~ "#00008B",  # dark blue for 2026 morning
      year == "2026" & time >= as_hms("12:00:00") ~ "#FFD700",  # dark yellow for 2026 afternoon
      time < as_hms("12:00:00")                       ~ "blue",  # normal blue for other years
      time >= as_hms("12:00:00")                      ~ "yellow" # normal yellow for other years
    )
  )

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
    data = therm_site_sf_B_filtered,
    color = "red",
    radius = 3,
    label = ~site,   # use the site column for labels
    labelOptions = labelOptions(noHide = TRUE)
  ) %>%
  
  setView(lng = -149.82, lat = -17.53, zoom = 13)






#trying to fix labels
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # gps_sf points, colored by time
  addCircleMarkers(
    data = gps_sf,
    color = ~color,
    radius = 2
  ) %>%
  
  # therm_sites points
  addCircleMarkers(
    data = therm_site_sf_B_filtered,
    color = "red",
    radius = 3
  ) %>%
  
  # add labels separately with connecting lines
  addLabelOnlyMarkers(
    data = therm_site_sf_B_filtered,
    label = ~site,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "auto",
      textsize = "12px",
      style = list(
        "color" = "white",
        "font-weight" = "bold",
        "text-shadow" = "1px 1px 2px black",
        "background-color" = "rgba(0,0,0,0.4)",
        "padding" = "2px 4px",
        "border-radius" = "4px",
        "box-shadow" = "0 0 3px rgba(0,0,0,0.5)"
      ),
      textOnly = TRUE,   # ensures no arrow/connector
      opacity = 1,
      sticky = FALSE,    # turns off arrow/leader line
      offset = c(0, -30) # offset label from marker
    )
  ) %>%
  
  setView(lng = -149.82, lat = -17.53, zoom = 13)

