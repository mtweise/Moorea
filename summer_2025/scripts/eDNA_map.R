
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


# "red" = Thermistor site
# "blue" = Morning samples
# "yellow" = Afternoon samples 


##############################################
#clean map of just the therm sites

# Clean environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(sf)
library(here)
library(janitor)
library(rnaturalearth)
library(ggspatial)
library(ggrepel)
library(maptiles)

# --- Read data ---
therm_sites <- read_csv(here("summer_2025/data", "MCR_thermistor_sites.csv")) |>
  clean_names()

# Convert to sf
therm_site_sf <- st_as_sf(
  therm_sites,
  coords = c("lon_3", "lat_2"),
  crs = 4326,
  remove = FALSE
)

# Filter to your 5 thermistor sites
therm_filtered <- therm_site_sf |>
  filter(site %in% c("B01", "B06", "B32", "B09", "B35"))

# --- Define Moorea bounding box ---
moorea_bbox <- st_bbox(c(xmin = -150, xmax = -149.7, ymin = -17.63, ymax = -17.45), crs = st_crs(4326))

# --- Get basemap tiles ---
# you can also try: "CartoDB.Positron" or "Stamen.TerrainBackground"
moorea_basemap <- get_tiles(moorea_bbox, provider = "Esri.WorldImagery", zoom = 13)

# --- Plot ---
ggplot() +
  layer_spatial(moorea_basemap) +
  geom_sf(
    data = therm_filtered,
    color= "red",
    size = 3.2,
    shape = 21,
    fill = "red",
    stroke = 1
  ) +
  geom_label_repel(
    data = therm_filtered,
    aes(label = site, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    family = "sans",
    nudge_y = 0.002,
    box.padding = 0.4
  ) +
  annotation_scale(location = "bl", width_hint = 0.3, text_col = "white",
                   line_col = "white",
                   tick_col = "white") +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering (
    text_col = "white",
    fill = c("white", "gray40")
  )) +
  coord_sf(
    xlim = c(-149.94, -149.74),
    ylim = c(-17.615, -17.46),
    expand = FALSE
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  ggtitle("Proposed Thermistor Sites")



