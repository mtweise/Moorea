########
#lewis and clark figure combining ideal sites w temperature

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


#######thermistor site locations
therm_sites <- read_csv(here("summer_2025/data", "MCR_thermistor_sites.csv")) |>
  clean_names()


# Convert GPS data to sf format

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


#####temperature data

temp_data <- read_csv(here("summer_2025/data", "temperature_summary.csv")) |>
  clean_names()

# --- Add a column for daily range and average range per site ---
temp_data <- temp_data |>
  mutate(daily_range_c = max_temp_c - min_temp_c) |>        # step 1: range per day
  group_by(site) |>
  mutate(avg_daily_range_site = mean(daily_range_c, na.rm = TRUE)) |>  # step 2: mean range per site
  ungroup()




##############################################
#clean map of just 

######################### 
#attempt 1

####### Load and prepare thermistor site data ########

therm_sites <- read_csv(here("summer_2025/data", "MCR_thermistor_sites.csv")) |>
  clean_names()

# Convert to sf
therm_site_sf <- st_as_sf(
  therm_sites,
  coords = c("lon_3", "lat_2"),
  crs = 4326,
  remove = FALSE
)

# Filter to all B sites
therm_site_sf_B <- therm_site_sf %>%
  filter(grepl("^B", site))

# Your five sites of interest
therm_site_sf_B_filtered <- therm_site_sf_B %>%
  filter(site %in% c("B01", "B06", "B32", "B09", "B35"))

####### Load and process temperature data ########

temp_data <- read_csv(here("summer_2025/data", "temperature_summary.csv")) |>
  clean_names() |>
  mutate(daily_range_c = max_temp_c - min_temp_c) |>
  group_by(site) |>
  summarize(avg_daily_range_c = mean(daily_range_c, na.rm = TRUE)) |>
  ungroup()

# Join average daily range data to the filtered thermistor sites
therm_with_temp <- therm_site_sf_B_filtered %>%
  left_join(temp_data, by = "site")

####### Make the map ########

# Define Moorea bounding box
moorea_bbox <- st_bbox(c(xmin = -150, xmax = -149.7, ymin = -17.63, ymax = -17.45), crs = st_crs(4326))

# Get basemap
moorea_basemap <- get_tiles(moorea_bbox, provider = "Esri.WorldImagery", zoom = 13)

###############################attempt 2

p <- ggplot() +
  # Basemap
  layer_spatial(moorea_basemap) +
  
  # All B sites (gray points for context)
  geom_sf(data = therm_site_sf_B, color = "white", size = 1.5, alpha = 0.7) +
  
  # Five selected sites — color fill = average daily range
  geom_sf(
    data = therm_with_temp,
    aes(fill = avg_daily_range_c),   # <-- fill now encodes the data
    shape = 21,                      # circle with fill
    color = "white",                 # outline
    size = 4,                        # uniform size
    stroke = 1.2
  ) +
  
  # Site labels
  geom_label_repel(
    data = therm_with_temp,
    aes(label = site, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    nudge_y = 0.002,
    box.padding = 0.4,
    color = "white",
    fill = "black"
  ) +
  
  # Color scale + legend
  scale_fill_viridis_c(
    option = "plasma",
    name = "Avg Daily Range (°C)",
    guide = guide_colorbar(
      barheight = unit(80, "pt"),
      barwidth  = unit(10, "pt"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.3, text_col = "white", line_col = "white") +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering(
      text_col = "white", fill = c("white", "gray40")
    )
  ) +
  
  coord_sf(xlim = c(-149.94, -149.74), ylim = c(-17.615, -17.46), expand = FALSE) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text( size = 14, color = "black"),
    legend.position = "right",
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  ggtitle("Average Daily Temperature Range at Selected Thermistor Sites")

# Print the map
print(p)


####################
#attempt to fix legend

p <- ggplot() +
  # Basemap
  layer_spatial(moorea_basemap) +
  
  # All B sites (gray points for context)
  geom_sf(
    data = therm_site_sf_B,
    aes(color = "Other existing thermistor sites"),
    size = 1.5,
    alpha = 0.7
  ) +
  
  # Five selected sites — color fill = average daily range
  geom_sf(
    data = therm_with_temp,
    aes(fill = avg_daily_range_c),
    shape = 21,
    color = "white",
    size = 4,
    stroke = 1.2
  ) +
  
  # Site labels
  geom_label_repel(
    data = therm_with_temp,
    aes(label = site, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    nudge_y = 0.002,
    box.padding = 0.4,
    color = "white",
    fill = "black"
  ) +
  
  # Color scale for temperature
  scale_fill_viridis_c(
    option = "plasma",
    name = "Avg Daily Range (°C)",
    guide = guide_colorbar(
      barheight = unit(50, "pt"),
      barwidth  = unit(6, "pt"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # Manual color legend for gray points
  scale_color_manual(
    name = "",
    values = c("Other existing thermistor sites" = "gray80")
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.3, text_col = "white", line_col = "white") +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering(
      text_col = "white", fill = c("white", "gray40")
    )
  ) +
  
  coord_sf(xlim = c(-149.94, -149.74), ylim = c(-17.615, -17.46), expand = FALSE) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 14, color = "black"),
    legend.position = c(0.99, 0.04),
    legend.justification = c("right", "bottom"),
    legend.text = element_text(color = "black", size = 8),     # smaller text
    legend.title = element_text(color = "black", size = 9),    # smaller title
    legend.key.size = unit(5, "pt"),        # shrink legend keys
    legend.key.height = unit(8, "pt"),      # shrink colorbar height
    legend.background = element_rect(fill = "white", color = NA),
    legend.spacing.y = unit(1, "pt"),            # reduce vertical space between keys
    legend.spacing.x = unit(1, "pt"),            # reduce horizontal space
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2), # tighter box
  ) +
  ggtitle("Average Daily Temperature Range at Selected Thermistor Sites")

print(p)


########################
#combine legends

p <- ggplot() +
  # Basemap
  layer_spatial(moorea_basemap) +
  
  # All B sites (gray points for context)
  geom_sf(
    data = therm_site_sf_B,
    aes(color = "Other existing thermistor sites"),
    size = 1.5,
    alpha = 0.7
  ) +
  
  # Five selected sites — color fill = average daily range
  geom_sf(
    data = therm_with_temp,
    aes(fill = avg_daily_range_c, color = "Selected thermistor sites"),
    shape = 21,
    size = 4,
    stroke = 1.2
  ) +
  
  # Site labels
  geom_label_repel(
    data = therm_with_temp,
    aes(label = site, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    nudge_y = 0.002,
    box.padding = 0.4,
    color = "white",
    fill = "black"
  ) +
  
  # Color scale (shared title)
  scale_fill_viridis_c(
    option = "plasma",
    name = "Thermistor Sites\nAvg Daily Range (°C)",
    guide = guide_colorbar(
      barheight = unit(45, "pt"),
      barwidth  = unit(6, "pt"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # Manual color legend for point categories (shares title)
  scale_color_manual(
    name = "Thermistor Sites\nAvg Daily Range (°C)",
    values = c(
      "Other existing thermistor sites" = "gray80",
      "Selected thermistor sites" = "white"
    )
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.3, text_col = "white", line_col = "white") +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering(
      text_col = "white", fill = c("white", "gray40")
    )
  ) +
  
  coord_sf(xlim = c(-149.94, -149.74), ylim = c(-17.615, -17.46), expand = FALSE) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 14, color = "black"),
    
    # Compact bottom-right legend
    legend.position = c(0.98, 0.05),
    legend.justification = c("right", "bottom"),
    legend.text = element_text(color = "black", size = 8),
    legend.title = element_text(color = "black", size = 9),
    legend.key.size = unit(5, "pt"),
    legend.key.height = unit(6, "pt"),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.spacing.y = unit(1, "pt"),
    legend.spacing.x = unit(1, "pt"),
    legend.margin = margin(2, 2, 2, 2),
    legend.box.margin = margin(-2, -2, -2, -2)
  ) +
  ggtitle("Average Daily Temperature Range at Selected Thermistor Sites")

print(p)

###########
p <- ggplot() +
  # Basemap
  layer_spatial(moorea_basemap) +
  
  # All B sites (gray points for context)
  geom_sf(
    data = therm_site_sf_B,
    aes(color = "Other existing thermistor sites"),
    size = 1.5,
    alpha = 0.7
  ) +
  
  # Five selected sites — color fill = average daily range
  geom_sf(
    data = therm_with_temp,
    aes(fill = avg_daily_range_c),
    shape = 21,
    color = "white",
    size = 4,
    stroke = 1.2
  ) +
  
  # Site labels
  geom_label_repel(
    data = therm_with_temp,
    aes(label = site, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    nudge_y = 0.002,
    box.padding = 0.4,
    color = "white",
    fill = "black"
  ) +
  
  # Color scale for temperature
  scale_fill_viridis_c(
    option = "plasma",
    name = "Avg Daily Range (°C)",
    guide = guide_colorbar(
      barheight = unit(50, "pt"),
      barwidth  = unit(6, "pt"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # Manual color legend for gray points
  scale_color_manual(
    name = "",
    values = c("Other existing thermistor sites" = "gray30")
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.3, text_col = "white", line_col = "white") +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering(
      text_col = "white", fill = c("white", "gray40")
    )
  ) +
  
  coord_sf(xlim = c(-149.94, -149.74), ylim = c(-17.615, -17.46), expand = FALSE) +
  
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 14, color = "black"),
    
    # Legend positioning
    legend.position = c(0.99, 0.03),
    legend.justification = c("right", "bottom"),
    
    # Text and key sizes
    legend.text = element_text(color = "black", size = 8),
    legend.title = element_text(color = "black", size = 9),
    legend.key.size = unit(4, "pt"),         # smaller keys
    legend.key.width = unit(4, "pt"),        # thinner colorbar
    legend.key.height = unit(6, "pt"),       # shorter colorbar
    
    # Reduce spacing between items
    legend.spacing.y = unit(0.5, "pt"),
    legend.spacing.x = unit(1, "pt"),
    legend.margin = margin(t = 1, r = 1, b = 1, l = 1),
    
    # Semi-transparent background
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA)
  ) +
  ggtitle("Average Daily Temperature Range at Selected Thermistor Sites")

print(p)

