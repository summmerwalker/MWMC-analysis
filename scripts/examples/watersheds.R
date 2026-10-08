## Watershed Mapping Example with Core Data ####
## Contact: wolfejax@si.edu
## June 1, 2026

## 1. Prepare Workspace ####

# load libraries
library(sf)
library(tidyverse)

# turn off spherical geometry (revert to planar)
sf::sf_use_s2(FALSE)

# Load in data
## Core data hosted by the Coastal Carbon Network
cores <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/refs/heads/main/data/CCN_synthesis/CCN_cores.csv") %>% 
  filter(admin_division %in% c("Maryland", "Virginia", "Deleware"))

# Watershed Level 10 data from HydroSHEDS (https://www.hydrosheds.org/products/hydrobasins)
na_watersheds <- st_read("data/maps/watersheds_na_lev06/hybas_na_lev06_v1c.shp") 
# st_crs(na_watersheds) # check CRS

## 2. Clean, Crop, and Join Spatial Data ####

# Define the bounding box vector for the region of interest 
# alternatively if you have a shp of the region, you can load it in and crop to that
bbox_coords <- c(ymin = 36.6575, 
                 ymax = 40,
                 xmin = -74.5807,
                 xmax = -77.5)

# Crop the shapefile to the bounding box
dmv_watershed <- st_crop(na_watersheds, bbox_coords) 

# Optional: Write cropped version of the shapefile if you want to save some time reading in a smaller file 
# st_write(dmv_watershed, "path/to/filename.shp")

# convert sampling locations to spatial object
sf_cores <- st_as_sf(cores, coords = c("longitude", "latitude"), crs = 4326)

# spatial join field data with watersheds (using default st_within join approach)
# create summary table of how many cores are in each watershed 
cores_per_watershed <- st_join(sf_cores, dmv_watershed, join = st_within) %>% 
  count(HYBAS_ID, name = "core_count") %>% 
  drop_na(HYBAS_ID) %>% # some fell outside the watersheds of interest
  st_drop_geometry()

# merge with watershed polygons
final_table <- left_join(dmv_watershed, cores_per_watershed)

## 3. Map It! ####

# custom palette option with color brewer
# custom_palette <- RColorBrewer::brewer.pal(name="YlGn", n=9)[3:9] # select for darker colors on the spectrum

## static map 
ggplot() +
  geom_sf(data = final_table, 
          mapping = aes(fill = core_count), 
          color = "grey", size = 0.2) +
  scale_fill_continuous(palette = "Blues", na.value = "grey60") + # comment out to switch palettes
  # scale_fill_continuous(palette = custom_palette, na.value = "white") + # uncomment for custom palette
  coord_sf() + 
  theme_bw()

# Export the figure
# ggsave(filename = "cool_watershed_map.jpg", width = 5, height = 7)

## interactive map
library(leaflet)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = dmv_watershed %>% filter(COAST == 1), # remove filter to reveal non-coastal watersheds
              group = "Watersheds",
              label = ~paste("HYBAS ID:", HYBAS_ID),
              weight = 0.5) %>%
  addCircleMarkers(data = cores, 
                   group = "Cores",
                   radius = 1, 
                   color = "green", 
                   label = ~paste("Core ID:", core_id)) %>% 
  addLayersControl(overlayGroups = c("Watersheds", "Cores"), 
                   options = layersControlOptions(collapsed = TRUE))

#### Yay!! ####