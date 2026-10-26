## MWMC Watershed Level MD Analysis Scripts ####
## Contact: walkers3@si.edu
## October 2026 

## 1. Prepare Workspace ####

# load libraries
library(sf)
library(tidyverse)
library(patchwork)
library(viridis)

# turn off spherical geometry (revert to planar)
sf::sf_use_s2(FALSE)

# Load in data
## Core data hosted by the Coastal Carbon Network
cores <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/refs/heads/main/data/CCN_synthesis/CCN_cores.csv") %>% 
  filter(admin_division %in% c("Maryland"))

# Watershed Level 10 data from HydroSHEDS (https://www.hydrosheds.org/products/hydrobasins)
na_watersheds <- st_read("data/maps/watersheds_na_lev06/hybas_na_lev06_v1c.shp") 
# st_crs(na_watersheds) # check CRS

#MD-level hydrobasins 
library(readr)
hybas_na_lev06_marsh_area <- read_csv("data/hybas_na_lev06_marsh_area.csv") %>%
  filter(STATE == "Maryland")

## 2. Clean, Crop, and Join Spatial Data ####

# Define the bounding box vector for the region of interest 
# alternatively if you have a shp of the region, you can load it in and crop to that
bbox_coords <- c(ymin = 36.6575, 
                 ymax = 40,
                 xmin = -74.5807,
                 xmax = -77.5)
md_shp <- st_read("data/maps/md_state_boundary/MD_state.shp")
# Crop the shapefile to the bounding box
#using DMV bbox instead of shape file bc map looks better not cut off/includes full watershed on eastern shore 
md_watershed <- st_crop(na_watersheds, bbox_coords) 

# Optional: Write cropped version of the shapefile if you want to save some time reading in a smaller file 
# st_write(dmv_watershed, "path/to/filename.shp")

# convert sampling locations to spatial object
sf_cores <- st_as_sf(cores, coords = c("longitude", "latitude"), crs = 4326)

# spatial join field data with watersheds (using default st_within join approach)
# create summary table of how many cores are in each watershed 
cores_per_watershed <- st_join(sf_cores, md_watershed, join = st_within) %>% 
  count(HYBAS_ID, name = "core_count") %>% 
  drop_na(HYBAS_ID) %>% # some fell outside the watersheds of interest
  st_drop_geometry()

# merge with watershed polygons
final_table <- left_join(md_watershed, cores_per_watershed)

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
  labs(fill = "Core Count")+
  theme_bw()




# Export the figure
# ggsave(filename = "cool_watershed_map.jpg", width = 5, height = 7)

## interactive map
library(leaflet)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = md_watershed %>% filter(COAST == 1), # remove filter to reveal non-coastal watersheds
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


## Presentation Structure 
### Data Availability 

#Data Quantity ####
  
   #How many soil cores were sampled per watershed?
view(cores_per_watershed)

   #How many soil cores were sampled per 100ha of marsh habitat within each watershed?

area_table <- left_join(final_table, hybas_na_lev06_marsh_area)

watershed_summary <- area_table %>%
  mutate(cores_per_100ha = (core_count / marsh_area_ha) * 100)

watershed_summary %>%
  select(HYBAS_ID,core_count,marsh_area_ha,cores_per_100ha) %>%
  arrange(desc(cores_per_100ha))

ggplot(watershed_summary) +
  geom_sf(aes(fill = cores_per_100ha)) +
  scale_fill_viridis_c(name = "Cores per 100 ha",na.value = "grey80") +
  labs(
    title = "Soil Core Sampling Density",
    subtitle = "Sampling density relative to marsh area by watershed") +
  theme_minimal()

#two panel plot with total cores and density 
cores1 <- ggplot(watershed_summary) +
  geom_sf(
    aes(fill = core_count),
    color = "white",
    linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c("#F5F1E8", "#B8D8D0", "#5A9F99", "#174A5B"),
    name = "Number of\nsoil cores",
    na.value = "#D9DDE0")+
  labs(title = "A. Total Soil Cores",
    subtitle = "Number of cores per watershed") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right")

# Panel B: cores per area
cores2 <- ggplot(watershed_summary) +
  geom_sf(aes(fill = cores_per_100ha),color = "white",linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c("#F5F1E8", "#B8D8D0", "#5A9F99", "#174A5B"),
    name = "Cores per\n100 ha",
    na.value = "#D9DDE0")+
  labs( title = "B. Sampling Density",
    subtitle = "Cores per 100 hectares of marsh") +
  theme_minimal() +
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right")

# Combine maps side by side
corePanels <- cores1 + cores2 +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Soil Core Sampling Across Maryland Watersheds",
    theme = theme(
      plot.title = element_text(face = "bold",size = 16,hjust = 0.5)))

corePanels


#Data Utility ####
  
# What are the applications for soil cores sampled in this state? (Ex. soil carbon stock, soil carbon accumulation rates, forecasting ecosystem response to future scenarios) 
#What is the representativeness of soil cores in these different utility categories?
  

wetland_area <- read_csv("blue_carbon_data_inventory/data/wetland_area/state_wetland_area.csv")
  
 core_quality <- cores %>%
   filter(stocks_qual_code == "C1" | 
            dates_qual_code == "B1" | 
            elevation_qual_code %in% c("A1", "A2"))
#need to ask more about what these codes mean and how to distinguish good vs. great cores 

#Habitat Representativeness ####
  
  #What habitats have been sampled and how well do these sampling efforts reflect the extent of different coastal wetland habitats in this state?
habitat_counts <- cores %>%
  filter(!is.na(habitat)) %>%
  distinct(core_id, habitat) %>%
  count(habitat, name = "n_cores")

habitat_counts <- habitat_counts %>%
  rename(habitat_simplified = habitat)

#plot with total counts 
ggplot(habitat_counts,
       aes(x = reorder(habitat_simplified, n_cores),y = n_cores)) +
  geom_col() +
  coord_flip() +
  labs(title = "Soil Core Sampling by Habitat",
    x = "Coastal wetland habitat",
    y = "Number of soil cores") +
  theme_minimal()



#plot with area proportions 
state_habitat_wetland_area <- read_csv("data/state_habitat_wetland_area.csv")
md_habitat_area <- state_habitat_wetland_area %>%
  filter(state == "MD")

habitat_cores <- full_join(md_habitat_area, habitat_counts) %>%
  drop_na(state) 
habitat_cores <- habitat_cores %>%
  mutate(state_count = sum(n_cores,na.rm = TRUE)) %>%
  mutate(core_proportion = n_cores/state_count) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

#estimated
hab1<- ggplot(habitat_cores, 
       aes(x = reorder(habitat_simplified,mapped_proportion),y = mapped_proportion))+
  geom_col() +
    coord_flip() +
    labs(title = "Maryland Estimated Blue Carbon Habitat",
         x = "Habitat",
         y = "Proportion") +
  scale_y_continuous(limits = c(0, 1))+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    theme_minimal()

#sampled
hab2<- ggplot(habitat_cores, 
       aes(x = reorder(habitat_simplified,core_proportion),y = core_proportion))+
  geom_col() +
  coord_flip() +
  labs(title = "Maryland  Sampled Blue Carbon Habitat",
       x = "Habitat",
       y = "Proportion") +
  scale_y_continuous(limits = c(0, 1))+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal()

habCombined <- hab1 + hab2 +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Estimated vs Sampled",
    theme = theme(
      plot.title = element_text(face = "bold",size = 16,hjust = 0.5)))

habCombined
# Soil Carbon Stocks ####
  
  # What does soil carbon stock look like across the different watersheds in this state which contain coastal wetlands?
  