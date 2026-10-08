## Data Inventorying Functions

## Utility functions for data inventorying
## Generates metrics provided synthesis data

## Quantity ####

quantityMetric <- function(df){
  
  wetland_area <- read_csv("blue_carbon_data_inventory/data/wetland_area/state_wetland_area.csv")
  
  core_quantity <- df %>% 
    group_by(state) %>% 
    summarise(n = n()) %>% # calculate number of cores per state
    ungroup() %>% 
    mutate(state = state.abb[match(state,state.name)]) %>% 
    left_join(wetland_area) %>% 
    mutate(cores_per_1000ha = n / estimated_area_ha * 1000) %>% # calculate number of cores per 1000ha of wetland in each states
    arrange(-cores_per_1000ha) %>% 
    mutate(quantity_metric_rank = row.names(.), # rank states by metric
           # calculate weighted metric for each state
           quantity_metric_weight = cores_per_1000ha/sum(cores_per_1000ha))  %>% 
    select(-c(estimated_area_ha, percent_wetland, n)) %>% 
    rename(quantity_metric = cores_per_1000ha)
  
  return(core_quantity)
}


## Quality ####

qualityMetric <- function(df){
  
  wetland_area <- read_csv("blue_carbon_data_inventory/data/wetland_area/state_wetland_area.csv")
  
  core_quality <- df %>% 
    # count the number of cores per state with good quality data (for stocks, dates, or elevation)
    filter(stocks_qual_code == "C1" | 
             dates_qual_code == "B1" | 
             elevation_qual_code %in% c("A1", "A2")) %>% 
    group_by(state) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(state = state.abb[match(state,state.name)]) %>% 
    left_join(wetland_area) %>% 
    mutate(cores_per_1000ha = n / estimated_area_ha * 1000) %>% 
    arrange(-cores_per_1000ha) %>% 
    mutate(quality_metric_rank = row.names(.),
           quality_metric_weight = cores_per_1000ha/sum(cores_per_1000ha))  %>% 
    select(-c(estimated_area_ha, percent_wetland, n)) %>% 
    rename(quality_metric = cores_per_1000ha)
  
  return(core_quality)
}


## Spatial ####

spatialMetric <- function(df){
  
  projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # set cores as simple feature
  cores_sf <- st_as_sf(df,
                       coords = c("longitude", "latitude"),
                       crs = projcrs)
  
  # Common projection for cores
  # equal area projection
  cores_aea <- st_transform(cores_sf,
                            crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")
  
  # Import HUC8 watershed data
  huc8shp <- st_read("blue_carbon_data_inventory/data/shapefiles/coastal_HUC8s/HUC8_AllTidalNwiAndNonTidalPlusFarmedBelowMHHWS_ObviousOutliersRemoved.shp",
                     stringsAsFactors = F)
  
  # Common projection for watersheds
  huc8shp_aea <- st_transform(huc8shp,
                              crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")
  
  # Import states
  states <- st_read("blue_carbon_data_inventory/data/shapefiles/us_states/states_coastline_boundaries/cb_2017_us_state_500k.shp",
                    stringsAsFactors = F)
  
  # Common projection for states
  states_aea <- st_transform(states,
                             crs = "+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5 +lon_0=-96 +x_0=0 +y_0=0")
  
  # Calculate centroid of watersheds
  huc8shp_aea_centroid <- huc8shp_aea %>% 
    st_centroid() %>% 
    # Join with states
    st_join(states_aea, join = st_nearest_feature)
  
  # ggplot(data = huc8shp_aea_centroid) +
  #   geom_sf(aes(color = STUSPS))
  
  # Which states are involved in the analysis?
  states_to_analyse <- unique(huc8shp_aea_centroid$NAME)
  
  spatial_rep_by_state <- data.frame(state = states_to_analyse,
                                     mean_core_distance = rep(NA, length(states_to_analyse)),
                                     mean_huc8_distance = rep(NA, length(states_to_analyse)),
                                     spatial_representativeness_metric = rep(NA, length(states_to_analyse)))
  
  # Iterate through each state
  for (i in 1:length(states_to_analyse)) {
    
    # Filter cores by state
    cores_in_state <- cores_aea %>% 
      filter(state == states_to_analyse[i])
    
    # Filter HUC8's by state
    huc8s_in_state <- huc8shp_aea_centroid %>% 
      filter(NAME == states_to_analyse[i])
    
    # Distance matrix for cores
    core_distance_matrix <- st_distance(cores_in_state)
    
    # Mean distance for cores
    mean_core_distance <- mean(core_distance_matrix[upper.tri(core_distance_matrix)])
    
    # Distance matrix for HUC8's
    huc8_distance_matrix <- st_distance(huc8s_in_state)
    
    # Mean distance for HUC8's
    mean_huc8_distance <- mean(huc8_distance_matrix[upper.tri(huc8_distance_matrix)])
    
    # Mean Core Dist. / Mean Wetland Watershed Dist.
    spatial_representativeness_metric <- mean_core_distance / mean_huc8_distance
    
    # Add to table
    spatial_rep_by_state$mean_core_distance[i] <- mean_core_distance
    spatial_rep_by_state$mean_huc8_distance[i] <- mean_huc8_distance 
    spatial_rep_by_state$spatial_representativeness_metric[i] <- spatial_representativeness_metric
    
  }
  
  # Rank and Graph
  spatial_rep_by_state$spatial_representativeness_metric[is.na(spatial_rep_by_state$spatial_representativeness_metric)] <- 0
  
  spatial_metric_table <- spatial_rep_by_state %>% 
    arrange(-spatial_representativeness_metric) %>% 
    mutate(state = state.abb[match(state,state.name)]) %>% 
    # add state-level ranks and weights
    mutate(spatial_metric_rank = row.names(.),
           spatial_metric_weight = spatial_representativeness_metric/sum(spatial_representativeness_metric))
    # arrange(spatial_representativeness_metric)
  
  # cut off for fxn
  return(spatial_metric_table)
}

## Habitat Mapped Area Estimation ####

habitatProportions <- function(df){
  
  output_states_scaled_simplified <- read_csv("blue_carbon_data_inventory/data/wetland_area/state_habitat_wetland_area.csv")
  
  # Simplify habitat categories
  cores_habitat_simplified <- df %>% 
    # If any of: marsh, scrub/shrub, swamp, mangrove, then let the definition stand
    mutate(habitat_simplified = ifelse(habitat %in% c("marsh", "scrub/shrub", "mangrove", "swamp", "scrub shrub"),
                                       habitat,
                                       # If either, seagrass or algal mats, then redefine as seagrass, kelp and algal mats
                                       ifelse(habitat %in% c("seagrass", "algal mat", "EAB"),
                                              "seagrass, kelp and algal mats",
                                              # If other then NA
                                              NA
                                       )),
           state = state.abb[match(state,state.name)]) %>% 
    filter(complete.cases(habitat_simplified)) %>% 
    # Group by state and calculate habitat proportions by core counts
    group_by(state, habitat_simplified) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(state) %>% 
    mutate(state_count = sum(n)) %>% 
    ungroup() %>% 
    mutate(state_core_proportion = n / state_count) %>% 
    arrange(state, -state_core_proportion)
  
  # Run a full join between the mapped proportions and core representativeness
  full_mapped_and_core_proprtions <- full_join(output_states_scaled_simplified, cores_habitat_simplified) %>% 
    select(state, habitat_simplified, mapped_proportion, state_core_proportion)
  
  # Turn NA values for core representativeness into 0's
  full_mapped_and_core_proprtions[is.na(full_mapped_and_core_proprtions)] <- 0
  
  return(full_mapped_and_core_proprtions)
}
  
# Habitat Metric ####

habitatMetric <- function(df){
  # call function which calculates mapped vs sampled proportions
  full_mapped_and_core_proprtions <- habitatProportions(df)
  
  state_level_euclidean_distance <- full_mapped_and_core_proprtions %>% 
    # mutate to get the difference for each state
    mutate(exp_minus_obs = mapped_proportion - state_core_proportion) %>% 
    # Group by state
    group_by(state) %>%
    # calculate the sqrt(sum of differences squared), aka euclidean distance
    summarise(euclidean_distance = 1 - sqrt(sum(exp_minus_obs^2))) %>% 
    # arrange by euclidean distance
    arrange(-euclidean_distance) %>% 
    
    # calculate ranks and weights for habitat metric (aka. euclidean dist)
    rename(habitat_metric = euclidean_distance) %>%
    # arrange(-habitat_metric) %>%
    mutate(habitat_metric_rank = row.names(.),
           habitat_metric_weight = habitat_metric / sum(habitat_metric))

  return(state_level_euclidean_distance)  
}

## Composite Scoring ####

compositeMetricScore <- function(df){
  
  # calculate composite score
  rankings <- df %>% 
    # set the maximum rank
    mutate_at(vars(quantity_metric_rank, quality_metric_rank, spatial_metric_rank, habitat_metric_rank),
              ~ifelse(is.na(.), 23, .)) %>% 
    mutate_at(vars(-state), as.numeric) %>% 
    mutate(total_metric_rank = (quantity_metric_rank + quality_metric_rank + spatial_metric_rank + habitat_metric_rank) / 4,
           total_metric_weight = quantity_metric_weight + quality_metric_weight + spatial_metric_weight + habitat_metric_weight) %>% 
    arrange(-total_metric_rank)
  
  # Normalized average metric weight was not used for the final figure
  # mutate(total_metric_weight = total_metric_weight / sum(total_metric_weight)) %>%
  # arrange(-total_metric_weight)
  
  # rankings$total_metric_weight <- rankings$total_metric_weight / sum(rankings$total_metric_weight)
  
  # rankings <- rankings %>%
  #   arrange(-total_metric_weight)
  
  # all_data_weights <- rankings %>% 
  #   select(state, quantity_metric_weight, quality_metric_weight, spatial_metric_weight, habitat_metric_weight, total_metric_weight) %>% 
  #   mutate(state = factor(state, rev(rankings$state))) %>% 
  #   gather(value = "weight", key = "metric", -state) %>% 
  #   mutate(metric = str_remove_all(metric, "_metric_weight")) %>% 
  #   filter(weight > 0)
  
  all_data_ranks <- rankings %>% 
    select(state, quantity_metric_rank, quality_metric_rank, spatial_metric_rank, habitat_metric_rank, total_metric_rank) %>% 
    mutate(state = factor(state, rankings$state)) %>% 
    gather(value = "rank", key = "metric", -state) %>% 
    mutate(metric = str_remove_all(metric, "_metric_rank"),
           metric = str_to_sentence(metric),
           metric = recode(metric,"Spatial" = "Spatial coverage", "Habitat" = "Habitat coverage"),
           metric = factor(metric, c("Total", "Quantity", "Quality", "Spatial coverage", "Habitat coverage")),
           rank = round(rank,0))
  
  final_ranks <- all_data_ranks %>% 
    mutate(rank = ifelse(state %in% c("DC", "PA"), NA, rank)) %>% 
    group_by(metric) %>%
    mutate(min_rank = min(rank, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(normalized_rank = round((rank-min_rank)/(23-min_rank)*6)) %>% 
    select(-min_rank, -rank)
  
  return(final_ranks)
}

