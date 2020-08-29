#Barriers
#Mike McQueen

#=====================#
#Load packages####
#=====================#
library(ggmap)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(viridis)
library(ggsn)

#=====================#
#Auxiliary Helper Functions####
#=====================#
#This helper function found online from https://github.com/dkahle/ggmap/issues/160
# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

#=====================#
#Load and Clean Shapes####
#=====================#

#load map shapes
MAX_lines <- read_sf("geometries/MAX/tm_rail_lines.shp") %>% 
  filter(TYPE %in% c("MAX", "MAX/SC"),
         LINE != "AUX")
MAX_stops <- read_sf("geometries/MAX/tm_rail_stops.shp") %>% 
  filter(TYPE == "MAX",
         STATUS == "Existing")
A_loop <- read_sf("geometries/Streetcar/A Loop Route.shp")
bike_network <- read_sf("geometries/BikeNetwork/Metro/bike_routes.shp") %>% 
  filter(BIKETYP %in% c("BKE-BLVD", "BKE-LANE", "BKE-BUFF", "BKE-SHRD", "PTH-LOMU", "PTH-REMU", "OTH-CONN", "OTH-XING", "OTH-SWLK")) #Filter for low-stress infrastructure

#=====================#
#Load and Clean Data####
#=====================#
#Set current data frame from last script
d <- step4_output

#Load the geocoded data
d_geo_raw <- read.csv("../../GeoCoding/geocoded/backup/geocoded_backup_060320_2.csv")

#clean the geocoded data
d_geo <- d_geo_raw %>% 
  filter(loctype == "geometric_center", #use only geometric center type matches
         type != "route")

#join the survey data to the geocoded data
d_geo_full <- d_geo %>% 
  left_join(d %>% 
              select(-Q71, -Q7_4),
            by = "ResponseId") %>% 
  select(-X, -X.1) #cleanup useless columns

#=====================#
#Reorient Data####
#=====================#

#Helper function to grab barrier data to plot by specifying the groups
get_barrier_data <- function(data, ..., question_str) {
  barrier_data <- data %>% 
      select(ResponseId, lat, lon, ...) %>% #select ResponseId, lat, lon, and barrier columns
      pivot_longer(cols = ..., names_to = "barriers") %>% #Turn several columns into one columns
      mutate(barriers = recode(barriers, !!!var_labels)) %>% #Recode the values from question numbers to question text
      mutate(barriers = str_remove(barriers, #Get rid of the question where it is the same
                                   fixed(question_str)))
  return(barrier_data)
}

#MAX
d_geo_MAX_neighborhood_barrier <- get_barrier_data(data = d_geo_full,
                                                   Q33_2:Q33_3,
                                                   question_str = "How does the neighborhood where you live prevent you from riding MAX more frequently? (Select all that apply) - Selected Choice ")
d_geo_MAX_personal_barrier <- get_barrier_data(data = d_geo_full,
                                               Q32_1:Q32_11,
                                               question_str = "What personal reasons prevent you from riding MAX more frequently? (Select all that apply) - Selected Choice ")
#Bike
d_geo_Bike_neighborhood_barrier <- get_barrier_data(data = d_geo_full,
                                                   Q20_2:Q20_9,
                                                   question_str = "How does the neighborhood where you live prevent you from biking more frequently? (Select all that apply) - Selected Choice ")
d_geo_Bike_personal_barrier <- get_barrier_data(data = d_geo_full,
                                               Q19_1:Q19_16,
                                               question_str = "What personal reasons prevent you from biking more frequently? (Select all that apply) - Selected Choice ")
#E-Scooter
d_geo_EScooter_neighborhood_barrier <- get_barrier_data(data = d_geo_full,
                                                   Q26_2:Q26_6,
                                                   question_str = "How does the neighborhood where you live prevent you from using an e-scooter more frequently? (Select all that apply) - Selected Choice ")
d_geo_EScooter_personal_barrier <- get_barrier_data(data = d_geo_full,
                                               Q25_1:Q25_12,
                                               question_str = "What personal reasons prevent you from using an e-scooter more frequently? (Select all that apply) - Selected Choice ")

#=====================#
#Define base map####
#=====================#

#Define map boundaries
bbox <- c(left = -123.0500,
          right = -122.3500,
          bottom = 45.400,
          top = 45.625)

#Define the lower right hand corner coordinates in meters (not degrees) so that the map scale can be drawn correctly
bbox_coords <- data.frame(lon = bbox[1], lat = bbox[4]) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(3857) %>%
  st_coordinates()

t <- get_stamenmap(bbox,
                   zoom = 12,
                   maptype = "toner-background",
                   force = F)

map <- ggmap_bbox(t)

#=====================#
#Helper function for creating maps####
#=====================#
barrier_hex_map <- function(basemap, data, under_feature_type = c("none", "MAX", "bike"), title) {
  
  #=====================#
  #Some knobs to play around with####
  #=====================#
  scale_text_size = 2.5
  scale_nudge_right = 1000
  scale_nudge_down = 3000
  
  #=====================#
  #Turn Data into Simple Feature####
  #=====================#
  
  #Convert data to Simple Feature
  data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
  data_sf_3857 <- st_transform(data_sf, 3857)
  
  #=====================#
  #Attach under_feature if desired####
  #=====================#
  under_feature <- case_when(
    under_feature_type == "MAX" ~ list(geom_sf(data = MAX_lines,
                                          aes(geometry = geometry),
                                          color = 'orangered',
                                          inherit.aes = F,
                                          lwd = 1.5),
                                  geom_sf(data = MAX_stops,
                                          aes(geometry = geometry),
                                          fill = 'white',
                                          color = 'orangered',
                                          size = 2,
                                          shape = 21,
                                          inherit.aes = F)),
    under_feature_type == "bike" ~ geom_sf(data = bike_network,
                                           aes(geometry = geometry),
                                           color = 'green3',
                                           inherit.aes = F,
                                           lwd = .5)
  )
  
  #=====================#
  #Make Maps####
  #=====================#
  for (i in unique(data_sf_3857$barriers)) {
    #Filter the data for each barrier
    data <- data_sf_3857 %>% 
      filter(barriers == i)
  
  map3 <- ggmap(basemap) +
    #make the projection of the map correct
    coord_sf(datum = st_crs(3857)) +
    #add heatmap of points
    stat_summary_hex(data = data,
                     aes(x = st_coordinates(data)[,1],
                         y = st_coordinates(data)[,2],
                         z = value),
                     fun = function(z) (sum(z == 1) / length(z)) * tanh(length(z) / 5),
                     geom = "hex",
                     color = "black",
                     alpha = 0.5,
                     binwidth = c(3e3, 3e3)) +
    scale_fill_viridis(option = 'inferno',
                       limits = c(0,1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    #Add map scale
    scalebar(data = data,
             location = "topleft",
             dist = 3,
             transform = F,
             dist_unit = "mi",
             anchor = c(
               x = bbox_coords[1] + scale_nudge_right,
               y = bbox_coords[2] - scale_nudge_down),
             height = 0.0005,
             st.dist = 0.0007,
             st.size = scale_text_size) +
    #get rid of axis labels
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    #Update lables
    labs(fill = "Weighted Ratio \nof Respondents",
         title = title,
         subtitle = i)
  
  map2 <- ggmap(basemap) +
    #make the projection of the map correct
    coord_sf(datum = st_crs(3857)) +
    #add heatmap of points
    stat_summary_hex(data = data,
                     aes(x = st_coordinates(data)[,1],
                         y = st_coordinates(data)[,2],
                         z = value),
                     fun = function(z) (sum(z == 1) / length(z)),
                     geom = "hex",
                     color = "black",
                     alpha = 0.5,
                     binwidth = c(3e3, 3e3)) +
    scale_fill_viridis(option = 'inferno',
                       limits = c(0,1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    #Add map scale
    scalebar(data = data,
             location = "topleft",
             dist = 3,
             transform = F,
             dist_unit = "mi",
             anchor = c(
               x = bbox_coords[1] + scale_nudge_right,
               y = bbox_coords[2] - scale_nudge_down),
             height = 0.0005,
             st.dist = 0.0007,
             st.size = scale_text_size) +
    #get rid of axis labels
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    #Update lables
    labs(fill = "Ratio of\nRespondents",
         title = title,
         subtitle = i)
  
  #Backup of old method for counts
  map1 <- ggmap(basemap) +
    #make the projection of the map correct
    coord_sf(datum = st_crs(3857)) +
    #add heatmap of points
    stat_summary_hex(data = data,
                 aes(x = st_coordinates(data)[,1],
                     y = st_coordinates(data)[,2],
                     z = value),
                 fun = function(z) sum(z == 1),
                 geom = "hex",
                 color = "black",
                 alpha = 0.5,
                 binwidth = c(3e3, 3e3)) +
    scale_fill_viridis(option = 'inferno') +
    #Add map scale
    scalebar(data = data,
             location = "topleft",
             dist = 3,
             transform = F,
             dist_unit = "mi",
             anchor = c(
               x = bbox_coords[1] + scale_nudge_right,
               y = bbox_coords[2] - scale_nudge_down),
             height = 0.0005,
             st.dist = 0.0007,
             st.size = scale_text_size) +
    #get rid of axis labels
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    #Update lables
    labs(fill = "Count",
         title = title,
         subtitle = i)
  
  #Add other geometries if desired
  if (under_feature_type != "none") {
    map1 <-  map1 + under_feature
    map2 <- map2 + under_feature
    map3 <- map3 + under_feature
  } 
  
  ggsave(map1, filename = paste0("Exports/Barriers/count/", title, "_count_", i, ".jpg"),
         dpi = 800,
         height = 5,
         units = "in"
         )
  ggsave(map2, filename = paste0("Exports/Barriers/ratio/", title, "_ratio_", i, ".jpg"),
         dpi = 800,
         height = 5,
         units = "in"
         )
  ggsave(map3, filename = paste0("Exports/Barriers/weighted_ratio/", title, "_weighted ratio_", i, ".jpg"),
         dpi = 800,
         height = 5,
         units = "in")
  }
}

#=====================#
#Make the darn maps already####
#=====================#

#MAX
barrier_hex_map(map, d_geo_MAX_neighborhood_barrier, under_feature_type = "MAX", title = "MAX Neighborhood Barriers")
barrier_hex_map(map, d_geo_MAX_personal_barrier, under_feature_type = "MAX", title = "MAX Personal Barriers")

#Bike
barrier_hex_map(map, d_geo_Bike_neighborhood_barrier, under_feature_type = "none", title = "Bike Neighborhood Barriers")
barrier_hex_map(map, d_geo_Bike_personal_barrier, under_feature_type = "none", title = "Bike Personal Barriers")

#E-Scooter
barrier_hex_map(map, d_geo_EScooter_neighborhood_barrier, under_feature_type = "none", title = "E-scooter Neighborhood Barriers")
barrier_hex_map(map, d_geo_EScooter_personal_barrier, under_feature_type = "none", title = "E-scooter Personal Barriers")

#Special one-offs that include the Bike network
barrier_hex_map(map, d_geo_Bike_neighborhood_barrier %>% filter(barriers == "Not enough bike lanes or trails"), under_feature_type = "bike", title = "Bike Neighborhood Barriers")
barrier_hex_map(map, d_geo_EScooter_neighborhood_barrier %>% filter(barriers == "Not enough dedicated lanes to ride in"), under_feature_type = "bike", title = "E-scooter Neighborhood Barriers")

#=====================#
#Make a map showing the locations of the respondents####
#=====================#

points_map <- get_stamenmap(bbox,
                            zoom = 12,
                            maptype = "toner")

respondents_map <- ggmap(points_map) +
  #make the projection of the map correct
  geom_point(data = d_geo_full,
             aes(x = lon, y = lat),
             color = "dodgerblue2",
             inherit.aes = F) +
  #get rid of axis labels
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Approximate Location of Geocoded Responses",
       subtitle = "n = 1087")

ggsave(respondents_map, filename = "Exports/Barriers/respondents_map.jpg",
       dpi = 800,
       height = 5,
       units = "in")
  
