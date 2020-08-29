#Mode Probabilities Spatial
#Mike McQueen

#=====================#
#Load packages####
#=====================#

library(dplyr)
library(tidyr)
library(sf)
library(ggmap)
library(ggplot2)
library(maptools)
library(stplanr)
library(purrr)
library(units)
units_options(allow_mixed = T)
library(nngeo)
library(akima)
library(viridis)
library(ggsn)
library(scales)
library(nnet)

#=====================#
#Load the MNL models####
#=====================#

load("Exports/MNL/models.RData")

#=====================#
#Load and Clean Shapes####
#=====================#

streets_sf <- read_sf(("geometries/RoadNetwork/Streets.shp"))

bike_network_sf <- st_transform(read_sf("geometries/BikeNetwork/Metro/bike_routes.shp"), 3857) %>% 
  filter(BIKETYP %in% c("BKE-BLVD", "BKE-LANE", "BKE-BUFF", "BKE-SHRD", "PTH-LOMU", "PTH-REMU", "OTH-CONN", "OTH-XING", "OTH-SWLK")) #Filter for low-stress infrastructure

water_sf <- st_transform(read_sf(("geometries/WaterBodies/commondata/data0/mjriv_fi_prj.shp")), 3857)

MAX_lines_sf <- st_transform(read_sf("geometries/MAX/tm_rail_lines.shp") %>% 
  filter(TYPE %in% c("MAX", "MAX/SC"),
         LINE != "AUX"), 3857)

MAX_stops_sf <- st_transform(read_sf("geometries/MAX/tm_rail_stops.shp") %>% 
  filter(TYPE == "MAX",
         STATUS == "Existing"), 3857)

PSU_rec_center_sf <- st_transform(st_sf(geometry = st_sfc(st_point(c(-122.682266, 45.511299)), crs = 4326), Name = "PSU Rec Center"), 3857)

#=====================#
#Define universal parameters####
#=====================#

#Make your selection
map_view <- "eastside"


#From the selection, define the parameters to be used later on
if(map_view == "eastside") {
bbox <- c(xmin =  -122.64,
                   ymin = 45.51,
                   xmax = -122.47,
                   ymax = 45.545)
num_points <-  1000
zoom <-  14
anchor_box_position <- "upperright"
map_scale_length  <- .5
plot_width <- 12
} else if (map_view == "PDX") {
bbox <- c(xmin = -123.0500,
              ymin = 45.400,
              xmax = -122.3500,
              ymax = 45.625)
num_points <-  3000
zoom <-  12
anchor_box_position <- "lowerleft"
map_scale_length  <- 2.5
plot_width <- 8
}

# Coonvert the bbox to an sf polygon, transform it to 3857, 
# and convert back to a bbox
bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(bbox, crs = 4326)), 3857))

#=====================#
#Crop the shapes to be used####
#=====================#

crop_features <- function(bbox_3857) {
#crop the shapes that will be plotted
streets_sf_cropped <<- st_crop(streets_sf, bbox_3857)
water_sf_cropped <<- st_crop(water_sf, bbox_3857)
MAX_lines_sf_cropped <<- st_crop(MAX_lines_sf, bbox_3857)
MAX_stops_sf_cropped <<- st_crop(MAX_stops_sf, bbox_3857)
bike_network_sf_cropped <<- st_crop(bike_network_sf, bbox_3857)
}

crop_features(bbox_3857)

#=====================#
#Generate test points and snap to road network####
#=====================#


gen_test_points <- function(bbox_3857, num_points) {
#Generate set of test points that are bound to the street network
#sample random points within bbox
test_points_sf <- st_sample(st_as_sfc(bbox_3857), size = num_points, type = "regular", exact = TRUE)

#snap the test points to the nearest road
# Max distance
cut_dist = 200 # meters

# convert sf to sp
streets_sp_cropped <-  as_Spatial(streets_sf_cropped)
test_points_sp <-  as_Spatial(test_points_sf)

# snap test points to closest road
test_points_snapped <- snapPointsToLines(test_points_sp, streets_sp_cropped, maxDist = cut_dist)
test_points_snapped_sf <- st_as_sf(test_points_snapped)

#give each test point an id number
test_points_snapped_sf <- test_points_snapped_sf %>% 
  mutate(id = paste0("test_point_", 1:n()))
return(test_points_snapped_sf)
}

test_points_snapped_sf <- gen_test_points(bbox_3857, num_points)

#=====================#
#Find the closest MAX station as the crow flies####
#=====================#

test_points_snapped_sf_joined <- st_join(test_points_snapped_sf, MAX_stops_sf, st_nn, k = 1)

#Look to see how well this worked
closest <- ggplot() +
  geom_sf(data = streets_sf_cropped) + #streets
  geom_sf(data = test_points_snapped_sf_joined) + #test points
  geom_segment(aes(x = st_coordinates(test_points_snapped_sf_joined)[,1], #line segments connecting test points to matched closest MAX stop
                   y = st_coordinates(test_points_snapped_sf_joined)[,2],
                   xend = st_coordinates(test_points_snapped_sf_joined %>%
                                           as_tibble() %>% 
                                           select(-geometry) %>%
                                           left_join(MAX_stops_sf %>%
                                                       select(STATION, geometry), by = "STATION") %>% 
                                           distinct(id,
                                                    .keep_all = T) %>% 
                                           st_as_sf())[,1],
                   yend = st_coordinates(test_points_snapped_sf_joined %>%
                                           as_tibble() %>% 
                                           select(-geometry) %>%
                                           left_join(MAX_stops_sf %>%
                                                       select(STATION, geometry), by = "STATION") %>% 
                                           distinct(id,
                                                    .keep_all = T) %>% 
                                           st_as_sf())[,2]),
               color = "dodgerblue2",
               size = 1
  ) +
  geom_sf(data = MAX_lines_sf_cropped, color = "orangered", size = 1.5) + #MAX line
  geom_sf(data = MAX_stops_sf_cropped, shape = 21, size = 3, color = "orangered", fill = "white") + #MAX stops
  theme(axis.title.x = element_blank(), #get rid of axis labels
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlim(bbox_3857[[1]], bbox_3857[[3]]) +
  ylim(bbox_3857[[2]], bbox_3857[[4]]) +
  labs(title = "Test point snapping and nearest MAX stop matching")

ggsave(filename = paste0("Exports/Prepped data for Google/closest_", map_view, ".png"),
       height = 5,
       width = 10,
       units = "in",
       dpi = 400)

#Save the result - we don't want to keep doing this operation.
#save(test_points_snapped_sf_joined, file = paste0("Exports/Prepped data for Google/",map_view, "testpoints.RData"))

#=====================#
#Create list of test points for API OD analysis####
#=====================#

#We don't want to generate random test points every time. Use these that have been saved to file.
load(paste0("Exports/Prepped data for Google/",map_view, "testpoints.RData"))

#Create a simple tibble that just contains test point, coordinate, nearest station, coordinate, PSU rec center coordinates
test_points_list_for_API <- test_points_snapped_sf_joined %>%
  st_transform(4326) %>% #Convert test_point sf to 4326 (with lat/lon)
  rename("nearest_station" = STATION) %>% 
  select(id, nearest_station, geometry) %>% 
  mutate(test_point_lat = st_coordinates(geometry)[,2], #Pull out the lat
         test_point_lon = st_coordinates(geometry)[,1]) %>%  #Pull out the lon
  as_tibble() %>% #force the sf back into a tibble so can delete the geometry column
  select(-geometry) %>%   #remove the test points sf geometry column, no longer needed.
  left_join(st_transform(MAX_stops_sf, 4326) %>% select(STATION, geometry), by = c("nearest_station" = "STATION")) %>%  #convert station sf to 4326 (with lat/lon) and then join the station sf geometry
  mutate(nearest_station_lat = st_coordinates(geometry)[,2], #Pull out the lat
         nearest_station_lon = st_coordinates(geometry)[,1]) %>%  #Pull out the lon
  select(-geometry) %>%  #remove the nearest station test points sf geometry column, no longer needed
  mutate(PSU_rec_center_coordinates = st_transform(PSU_rec_center_sf, 4326)[2]) %>% 
  mutate(PSU_rec_center_lat = st_coordinates(PSU_rec_center_coordinates)[,2], #Pull out the lat
         PSU_rec_center_lon = st_coordinates(PSU_rec_center_coordinates)[,1]) %>%  #Pull out the lon
  as_tibble() %>% #force the sf back into a tibble so can delete the geometry column
  select(-PSU_rec_center_coordinates) #remove the geometry column, no longer needed

#Create a list of all the MAX stops, and just add the PSU rec center coordinates
MAX_stops_list_for_API <- st_transform(MAX_stops_sf, 4326) %>%
  select(STATION, geometry) %>%  #convert station sf to 4326 (with lat/lon) and then join the station sf geometry
  mutate(station_lat = st_coordinates(geometry)[,2], #Pull out the lat
         station_lon = st_coordinates(geometry)[,1]) %>%  #Pull out the lon
  as_tibble() %>% #force the sf back into a tibble so can delete the geometry column
  select(-geometry) %>%  #remove the nearest station test points sf geometry column, no longer needed
  mutate(PSU_rec_center_coordinates = st_transform(PSU_rec_center_sf, 4326)[2]) %>% 
  mutate(PSU_rec_center_lat = st_coordinates(PSU_rec_center_coordinates)[,2], #Pull out the lat
         PSU_rec_center_lon = st_coordinates(PSU_rec_center_coordinates)[,1]) %>%  #Pull out the lon
  as_tibble() %>% #force the sf back into a tibble so can delete the geometry column
  select(-PSU_rec_center_coordinates) #remove the geometry column, no longer needed

#write out the OD matrix to file
#save(test_points_list_for_API, MAX_stops_list_for_API, file = paste0("Exports/Prepped data for Google/", map_view, "test_points_list_for_API.RData"))

#=====================#
#DOESN'T WORK####
#Find the closest MAX station via the street network
#=====================#

#convert the streets sf to a spatial lines network (sln) class
#network <- SpatialLinesNetwork(streets_sf)
#network_cleaned <<- sln_clean_graph(network)

#Make a table containing all combinations of test point origins and station destinations (OD)
#od <- expand_grid(STATION = MAX_stops_sf$STATION, id = test_points_snapped_sf$id) %>% 
#  left_join(MAX_stops_sf %>% select(STATION, geometry), by = "STATION") %>% 
#  left_join(test_points_snapped_sf %>% select(id, geometry), by = "id") %>% 
#  rename(geometry.STATION = geometry.x,
#         geometry.test_point = geometry.y) %>% 
#  mutate(geometry.STATION = st_coordinates(geometry.STATION),
#         geometry.test_point = st_coordinates(geometry.test_point)) %>% 
#  slice(1:100)

#Make a helper function to pull out the total distance only from the route finding function
#distance_to_station <- function(geometry.test_point, geometry.STATION) {
#  length <- sum(route_local(sln = network_cleaned,
#              from = geometry.test_point,
#              to = geometry.STATION)$LENGTH)
#  length <- as.numeric(length)
#  return(length)
#}


#route_lengths <- pmap(list(od$geometry.STATION, od$geometry.test_point), .f = distance_to_station)

#test <- route_local(sln = network_cleaned, from = c(od$geometry.STATION[,1], od$geometry.STATION[,2]), to = c(od$geometry.test_point[,1], od$geometry.test_point[,2]))

#This is super inefficient but I couldn't get it to work in a vectorized way!!!
#Loop through each OD pair and find the distance between each test point and each MAX station.
#route_lengths <- data.frame(length = rep(NA, nrow(od)))
#for (i in 1:nrow(od)) {
#  route_lengths$length[i] <- distance_to_station(sln, od$geometry.test_point[i,], od$geometry.STATION[i,])
#  print(i)
#}

#Sort for the test point/MAX station pair with the shortest distance. In effect, finds the closest MAX station
#by way of the road network
#od2 <- od %>% 
#  bind_cols(lengths = route_lengths) %>%  #Bind the list of length to the od list
#  group_by(id) %>%
#  filter(length == min(length))

#Now, join the name of the closest station to each test point as determined by the processes above to the test_points_snapped_sf (simple feature)
#test_points_snapped_sf2 <- test_points_snapped_sf %>% 
#  left_join(od2 %>% select(id, STATION), by = "id") %>% #Join the nearest station name to the test point
#  rename("nearest_station" = STATION) %>%  #rename the station name column to "nearest station"
#  st_transform(4326) #convert the coordinates back from meters to lat/lon (crs 4326)

#Create a simple tibble that just contains test point, coordinate, nearest station, coordinate
#test_points_list_for_API <- test_points_snapped_sf2 %>% 
#  select(id, nearest_station, geometry) %>% 
#  mutate(test_point_lat = st_coordinates(geometry)[,2], #Pull out the lat
#         test_point_lon = st_coordinates(geometry)[,1]) %>%  #Pull out the lon
#  as_tibble() %>% 
#  select(-geometry) %>%   #remove the test points sf geometry column, no longer needed.
#  left_join(st_transform(MAX_stops_sf, 4326) %>% select(STATION, geometry), by = c("nearest_station" = "STATION")) %>%  #convert station sf to 4326 (with lat/lon) and then join the station sf geometry
#  mutate(nearest_station_lat = st_coordinates(geometry)[,2], #Pull out the lat
#         nearest_station_lon = st_coordinates(geometry)[,1]) %>%  #Pull out the lon
#  select(-geometry) #remove the nearest station test points sf geometry column, no longer needed
  
#ggplot() +
#geom_segment(data = test_points_list_for_API,
#             aes(x = nearest_station_lon, y = nearest_station_lat, xend = test_point_lon, yend = test_point_lat),
#             color = "black")

#=====================#
#After using the Google API, load the data back in####
#=====================#

load("Exports/OD Data from Google/MAX.RData")
load(paste0("Exports/OD Data from Google/", map_view, "_bike.RData"))
load(paste0("Exports/OD Data from Google/", map_view, "_car.RData"))
load(paste0("Exports/OD Data from Google/", map_view, "_escooter.RData"))

#=====================#
#Perform Row-Wise elimination (as was performed when the model was estimated)####
#=====================#

d <- d_complete %>% 
  drop_na(car_walk_time, car_drive_time, 
          car_parking_cost, bike_ride_time, em_walk_time, em_scoot_time, 
          em_scoot_cost, em_ticket_cost, em_ride_time, Q68_inc_2, Q68_inc_3, 
          Q68_inc_4, Q68_inc_5, Q68_inc_6, RECODED_Q63_am_ind_ala, 
          RECODED_Q63_asian, RECODED_Q63_black, RECODED_Q63_hisp_lat, 
          RECODED_Q63_more_than_one_race, RECODED_Q63_nat_haw_pac, 
          RECODED_Q63_other, Q65_female, Q65_nonbinary, Q65_other, 
          Q11, Q64, Q66, Q67, Q69, Q70, Q5, Q7_4, RECODED_Q6_activetransport, RECODED_Q6_publictransit, RECODED_Q6_other, 
          Q13, Q17, Q23, Q30, attitude_fac_car, attitude_fac_bike, 
          attitude_fac_escooter, attitude_fac_MAX, Q8_FAC1, Q8_FAC2)

#=====================#
#Make the table complete by
#adding the info from Google and
#calculating the values predicted by the model####
#=====================#

test_points_final_sf <- test_points_snapped_sf_joined %>% #the sf object that contains test point geometry and attributes
  left_join(d_max %>% select(STATION, MAX_TT_sec), by = "STATION") %>% #join MAX travel time info from closest MAX station
  left_join(d_car %>% select(id, car_distance_m = distance_m, car_TT_sec, car_traffic_TT_sec), by = "id") %>% #join car travel time info to PSU rec center
  left_join(d_bike %>% select(id, bike_distance_m, bike_TT_sec), by = "id") %>%  #join bike travel time info to PSU rec center
  left_join(d_escooter %>% select(id, escooter_distance_m, escooter_TT_sec), by = "id")  #join escooter travel time info to closest MAX station

mode_selection_probability_map <- function(test, d, test_points_final_sf, bbox, map_view, zoom, anchor_box_position, map_scale_length, plot_width) {
#Remember - can only pass 1 row at a time. Must call function in a for loop.
  
#Clean the input test as required
  test <-  test %>% 
    #Replace "mean" cells with the actual mean from the data:
    mutate(car_walk_time = ifelse(car_walk_time == "mean", mean(d$car_walk_time), as.numeric(car_walk_time)),
           car_parking_cost = ifelse(car_parking_cost == "mean", 7, as.numeric(car_parking_cost)),
           em_walk_time = ifelse(em_walk_time == "mean", mean(d$em_walk_time), as.numeric(em_walk_time)),
           Q68_inc_2 = ifelse(Q68_inc_2 == "mean", mean(d$Q68_inc_2), as.numeric(Q68_inc_2)),
           Q68_inc_3 = ifelse(Q68_inc_3 == "mean", mean(d$Q68_inc_3), as.numeric(Q68_inc_3)),
           Q68_inc_4 = ifelse(Q68_inc_4 == "mean", mean(d$Q68_inc_4), as.numeric(Q68_inc_4)),
           Q68_inc_5 = ifelse(Q68_inc_5 == "mean", mean(d$Q68_inc_5), as.numeric(Q68_inc_5)),
           Q68_inc_6 = ifelse(Q68_inc_6 == "mean", mean(d$Q68_inc_6), as.numeric(Q68_inc_6)),
           RECODED_Q63_am_ind_ala = ifelse(RECODED_Q63_am_ind_ala == "mean", mean(d$RECODED_Q63_am_ind_ala), as.numeric(RECODED_Q63_am_ind_ala)),
           RECODED_Q63_asian = ifelse(RECODED_Q63_asian == "mean", mean(d$RECODED_Q63_asian), as.numeric(RECODED_Q63_asian)),
           RECODED_Q63_black = ifelse(RECODED_Q63_black == "mean", mean(d$RECODED_Q63_black), as.numeric(RECODED_Q63_black)),
           RECODED_Q63_hisp_lat = ifelse(RECODED_Q63_hisp_lat == "mean", mean(d$RECODED_Q63_hisp_lat), as.numeric(RECODED_Q63_hisp_lat)),
           RECODED_Q63_more_than_one_race = ifelse(RECODED_Q63_more_than_one_race == "mean", mean(d$RECODED_Q63_more_than_one_race), as.numeric(RECODED_Q63_more_than_one_race)),
           RECODED_Q63_nat_haw_pac = ifelse(RECODED_Q63_nat_haw_pac == "mean", mean(d$RECODED_Q63_nat_haw_pac), as.numeric(RECODED_Q63_nat_haw_pac)),
           RECODED_Q63_other = ifelse(RECODED_Q63_other == "mean", mean(d$RECODED_Q63_other), as.numeric(RECODED_Q63_other)),
           Q65_female = ifelse(Q65_female == "mean", mean(d$Q65_female), as.numeric(Q65_female)),
           Q65_nonbinary = ifelse(Q65_nonbinary == "mean", mean(d$Q65_nonbinary), as.numeric(Q65_nonbinary)),
           Q65_other = ifelse(Q65_other == "mean", mean(d$Q65_other), as.numeric(Q65_other)),
           Q11 = ifelse(Q11 == "mean", mean(d$Q11), as.numeric(Q11)),
           Q64 = ifelse(Q64 == "mean", mean(d$Q64), as.numeric(Q64)),
           Q66 = ifelse(Q66 == "mean", mean(d$Q66), as.numeric(Q66)),
           Q67 = ifelse(Q67 == "mean", mean(d$Q67), as.numeric(Q67)),
           Q69 = ifelse(Q69 == "mean", mean(d$Q69), as.numeric(Q69)),
           Q70 = ifelse(Q70 == "mean", mean(d$Q70), as.numeric(Q70)),
           Q5 = ifelse(Q5 == "mean", mean(d$Q5), as.numeric(Q5)),
           Q7_4 = ifelse(Q7_4 == "mean", mean(d$Q7_4), as.numeric(Q7_4)),
           RECODED_Q6_activetransport = ifelse(RECODED_Q6_activetransport == "mean", mean(d$RECODED_Q6_activetransport), as.numeric(RECODED_Q6_activetransport)),
           RECODED_Q6_publictransit = ifelse(RECODED_Q6_publictransit == "mean", mean(d$RECODED_Q6_publictransit), as.numeric(RECODED_Q6_publictransit)),
           RECODED_Q6_other = ifelse(RECODED_Q6_other == "mean", mean(d$RECODED_Q6_other), as.numeric(RECODED_Q6_other)),
           Q13 = ifelse(Q13 == "mean", mean(d$Q13), as.numeric(Q13)),
           Q17 = ifelse(Q17 == "mean", mean(d$Q17), as.numeric(Q17)),
           Q23 = ifelse(Q23 == "mean", mean(d$Q23), as.numeric(Q23)),
           Q30 = ifelse(Q30 == "mean", mean(d$Q30), as.numeric(Q30)),
           attitude_fac_car = ifelse(attitude_fac_car == "mean", mean(d$attitude_fac_car), as.numeric(attitude_fac_car)),
           attitude_fac_bike = ifelse(attitude_fac_bike == "mean", mean(d$attitude_fac_bike), as.numeric(attitude_fac_bike)),
           attitude_fac_escooter = ifelse(attitude_fac_escooter == "mean", mean(d$attitude_fac_escooter), as.numeric(attitude_fac_escooter)),
           attitude_fac_MAX = ifelse(attitude_fac_MAX == "mean", mean(d$attitude_fac_MAX), as.numeric(attitude_fac_MAX)),
           Q8_FAC1 = ifelse(Q8_FAC1 == "mean", mean(d$Q8_FAC1), as.numeric(Q8_FAC1)),
           Q8_FAC2 = ifelse(Q8_FAC2 == "mean", mean(d$Q8_FAC2), as.numeric(Q8_FAC2))
    )
  
  #Expand the test for all of the test points
    test_points_test <- tibble(car_drive_time = (test_points_final_sf$car_traffic_TT_sec / 60),
                               bike_ride_time = (test_points_final_sf$bike_TT_sec / 60),
                               em_scoot_time = (test_points_final_sf$escooter_TT_sec / 60),
                               em_scoot_cost = (test$em_scoot_cost_flat + em_scoot_time * test$em_scoot_cost_rate),
                               em_ride_time = (test_points_final_sf$MAX_TT_sec / 60)) %>%  #Calculate the total scoot cost at each test point
      bind_cols(test)

#Predict the probabilities of selecting each mode based on the generated model parameters
mode_probs <- data.frame(predict(build6,
                                newdata = test_points_test,
                                type = "probs")
                         )
#Using the mode with the highest probability, predict the mode that is most likely to be chosen
mode_choice <- data.frame(mode = predict(build6,
                                         newdata = test_points_test,
                                          type = "class")
                          )

#join this with the test_points_final_sf
test_points_final_sf <- test_points_final_sf %>% 
  bind_cols(mode_probs) %>% 
  bind_cols(mode_choice)

#=====================#
#Plot the shapes for a quick look####
#=====================#

#ggplot() +
  #geom_sf(data = water_sf_cropped) +
  #geom_sf(data = test_points_final_sf, aes(color = mode), size = 3) +
  #geom_sf(data = streets_sf_cropped) +
  #geom_sf(data = MAX_lines_sf_cropped, color = "orangered") +
  #geom_sf(data = MAX_stops_sf_cropped, shape = 21, color = "orangered", fill = "white")

#=====================#
#Transform the things to be plotted to the correct projection####
#=====================#

#Transform the sf objects to the right projection
test_points_final_4326_sf <- st_transform(test_points_final_sf, 4326) %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%  #add lon/lat columns for density functions
  rowwise() %>% 
  mutate(max_prob = max(Bike, Personal.Car, E.Scooter...MAX))

bike_network_sf_cropped_4326 <- st_transform(bike_network_sf_cropped, 4326)

MAX_lines_sf_cropped_4326 <- st_transform(MAX_lines_sf_cropped, 4326)

MAX_stops_sf_cropped_4326 <- st_transform(MAX_stops_sf_cropped, 4326)

#=====================#
#Perform kernel density interpolation for regularly spaced cells####
#=====================#

#Determine the number of cells for the height
#num_cells <- 5 #Number of cells in y direction, doubles for x direction below
num_cells <- 80 #Number of cells in y direction, doubles for x direction below

#Interpolate the z value for each cell for the car mode
interp_car <- interp2xyz(interp(x = test_points_final_4326_sf$lon,
                                y = test_points_final_4326_sf$lat,
                                z = test_points_final_4326_sf$Personal.Car,
                                duplicate = "mean",
                                nx = 2 * num_cells,
                                ny = num_cells),
                         data.frame = T)
#Interpolate the z value for each cell for the bike mode
interp_bike <- interp2xyz(interp(x = test_points_final_4326_sf$lon,
                                 y = test_points_final_4326_sf$lat,
                                 z = test_points_final_4326_sf$Bike,
                                 duplicate = "mean",
                                 nx = 2 * num_cells,
                                 ny = num_cells),
                          data.frame = T)
#Interpolate the z value for each cell for the em mode
interp_em <- interp2xyz(interp(x = test_points_final_4326_sf$lon,
                               y = test_points_final_4326_sf$lat,
                               z = test_points_final_4326_sf$E.Scooter...MAX,
                               duplicate = "mean",
                               nx = 2 * num_cells,
                               ny = num_cells),
                        data.frame = T)

#Create a table that selects the prob value of the "winner", that is the mode with the highest probabilty. Print the mode.
#This is necessary so that the winning mode can have its own color and the extent (probability) can determine the alpha (transparency)
interp_comb <- (interp_car %>% mutate(mode = "car")) %>% 
  bind_rows(interp_bike %>% mutate(mode = "bike")) %>% 
  bind_rows(interp_em %>% mutate(mode = "em")) %>% 
  drop_na() %>% 
  group_by(x, y) %>% 
  filter(z == max(z))

#=====================#
#Prep the basemap for the plot####
#=====================#

#Fix the names of the bbox for the map
renamed_bbox <- bbox
names(renamed_bbox) <- c("left", "bottom", "right", "top")
xmin <- bbox[[1]]
xmax <- bbox[[3]]
ymin <- bbox[[2]]
ymax <- bbox[[4]]

if(anchor_box_position == "upperright") {
  info_box_anchor_x <- xmax - .15 * (xmax - xmin)
  info_box_anchor_y <- ymax - .25 * (ymax - ymin)
} else if(anchor_box_position == "lowerleft") {
  info_box_anchor_x <- xmin + .15 * (xmax - xmin)
  info_box_anchor_y <- ymin + .25 * (ymax - ymin)
}

#Get the basemap
t <- get_stamenmap(renamed_bbox,
                   zoom = zoom,
                   maptype = "toner-background")

#=====================#
#Plot####
#=====================#

#Define the info box text
label = paste0("Walk time from car: ", test$car_walk_time, " mins\n",
              "Parking cost: $", test$car_parking_cost, "\n",
              "MAX ticket cost $", test$em_ticket_cost, "\n",
              "Walk time to e-scooter: ", test$em_walk_time, " mins \n",
              "E-Scooter Ride Cost: \n$", test$em_scoot_cost_flat, " + $", test$em_scoot_cost_rate, "/min")

#Define the color palette
#color palate
mode_scale_fill <- scale_fill_manual(name = "Mode",
                                    values = c(bike = viridis(3)[2],
                                                car = viridis(3)[1],
                                                em = viridis(3)[3]),
                                     labels = c(bike = "Bike",
                                                car = "Car",
                                                em = "E-Scooter + MAX"))

#make the plot
final_map <- ggmap(t) +
                geom_tile(data=interp_comb, #Select the dataset with only "winning" modes
                          aes(x = x, #lon
                              y = y, #lat
                              fill = mode, #the mode gets its own color
                              alpha = z), #determine the transparency based on the probability of the winning mode
                          inherit.aes = F) +
                scale_alpha(range = c(0, .75), labels = label_percent(accuracy = 1)) + #This constrains the alpha such that it never overpowers the map underneath
                mode_scale_fill + #Specify the colors, same color for mode each time even if mode not present in plot
                #geom_sf(data = bike_network_sf_cropped_4326, #add the bike network
                        #color = "green3",
                        #size = 1,
                        #inherit.aes = F) +
                geom_sf(data = MAX_lines_sf_cropped_4326, #add MAX lines
                        aes(geometry = geometry),
                        color = 'orangered',
                        inherit.aes = F,
                        lwd = 2) +
                geom_sf(data = MAX_stops_sf_cropped_4326, #add MAX stops
                        aes(geometry = geometry),
                        fill = 'white',
                        color = 'orangered',
                        size = 3,
                        shape = 21,
                        inherit.aes = F) +
                scalebar(location = "bottomright", #add scalebar
                         transform = T,
                         dist = map_scale_length,
                         st.dist = .04,
                         anchor = c(x = xmax - .07 * (xmax - xmin),
                                    y = ymin + .08 * (ymax - ymin)),
                         x.min = xmin,
                         x.max = xmax,
                         y.min = ymin,
                         y.max = ymax,
                         dist_unit = "mi",
                         inherit.aes = F) +
                theme(axis.title.x = element_blank(), #get rid of axis labels
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank()) +
                labs(fill = "Highest Probability Mode",
                     title = "Probability of Selecting a Mode to get to PSU",
                     subtitle = paste("Test:", test$testname),
                     alpha = "Probability") +
                geom_label(aes(x = info_box_anchor_x, #Add in a box with all of the attributes
                               y = info_box_anchor_y,
                               label = label),
                           size = 3,
                           inherit.aes = F)
  
ggsave(final_map,
       filename = paste0("Exports/probabilities/", map_view, test$filename, ".png"),
       width = plot_width,
       height = 4,
       units = "in",
       dpi = 450)
}

#Define sets of static values
tests <- read.csv(paste0("Exports/probabilities/", map_view, "combosets.csv"), fileEncoding="UTF-8-BOM")

#Run the mapping function.
for (i in 1:nrow(tests)) {
  mode_selection_probability_map(
    tests[i,],
    d,
    test_points_final_sf,
    bbox = bbox,
    map_view = map_view,
    zoom = zoom,
    anchor_box_position = anchor_box_position,
    map_scale_length = map_scale_length,
    plot_width = plot_width)
}
