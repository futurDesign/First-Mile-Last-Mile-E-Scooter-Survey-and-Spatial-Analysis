#Travel Time Google API Queries
#Mike McQueen

#=====================#
#Load packages####
#=====================#

library(dplyr)
library(ggplot2)
library(ggmap)
library(lubridate)

#=====================#
#Activate API key####
#=====================#

#read google key
#key <- pull(read.delim("../../GeoCoding/Google Maps/API Key.txt", header = FALSE))

#Register the API key
register_google(key)

#=====================#
#Load the MAX stops (will just run them once)####
#=====================#


#=====================#
#Load the OD matrix####
#=====================#
load("Exports/Prepped data for Google/PDXtest_points_list_for_API.RData")
d <- test_points_list_for_API

#=====================#
#Pick the departure time of interest####
#Needs to be time since midnight, January 1, 1970 UTC.
#Use lubridate since it also defines time in the same way
#=====================#

departure_time <- as.numeric(mdy_hms("7/23/20 8:00:00", tz = "US/Pacific"))

#=====================#
#Make queries####
#This is commented out because did it once for all MAX stops
#=====================#

#create od matrix that's just MAX travel time from each station to save on queries
#d_max <- MAX_stops_list_for_API %>% 
  #mutate(distance_m = NA,
         #MAX_TT_sec = NA)

#travel time via MAX from each MAX station to PSU rec center
#MAX <- list()
#for (i in 1:nrow(d_max)) {
  #trip_info <- mapdist(from = paste(d_max$station_lat[i], d_max$station_lon[i]),
                           #to = paste(d_max$PSU_rec_center_lat[i], d_max$PSU_rec_center_lon[i]),
                           #mode = "transit",
                           #structure = "segment",
                           #output = "all",
                           #inject = paste0("transit_mode=tram&departure_time=", departure_time))
  #MAX = append(MAX, trip_info)
#}

#for (i in 1:nrow(d_max)) {
  #extract the distance in meters and add it to the df
  #d_max$distance_m[i] <- MAX[[i]][[1]]$distance[[2]]
  #extract the duration in seconds and add it to the df
  #d_max$MAX_TT_sec[i] <- MAX[[i]][[1]]$duration[[2]]
#}

#save both the raw output and the df with the info attached to a file
#save(MAX, d_max, file = "Exports/OD Data from Google/MAX.RData")

#====#

#travel time via car from each test point to PSU rec center, morning rush period
#create od matrix that's car travel time from each test point to PSU Rec Center
d_car <- test_points_list_for_API %>% 
  mutate(distance_m = NA,
         car_TT_sec = NA,
         car_traffic_TT_sec = NA)

#travel time via car from each test_point to PSU rec center
car <- list()
for (i in 1:nrow(d_car)) {
  trip_info <- mapdist(from = paste(d_car$test_point_lat[i], d_car$test_point_lon[i]),
                       to = paste(d_car$PSU_rec_center_lat[i], d_car$PSU_rec_center_lon[i]),
                       mode = "driving",
                       structure = "segment",
                       output = "all",
                       inject = paste0("&departure_time=", departure_time))
  car = append(car, trip_info)
}

for (i in 1:nrow(d_car)) {
  #extract the distance in meters and add it to the df
  d_car$distance_m[i] <- car[[i]][[1]]$distance[[2]]
  #extract the duration in seconds and add it to the df
  d_car$car_TT_sec[i] <- car[[i]][[1]]$duration[[2]]
  #extract the duration in seconds in traffic and add it to the df
  d_car$car_traffic_TT_sec[i] <- car[[i]][[1]]$duration_in_traffic[[2]]
}

#save both the raw output and the df with the info attached to a file
save(car, d_car, file = paste0("Exports/OD Data from Google/", map_view, "_car.RData"))

#====#

#travel time via bike from each test point to PSU rec center
#create od matrix that's bike travel time from each test point to PSU Rec Center
d_bike <- test_points_list_for_API %>% 
  mutate(bike_distance_m = NA,
         bike_TT_sec = NA)

#travel time via bike from each test_point to PSU rec center
bike <- list()
for (i in 1:nrow(d_bike)) {
  trip_info <- mapdist(from = paste(d_bike$test_point_lat[i], d_bike$test_point_lon[i]),
                       to = paste(d_bike$PSU_rec_center_lat[i], d_bike$PSU_rec_center_lon[i]),
                       mode = "bicycling",
                       structure = "segment",
                       output = "all",
                       inject = paste0("&departure_time=", departure_time))
  bike = append(bike, trip_info)
}

for (i in 1:nrow(d_bike)) {
  #extract the distance in meters and add it to the df
  d_bike$bike_distance_m[i] <- bike[[i]][[1]]$distance[[2]]
  #extract the duration in seconds and add it to the df
  d_bike$bike_TT_sec[i] <- bike[[i]][[1]]$duration[[2]]
}

#save both the raw output and the df with the info attached to a file
save(bike, d_bike, file = paste0("Exports/OD Data from Google/", map_view, "bike.RData"))

#====#

#travel time via "escooter" (actually bike) to the closest MAX station
#create od matrix that's "escooter" (bike) travel time from each test point to closest MAX station
d_escooter <- test_points_list_for_API %>% 
  mutate(escooter_distance_m = NA,
         escooter_TT_sec = NA)

#travel time via escooter from each test_point to the closest MAX station
escooter <- list()
for (i in 1:nrow(d_escooter)) {
  trip_info <- mapdist(from = paste(d_escooter$test_point_lat[i], d_escooter$test_point_lon[i]),
                       to = paste(d_escooter$nearest_station_lat[i], d_escooter$nearest_station_lon[i]),
                       mode = "bicycling",
                       structure = "segment",
                       output = "all",
                       inject = paste0("&departure_time=", departure_time))
  escooter = append(escooter, trip_info)
}

for (i in 1:nrow(d_escooter)) {
  #extract the distance in meters and add it to the df
  d_escooter$escooter_distance_m[i] <- escooter[[i]][[1]]$distance[[2]]
  #extract the duration in seconds and add it to the df
  d_escooter$escooter_TT_sec[i] <- escooter[[i]][[1]]$duration[[2]]
}

#save both the raw output and the df with the info attached to a file
save(escooter, d_escooter, file = paste0("Exports/OD Data from Google/", map_view, "escooter.RData"))
