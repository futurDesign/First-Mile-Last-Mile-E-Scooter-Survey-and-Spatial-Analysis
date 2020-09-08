#Elasticity

#Packages/Files
library(dplyr)
library(tidyr)
source("mnlogit_predict.r")

#Load models
load("Exports/MNL/mnlogit/models_mnlogit.RData")

#Load cleaned data
load("Exports/cleaned_data/longform.RData")
load("Exports/cleaned_data/TT_cleaned.RData")

#Load elasticity test file
elasticity_test_raw <- read.csv("Exports/Elasticities/elasticities_test.csv", fileEncoding="UTF-8-BOM")

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


#Load elasticity conditions set


#Load in the test case for elasticity computation
#Clean the input test as required
elasticity_test <-  elasticity_test_raw %>% 
  #Replace "mean" cells with the actual mean from the data:
  mutate(car_walk_time = ifelse(car_walk_time == "mean", mean(d %>% filter(mode == "1.car") %>% pull(car_walk_time)), as.numeric(car_walk_time)),
         car_drive_time = ifelse(car_drive_time == "mean", mean(d %>% filter(mode == "1.car") %>% pull(car_drive_time)), as.numeric(car_drive_time)),
         car_parking_cost = ifelse(car_parking_cost == "mean", mean(d %>% filter(mode == "1.car") %>% pull(car_parking_cost )), as.numeric(car_parking_cost)),
         bike_ride_time = ifelse(bike_ride_time == "mean", mean(d %>% filter(mode == "2.bike") %>% pull(bike_ride_time)), as.numeric(bike_ride_time)),
         em_walk_time = ifelse(em_walk_time == "mean", mean(d %>% filter(mode == "3.em") %>% pull(em_walk_time)), as.numeric(em_walk_time)),
         em_scoot_time = ifelse(em_scoot_time == "mean", mean(d %>% filter(mode == "3.em") %>% pull(em_scoot_time)), as.numeric(em_scoot_time)),
         em_scoot_cost = ifelse(em_scoot_cost == "mean", mean(d %>% filter(mode == "3.em") %>% pull(em_scoot_cost)), as.numeric(em_scoot_cost)),
         em_ride_time = ifelse(em_ride_time == "mean", mean(d %>% filter(mode == "3.em") %>% pull(em_ride_time)), as.numeric(em_ride_time)),
         em_ticket_cost = ifelse(em_ticket_cost == "mean", mean(d %>% filter(mode == "3.em") %>% pull(em_ticket_cost)), as.numeric(em_ticket_cost)),
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

#ONLY USE THIS IF YOU WANT TO USE AVERAGE VALUES FROM TRAVEL TIMES FROM API, NOT EXPERIMENT AVERAGES
#Add average TT and cost data to elasticities test table
#test_points_test <- tibble(car_drive_time = (test_points_info_final$car_traffic_TT_sec / 60),
                           #bike_ride_time = (test_points_info_final$bike_TT_sec / 60),
                           #em_scoot_time = (test_points_info_final$escooter_TT_sec / 60),
                           #em_scoot_cost = (elasticity_test$em_scoot_cost_flat + em_scoot_time * elasticity_test$em_scoot_cost_rate),
                           #em_ride_time = (test_points_info_final$MAX_TT_sec / 60)) %>%  #Calculate the total scoot cost at each test point
  #Get the mean values from all the test points
  #summarize_all(mean) %>% 
  #bind_cols(elasticity_test)

#Predict the probabilities and choice of selecting each mode based on the estimated model parameters
mode_probs <- data.frame(mnlogit_predict(newdata = elasticity_test, build6)
)

#Join the probs to the test_points for elasticity purposes
elasticity_info <- test_points_test %>% bind_cols(mode_probs)

#Now, get elasticities (B * z * (1 - P)) and cross elasticities (-B * z * P)
#Car
E_car_car_drive_time <- build6$coefficients["car_drive_time"] * elasticity_info$car_drive_time * (1 - elasticity_info$p_car)

