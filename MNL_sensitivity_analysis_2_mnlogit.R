#MNL Sensitivity Analysis
#Mike McQueen
#Version 2 - uses the model created by mnlogit

#=====================#
#Load packages####
#=====================#

library(ggplot2)
library(nnet)
library(dplyr)
library(tidyr)
library(stats)
library(scales)
library(viridis)
source("mnlogit_predict.R")

#=====================#
#Load the MNL models####
#=====================#

load("Exports/MNL/mnlogit/models.RData")

#=====================#
#Load clean var names####
#=====================#

var_labels_clean <- read.csv("labels/var_labels_clean.csv", fileEncoding = "UTF-8-BOM")

#=====================#
#Perform Row-Wise elimination (as was performed when the model was estimated)####
#=====================#

d <- d_complete_wide %>% 
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
#Define average values####
#=====================#

car_walk_time <- mean(d$car_walk_time)
car_drive_time <- mean(d$car_drive_time)
car_parking_cost <- 7
bike_ride_time <- mean(d$bike_ride_time)
em_walk_time <- mean(d$em_walk_time)
em_scoot_time <- mean(d$em_scoot_time)
em_scoot_cost <- mean(d$em_scoot_cost)
em_ticket_cost <- 2.50
em_ride_time <- mean(d$em_ride_time)
Q68_inc_2 <- mean(d$Q68_inc_2)
Q68_inc_3 <- mean(d$Q68_inc_3)
Q68_inc_4 <- mean(d$Q68_inc_4)
Q68_inc_5 <- mean(d$Q68_inc_5)
Q68_inc_6 <- mean(d$Q68_inc_6)
RECODED_Q63_am_ind_ala <- mean(d$RECODED_Q63_am_ind_ala)
RECODED_Q63_asian <- mean(d$RECODED_Q63_asian)
RECODED_Q63_black <- mean(d$RECODED_Q63_black)
RECODED_Q63_hisp_lat <- mean(d$RECODED_Q63_hisp_lat)
RECODED_Q63_more_than_one_race <- mean(d$RECODED_Q63_more_than_one_race)
RECODED_Q63_nat_haw_pac <- mean(d$RECODED_Q63_nat_haw_pac)
RECODED_Q63_other <- mean(d$RECODED_Q63_other)
Q65_female <- mean(d$Q65_female)
Q65_nonbinary <- mean(d$Q65_nonbinary)
Q65_other <- mean(d$Q65_other)
Q11 <- mean(d$Q11)
Q64 <- mean(d$Q64)
Q66 <- mean(d$Q66)
Q67 <- mean(d$Q67)
Q69 <- mean(d$Q69)
Q70 <- mean(d$Q70)
Q5 <- mean(d$Q5)
Q7_4 <- mean(d$Q7_4)
RECODED_Q6_activetransport <- mean(d$RECODED_Q6_activetransport)
RECODED_Q6_publictransit <- mean(d$RECODED_Q6_publictransit)
RECODED_Q6_other <- mean(d$RECODED_Q6_other)
Q13 <- mean(d$Q13)
Q17 <- mean(d$Q17)
Q23 <- mean(d$Q23)
Q30 <- mean(d$Q30)
attitude_fac_car <- mean(d$attitude_fac_car)
attitude_fac_bike <- mean(d$attitude_fac_bike)
attitude_fac_escooter <- mean(d$attitude_fac_escooter)
attitude_fac_MAX <- mean(d$attitude_fac_MAX)
Q8_FAC1 <- mean(d$Q8_FAC1)
Q8_FAC2 <- mean(d$Q8_FAC2)

#=====================#
#create ranges for testing elasticities####
#=====================#

get_seq <- function(col, num_test_points) {
  return(seq(min(col), max(col), length.out = num_test_points))
}

num_test_points <- 100

rangelist <- list(car_walk_time = get_seq(d$car_walk_time, num_test_points),
     car_drive_time = get_seq(d$car_drive_time, num_test_points),
     car_parking_cost = get_seq(d$car_parking_cost, num_test_points),
     bike_ride_time = get_seq(d$bike_ride_time, num_test_points),
     em_walk_time  = get_seq(d$em_walk_time, num_test_points),
     em_scoot_time  = get_seq(d$em_scoot_time, num_test_points),
     em_scoot_cost  = get_seq(d$em_scoot_cost, num_test_points),
     em_ticket_cost  = get_seq(d$em_ticket_cost, num_test_points),
     em_ride_time = get_seq(d$em_ride_time, num_test_points),
     Q68_inc_2  = get_seq(d$Q68_inc_2, num_test_points),
     Q68_inc_3  = get_seq(d$Q68_inc_3, num_test_points),
     Q68_inc_4  = get_seq(d$Q68_inc_4, num_test_points),
     Q68_inc_5  =  get_seq(d$Q68_inc_5, num_test_points),
     Q68_inc_6  = get_seq(d$Q68_inc_6, num_test_points),
     RECODED_Q63_am_ind_ala  = get_seq(d$RECODED_Q63_am_ind_ala, num_test_points),
     RECODED_Q63_asian  = get_seq(d$RECODED_Q63_asian, num_test_points),
     RECODED_Q63_black  = get_seq(d$RECODED_Q63_black, num_test_points),
     RECODED_Q63_hisp_lat  = get_seq(d$RECODED_Q63_hisp_lat, num_test_points),
     RECODED_Q63_more_than_one_race  = get_seq(d$RECODED_Q63_more_than_one_race, num_test_points),
     RECODED_Q63_nat_haw_pac  = get_seq(d$RECODED_Q63_nat_haw_pac, num_test_points),
     RECODED_Q63_other = get_seq(d$RECODED_Q63_other, num_test_points),
     Q65_female = get_seq(d$Q65_female, num_test_points),
     Q65_nonbinary = get_seq(d$Q65_nonbinary, num_test_points),
     Q65_other = get_seq(d$Q65_other, num_test_points),
     Q11 = get_seq(d$Q11, num_test_points),
     Q64 = get_seq(d$Q64, num_test_points),
     Q66 = get_seq(d$Q66, num_test_points),
     Q67 = get_seq(d$Q67, num_test_points),
     Q69 = get_seq(d$Q69, num_test_points),
     Q70 = get_seq(d$Q70, num_test_points),
     Q5 = get_seq(d$Q5, num_test_points),
     Q7_4 = get_seq(d$Q7_4, num_test_points),
     RECODED_Q6_activetransport = get_seq(d$RECODED_Q6_activetransport, num_test_points),
     RECODED_Q6_publictransit = get_seq(d$RECODED_Q6_publictransit, num_test_points),
     RECODED_Q6_other= get_seq(d$RECODED_Q6_other, num_test_points),
     Q13 = get_seq(d$Q13, num_test_points),
     Q17 = get_seq(d$Q17, num_test_points),
     Q23 = get_seq(d$Q23, num_test_points),
     Q30 = get_seq(d$Q30, num_test_points),
     attitude_fac_car = get_seq(d$attitude_fac_car, num_test_points),
     attitude_fac_bike  = get_seq(d$attitude_fac_bike, num_test_points),
     attitude_fac_escooter = get_seq(d$attitude_fac_escooter, num_test_points),
     attitude_fac_MAX = get_seq(d$attitude_fac_MAX, num_test_points),
     Q8_FAC1 = get_seq(d$Q8_FAC1, num_test_points),
     Q8_FAC2 = get_seq(d$Q8_FAC2, num_test_points))

#=====================#
#create the dataframe that contains only average values####
#=====================#

avg_data = tibble(car_walk_time = rep(car_walk_time, num_test_points), #This causes the tibble to be the length of the number of test points
                 car_drive_time = car_drive_time,
                 car_parking_cost = car_parking_cost,
                 bike_ride_time = bike_ride_time,
                 em_walk_time  = em_walk_time,
                 em_scoot_time  = em_scoot_time,
                 em_scoot_cost  = em_scoot_cost,
                 em_ticket_cost  = em_ticket_cost,
                 em_ride_time = em_ride_time,
                 Q68_inc_2  = Q68_inc_2,
                 Q68_inc_3  = Q68_inc_3,
                 Q68_inc_4  = Q68_inc_4,
                 Q68_inc_5  =  Q68_inc_5,
                 Q68_inc_6  = Q68_inc_6,
                 RECODED_Q63_am_ind_ala  = RECODED_Q63_am_ind_ala,
                 RECODED_Q63_asian  = RECODED_Q63_asian,
                 RECODED_Q63_black  = RECODED_Q63_black,
                 RECODED_Q63_hisp_lat  = RECODED_Q63_hisp_lat,
                 RECODED_Q63_more_than_one_race  = RECODED_Q63_more_than_one_race,
                 RECODED_Q63_nat_haw_pac  = RECODED_Q63_nat_haw_pac,
                 RECODED_Q63_other = RECODED_Q63_other,
                 Q65_female = Q65_female,
                 Q65_nonbinary = Q65_nonbinary,
                 Q65_other = Q65_other,
                 Q11 = Q11,
                 Q64 = Q64,
                 Q66 = Q66,
                 Q67 = Q67,
                 Q69 = Q69,
                 Q70 = Q70,
                 Q5 = Q5,
                 Q7_4 = Q7_4,
                 RECODED_Q6_activetransport = RECODED_Q6_activetransport,
                 RECODED_Q6_publictransit = RECODED_Q6_publictransit,
                 RECODED_Q6_other= RECODED_Q6_other,
                 Q13 = Q13,
                 Q17 = Q17,
                 Q23 = Q23,
                 Q30 = Q30,
                 attitude_fac_car = attitude_fac_car,
                 attitude_fac_bike  = attitude_fac_bike,
                 attitude_fac_escooter = attitude_fac_escooter,
                 attitude_fac_MAX = attitude_fac_MAX,
                 Q8_FAC1 = Q8_FAC1,
                 Q8_FAC2 = Q8_FAC2)

#=====================#
#Run sensitivity analysis####
#=====================#

#A helper function to calculate the sensitivity analysis values
sens_calc <- function(avg_data, rangelist) {
  #initialize the output list
  output_prob_list <- list()
  
  for(i in 1:length(rangelist)) {
    #Assign the average data (same data in each row) to the test_data object
    test_data <- avg_data
    
    #Replace one column at a time with the range data
    test_data[,i] <-  rangelist[[i]]
    
    #Predict the probabilities of the three modes at each test point (row)
    t <- mnlogit_predict(build6, newdata = test_data) %>% 
      select(p_car, p_bike, p_em)
    
    #Join the modified column to the test data
    t <- t %>% 
      bind_cols(test_data[,i]) %>% 
      pivot_longer(cols = p_car:p_em, names_to = "mode", values_to = "value")
    
    #Append the table to the output list
    output_prob_list <- append(output_prob_list, list(t))
  }
  
  return(output_prob_list)

}

#Call the helper function
results <- sens_calc(avg_data, rangelist)

#Define the color palette
#color palate
mode_scale_color <- scale_color_manual(name = "Mode",
                                     values = c(p_bike = viridis(5)[4],
                                                p_car = viridis(5)[1],
                                                p_em = viridis(5)[3]),
                                     labels = c(p_bike = "Bike",
                                                p_car = "Car",
                                                p_em = "E-Scooter + MAX"))

#Plot the results
for (i in 1:length(results)) {
g <- ggplot(results[[i]], aes(x = results[[i]][[1]], y = value, color = mode)) +
  geom_line(size = 1.5) +
  labs(title = paste0(var_labels_clean %>% filter(name == names(results[[i]][1])) %>% pull(label), " Sensitivity Analysis"),
       x = "Value",
       y = "Probability") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0,1)) +
  mode_scale_color

ggsave(filename = paste0("Exports/probabilities_sensitivity/mnlogit/", names(results[[i]][1]), ".png"),
       plot = g,
       height = 5,
       width = 9,
       units = c("in"),
       dpi = 500)
}
