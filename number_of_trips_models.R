#Number of Trips in Last 7 Days Models
#Mike McQueen

#=====================#
#Setup####
#=====================#

#Load packages
library(sjPlot)
library(dplyr)
library(tidyr)
library(MASS)

#Define recode key
level_key = c(`0` = "0 trips", `1` = "1-4 trips", `2` = "5-10 trips", `3` = "More than 10 trips")

#Set current data frame from last script
d <- step4_output %>% 
  mutate(Q13 = recode_factor(Q13, !!!level_key, .ordered = T), #!!! performs unquote splicing
         Q17 = recode_factor(Q17, !!!level_key, .ordered = T),
         Q23 = recode_factor(Q23, !!!level_key, .ordered = T),
         Q30 = recode_factor(Q30, !!!level_key, .ordered = T)) 

#Load DV label table
var_labels_clean <- read.csv("labels/var_labels_clean.csv", fileEncoding = "UTF-8-BOM")
var_labels_clean <- var_labels_clean %>% #turn the loaded table into a named character vector for sjplot
  pivot_wider(names_from = name, values_from = label)

#Helper function for model summary tables
summarize_logit_build_models <- function(..., mode, pred.labels) {
  tab_model(...,
            file = paste("Exports/Trips/", mode, "_build.html", sep = ""),
            transform = c("exp"),
            show.ci = F,
            show.se = T,
            show.p = T,
            p.style = "asterisk",
            pred.labels = pred.labels,
            dv.labels = c(paste0("1. Frequency of trips in last 7 days, mode: ", mode, ". Terms: Demographics"),
                          paste0("2. Frequency of trips in last 7 days, mode: ", mode, ". Terms: Demographics + Current travel behavior"),
                          paste0("3. Frequency of trips in last 7 days, mode: ", mode, ". Terms: Demographics + Current travel behavior + Mode Attitudes"),
                          paste0("4. Frequency of trips in last 7 days, mode: ", mode, ". Terms: Demographics + Current travel behavior + Mode Attitudes + Decision factors")
                          ),
            wrap.labels = 1000)
}


#Helper function for model fit tables
summarize_logit_model_fit <- function(..., mode) {
  
  #Pre allocate the table
  model_list <- list(...)
  N = length(model_list)
  model_fit <- data.frame(Model = rep("", N), "Deviance (-2LL)" = rep(NA, N), ChiSq = rep(NA, N), df = rep(NA, N), p = rep(NA, N))
  
  #Build the table
  model_fit[1,] <- list("Intercept Only", model_list[[1]]$deviance, NA, NA, NA)
  
  for (i in 2:length(model_list)) {
  model_fit[i,] <- list(toString(i - 1), #model number
                        model_list[[i]]$deviance, #model deviance (-2LL)
                        model_list[[1]]$deviance - model_list[[i]]$deviance, #model chi square (delta deviance)
                        model_list[[i]]$edf, #df
                        pchisq((model_list[[1]]$deviance - model_list[[i]]$deviance), df = model_list[[i]]$edf, lower.tail = F)) #p
  }
  
  #Export the table
  write.csv(model_fit, file = paste0("Exports/Trips/", mode, "_build_model_fit.csv"))
}
  #This one is just for the escooter models, since it's a binomial logit using glm
  summarize_binomial_logit_model_fit <- function(..., mode) {
    
    #Pre allocate the table
    model_list <- list(...)
    N = length(model_list)
    model_fit <- data.frame(Model = rep("", N), "Deviance (-2LL)" = rep(NA, N), ChiSq = rep(NA, N), df = rep(NA, N), p = rep(NA, N))
    
    #Build the table
    model_fit[1,] <- list("Intercept Only", model_list[[1]]$deviance, NA, NA, NA)
    
    for (i in 2:length(model_list)) {
      model_fit[i,] <- list(toString(i - 1), #model number
                            model_list[[i]]$deviance, #model deviance (-2LL)
                            model_list[[1]]$deviance - model_list[[i]]$deviance, #model chi square (delta deviance)
                            (length(coef(model_list[[i]])) - 1), #df, this is simply the number of predictors
                            pchisq((model_list[[1]]$deviance - model_list[[i]]$deviance), df = (length(coef(model_list[[i]])) - 1), lower.tail = F)) #p
    }
  
  #Export the table
  write.csv(model_fit, file = paste0("Exports/Trips/", mode, "_build_model_fit.csv"))
}

#=====================#
#Car Trips####
#=====================#

#First, try building up the model using calculated decision factors
#null (int only)
car_build0 <- polr(Q13 ~ 1,
                   d,
                   method = "logistic",
                   Hess = T)

#Dem
car_build1 <- polr(Q13 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                     RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                     RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                     Q65_female + Q65_nonbinary + Q65_other,
                   d,
                   method = "logistic",
                   Hess = T)
  
#Dem + current travel behavior
car_build2 <- polr(Q13 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                     RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                     RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                     Q65_female + Q65_nonbinary + Q65_other +
                     Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6,
                   d,
                   method = "logistic",
                   Hess = T)
  
#Dem + current travel behavior + attitudes (fac)
car_build3 <- polr(Q13 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                     RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                     RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                     Q65_female + Q65_nonbinary + Q65_other +
                     Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                     attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX,
                   d,
                   method = "logistic",
                   Hess = T)

#Dem + current travel behavior + attitudes (fac) + decision factors
car_build4 <- polr(Q13 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                     RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                     RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                     Q65_female + Q65_nonbinary + Q65_other +
                     Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                     attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                     Q8_FAC1 + Q8_FAC2,
                   d,
                   method = "logistic",
                   Hess = T)

#Summarize results in a table
summarize_logit_build_models(car_build1, car_build2, car_build3, car_build4, mode = "Car", pred.labels = var_labels_clean)
summarize_logit_model_fit(car_build0, car_build1, car_build2, car_build3, car_build4, mode = "Car")


#=====================#
#Bike Trips####
#=====================#

#Note: there were not enough cases to estimate parameters for RECODED_Q63_nat_haw_pac, RECODED_Q63_other, and Q65_other.
#Cases where these variables are true (equal 1) have been removed, and no parameter is estimated for these variables.

#First, try building up the model using calculated decision factors
#null (int only)
bike_build0 <- polr(Q17 ~ 1,
                    d %>% filter(RECODED_Q63_nat_haw_pac != 1,
                                 RECODED_Q63_other != 1,
                                 Q65_other != 1),
                   method = "logistic",
                   Hess = T)

#Dem
bike_build1 <- polr(Q17 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                      RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                      Q65_female + Q65_nonbinary,
                    d %>% filter(RECODED_Q63_nat_haw_pac != 1,
                                 RECODED_Q63_other != 1,
                                 Q65_other != 1),
                    method = "logistic",
                    Hess = T)
  
#Dem + current travel behavior
bike_build2 <- polr(Q17 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                      RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                      Q65_female + Q65_nonbinary +
                      Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6,
                    d %>% filter(RECODED_Q63_nat_haw_pac != 1,
                                 RECODED_Q63_other != 1,
                                 Q65_other != 1),
                    method = "logistic",
                    Hess = T)
  
#Dem + current travel behavior + attitudes (fac)
bike_build3 <- polr(Q17 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                      RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                      Q65_female + Q65_nonbinary +
                      Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                      attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX,
                    d %>% filter(RECODED_Q63_nat_haw_pac != 1,
                                 RECODED_Q63_other != 1,
                                 Q65_other != 1),
                    method = "logistic",
                    Hess = T)
  
#Dem + current travel behavior + attitudes (fac) + decision factors
bike_build4 <- polr(Q17 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                      RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                      Q65_female + Q65_nonbinary +
                      Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                      attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                      Q8_FAC1 + Q8_FAC2,
                    d %>% filter(RECODED_Q63_nat_haw_pac != 1,
                                 RECODED_Q63_other != 1,
                                 Q65_other != 1),
                    method = "logistic",
                    Hess = T)

#Summarize results in a table
summarize_logit_build_models(bike_build1, bike_build2, bike_build3, bike_build4, mode = "Bike", pred.labels = var_labels_clean)
summarize_logit_model_fit(bike_build0, bike_build1, bike_build2, bike_build3, bike_build4, mode = "Bike")

#=====================#
#E-Scooter Trips####
#=====================#

#There are only four cases where someone took 5-10 e-scooter trips, and there are only four cases where someone took more than 10 e-scooter trips
#combine these categories so it's just 0 vs. at least one

d_escooter <- d %>% 
  mutate(Q23 = recode_factor(Q23, "0 trips" = "0 trips",
                             "1-4 trips" = "At least one trip",
                             "5-10 trips" = "At least one trip",
                             "More than 10 trips" = "At least one trip",
                             .ordered = T))

#Note: there were not enough cases to estimate parameters for Q68_inc_5, Q68_inc_6, RECODED_Q63_am_ind_ala, RECODED_Q63_other,
#Q65_nonbinary, and Q65_other.
#Cases where these variables are true (equal 1) have been removed, and no parameter is estimated for these variables.


#First, try building up the model using calculated decision factors
#null (int only)
escooter_build0 <- glm(Q23 ~ 1,
                       d_escooter %>% filter(Q68_inc_5 != 1,
                                             Q68_inc_6 != 1,
                                             RECODED_Q63_am_ind_ala != 1,
                                             RECODED_Q63_other != 1,
                                             Q65_nonbinary != 1,
                                             Q65_other != 1),
                    family = binomial)

#Dem
escooter_build1 <- glm(Q23 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 +
                          RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                          RECODED_Q63_nat_haw_pac +
                          Q65_female,
                        data = d_escooter %>% filter(Q68_inc_5 != 1,
                                                     Q68_inc_6 != 1,
                                                     RECODED_Q63_am_ind_ala != 1,
                                                     RECODED_Q63_other != 1,
                                                     Q65_nonbinary != 1,
                                                     Q65_other != 1),
                        family = binomial)
  
#Dem + current travel behavior
escooter_build2 <- glm(Q23 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + 
                          RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                          RECODED_Q63_nat_haw_pac +
                          Q65_female +
                          Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6,
                        data = d_escooter %>% filter(Q68_inc_5 != 1,
                                                     Q68_inc_6 != 1,
                                                     RECODED_Q63_am_ind_ala != 1,
                                                     RECODED_Q63_other != 1,
                                                     Q65_nonbinary != 1,
                                                     Q65_other != 1),
                        family = binomial)
  
#Dem + current travel behavior + attitudes (fac)
escooter_build3 <- glm(Q23 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + 
                          RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                          RECODED_Q63_nat_haw_pac +
                          Q65_female +
                          Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                          attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX,
                        data = d_escooter %>% filter(Q68_inc_5 != 1,
                                                     Q68_inc_6 != 1,
                                                     RECODED_Q63_am_ind_ala != 1,
                                                     RECODED_Q63_other != 1,
                                                     Q65_nonbinary != 1,
                                                     Q65_other != 1),
                        family = binomial)
  
#Dem + current travel behavior + attitudes (fac) + decision factors
escooter_build4 <- glm(Q23 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 +
                          RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                          RECODED_Q63_nat_haw_pac + 
                          Q65_female +
                          Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                          attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                          Q8_FAC1 + Q8_FAC2,
                        data = d_escooter %>% filter(Q68_inc_5 != 1,
                                                     Q68_inc_6 != 1,
                                                     RECODED_Q63_am_ind_ala != 1,
                                                     RECODED_Q63_other != 1,
                                                     Q65_nonbinary != 1,
                                                     Q65_other != 1),
                        family = binomial)

#Summarize results in a table
summarize_logit_build_models(escooter_build1, escooter_build2, escooter_build3, escooter_build4, mode = "E-Scooter", pred.labels = var_labels_clean)
summarize_binomial_logit_model_fit(escooter_build0, escooter_build1, escooter_build2, escooter_build3, escooter_build4, mode = "E-Scooter")

#=====================#
#MAX Trips####
#=====================#

#First, try building up the model using calculated decision factors
#null (int only)
MAX_build0 <- polr(Q30 ~ 1,
                        d,
                        method = "logistic",
                        Hess = T)

#Dem
MAX_build1 <- polr(Q30 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                     RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                     RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                     Q65_female + Q65_nonbinary + Q65_other,
                   d,
                   method = "logistic",
                   Hess = T)
  
#Dem + current travel behavior
MAX_build2 <- polr(Q30 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                     RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                     RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                     Q65_female + Q65_nonbinary + Q65_other +
                     Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6,
                   d,
                   method = "logistic",
                   Hess = T)
  
#Dem + current travel behavior + attitudes (fac)
MAX_build3 <- polr(Q30 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                     RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                     RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                     Q65_female + Q65_nonbinary + Q65_other +
                     Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                     attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX,
                   d,
                   method = "logistic",
                   Hess = T)

  #Dem + current travel behavior + attitudes (fac) + decision factors
MAX_build4 <- polr(Q30 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                     RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                     RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                     Q65_female + Q65_nonbinary + Q65_other +
                     Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                     attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                     Q8_FAC1 + Q8_FAC2,
                   d,
                   method = "logistic",
                   Hess = T)

#Summarize results in a table
summarize_logit_build_models(MAX_build1, MAX_build2, MAX_build3, MAX_build4, mode = "MAX", pred.labels = var_labels_clean)
summarize_logit_model_fit(MAX_build0, MAX_build1, MAX_build2, MAX_build3, MAX_build4, mode = "MAX")
