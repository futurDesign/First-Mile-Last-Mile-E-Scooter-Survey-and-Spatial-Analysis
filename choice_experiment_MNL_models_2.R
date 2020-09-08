#Stated Choice Experiment MNL Models 2
#This version uses the mnlogit package instead of nnet
#This gets us a better model because each utility
#function will not include alternative-specific coefficients
#Mike McQueen

#=====================#
#Setup####
#=====================#

#Load packages
library(sjPlot)
library(dplyr)
library(tidyr)
library(haven)
library(sjlabelled)
library(mnlogit)
library(stargazer)
library(stats)

#Set current data frame from last script
load("Exports/cleaned_data/step4_output.RData")
d <- step4_output

#Load DV label table that plays with the mnlogit models
mnlogit_var_labels_clean <- read.csv("Exports/MNL/mnlogit/var_order_and_names.csv", fileEncoding = "UTF-8-BOM")

#Helper function to summarize MNL model fit
summarize_MNL_model_fit <- function(...) {
  
  #Pre allocate the table
  model_list <- list(...)
  N = length(model_list)
  model_fit <- data.frame(Model = rep("", N), "Deviance (-2LL)" = rep(NA, N), ChiSq = rep(NA, N), df = rep(NA, N), p = rep(NA, N))
  
  #Build the table
  model_fit[1,] <- list("Intercept Only", -2 * as.numeric(model_list[[1]]$logLik), NA, NA, NA) #-2LL for intercept only model
  
  for (i in 2:length(model_list)) {
    model_fit[i,] <- list(toString(i - 1), #model number
                          -2 * as.numeric(model_list[[i]]$logLik), #model deviance (-2LL)
                          -2 * as.numeric(model_list[[1]]$logLik) - 2 * as.numeric(model_list[[i]]$logLik), #model chi square (delta deviance)
                          as.numeric(model_list[[i]]$df), #df
                          stats::pchisq(-2 * as.numeric(model_list[[1]]$logLik) - 2 * as.numeric(model_list[[i]]$logLik), df = as.numeric(model_list[[i]]$df), lower.tail = F)) #p
  }
  
  #Export the table
  write.csv(model_fit, file = "Exports/MNL/mnlogit/build_model_fit.csv")
}

#=====================#
#Configure the Long Form Experiment Responses####
#=====================#

#load in
d_exp <- read_spss("../PSUModeChoiceSurveyDataCleaning/cleandata/statedprefexp_clean_032320.sav") %>% 
  as_factor(only_labelled = TRUE) %>%  #Factor appropriate vectors
  set_na(na = c(-7, -1, "Refused", "Appropriate Skip", "Refused or Prefer not to answer"), drop.levels = TRUE) %>% #Make missing values system missing and drop corresponding levels
  zap_label() %>% #Since we exported our variable labels, can delete them
  zap_labels() %>%  #Since we converted labelled columns to factor columns, can delete value labels
  zap_formats() %>% #deletes format attribute (for cleanliness only)
  zap_widths() #deletes display width attribute (for cleanliness only)

#join the other vars and save without lengthening
d_complete_wide <- d_exp %>% 
  left_join(d, by = "ResponseId")

#join the other vars
d_complete <- d_exp %>% 
  left_join(d, by = "ResponseId") %>% 
  mutate(mode1 = "1.car", mode2 = "2.bike", mode3 = "3.em") %>% #create this in preparation for lengthening
  pivot_longer(c(mode1, mode2, mode3), names_to = NULL, values_to = "mode") %>% #this lengthens the data, one new row for each mode
  mutate(car_walk_time = case_when(mode == "2.bike" ~ 0, #This gets rid of mode specific attributes that we don't want to go into each mode's utility functions
                                   mode == "3.em" ~ 0,
                                   T ~ car_walk_time),
         car_drive_time = case_when(mode == "2.bike" ~ 0,
                                    mode == "3.em" ~ 0,
                                    T ~ car_drive_time),
         car_parking_cost = case_when(mode == "2.bike" ~ 0,
                                      mode == "3.em" ~ 0,
                                      T ~ car_parking_cost),
         bike_ride_time = case_when(mode == "1.car" ~ 0,
                                    mode == "3.em" ~ 0,
                                    T ~ bike_ride_time),
         em_walk_time = case_when(mode == "2.bike" ~ 0,
                                  mode == "1.car" ~ 0,
                                  T ~ em_walk_time),
         em_scoot_time = case_when(mode == "2.bike" ~ 0,
                                   mode == "1.car" ~ 0,
                                   T ~ em_scoot_time),
         em_ride_time = case_when(mode == "2.bike" ~ 0,
                                  mode == "1.car" ~ 0,
                                  T ~ em_ride_time),
         em_scoot_cost = case_when(mode == "2.bike" ~ 0,
                                   mode == "1.car" ~ 0,
                                   T ~ em_scoot_cost),
         em_ticket_cost = case_when(mode == "2.bike" ~ 0,
                                    mode == "1.car" ~ 0,
                                    T ~ em_ticket_cost)
  ) %>% 
  mutate(selected = case_when(mode == "2.bike" & response == "Bike" ~ T, #This column tells the package that that choice was selected from the set
                              mode == "1.car" & response == "Personal Car" ~ T,
                              mode == "3.em" & response == "E-Scooter + MAX" ~ T,
                              T ~ F))

#Save the long-form data for later use
save(d_complete, file = "Exports/cleaned_data/longform.RData")

#=====================#
#Run MNL Models 2####
#=====================#

#Intercept only, for chi squared test
fm0 <- formula(selected ~ 1)
build0 <- mnlogit(fm0,
                  d_complete,
                  maxiter = 1000,
                  choiceVar = "mode")
print(build0, what = "modsize")

#First, try building up the model using calculated decision factors
#Attributes
fm1 <- formula(selected ~ car_walk_time + car_drive_time + car_parking_cost + bike_ride_time + em_walk_time + em_scoot_time + em_scoot_cost + em_ticket_cost + em_ride_time | 1 | 1)
build1 <- mnlogit(fm1,
                  d_complete,
                  maxiter = 1000,
                  choiceVar = "mode")
print(build1, what = "modsize")

#Attributes + Dem
fm2 <- formula(selected ~ car_walk_time + car_drive_time + car_parking_cost + bike_ride_time + em_walk_time + em_scoot_time + em_scoot_cost + em_ticket_cost + em_ride_time |
                 Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                 RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                 RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                 Q65_female + Q65_nonbinary + Q65_other +
                 Q11 + Q64 + Q66 + Q67 + Q69 + Q70)
build2 <- mnlogit(fm2,
                  d_complete,
                  maxiter = 1000,
                  choiceVar = "mode")
print(build2, what = "modsize")

#Attributes + Dem + current travel behavior
fm3 <- formula(selected ~ car_walk_time + car_drive_time + car_parking_cost + bike_ride_time + em_walk_time + em_scoot_time + em_scoot_cost + em_ticket_cost + em_ride_time |
                 Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                 RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                 RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                 Q65_female + Q65_nonbinary + Q65_other +
                 Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                 Q13 + Q17 + Q23 + Q30)
build3 <- mnlogit(fm3,
                  d_complete,
                  maxiter = 1000,
                  choiceVar = "mode")
print(build3, what = "modsize")

#Attributes + Dem + current travel behavior + attitudes (fac)
fm4 <- formula(selected ~ car_walk_time + car_drive_time + car_parking_cost + bike_ride_time + em_walk_time + em_scoot_time + em_scoot_cost + em_ticket_cost + em_ride_time |
                 Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                 RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                 RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                 Q65_female + Q65_nonbinary + Q65_other +
                 Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                 Q13 + Q17 + Q23 + Q30 +
                 attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX)
build4 <- mnlogit(fm4,
                  d_complete,
                  maxiter = 1000,
                  choiceVar = "mode")
print(build4, what = "modsize")

#Attributes + Dem + current travel behavior + attitudes (fac) + decision factors
fm5 <- formula(selected ~ car_walk_time + car_drive_time + car_parking_cost + bike_ride_time + em_walk_time + em_scoot_time + em_scoot_cost + em_ticket_cost + em_ride_time |
                 Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                 RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                 RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                 Q65_female + Q65_nonbinary + Q65_other +
                 Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                 Q13 + Q17 + Q23 + Q30 +
                 attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                 Q8_FAC1 + Q8_FAC2)
build5 <- mnlogit(fm5,
                  d_complete,
                  maxiter = 1000,
                  choiceVar = "mode")
print(build5, what = "modsize")

#Attributes + Dem + current travel behavior + attitudes (fac) + decision factors *fixes RECODED_Q6 so that the dummy columns are used
fm6 <- formula(selected ~ car_walk_time + car_drive_time + car_parking_cost + bike_ride_time + em_walk_time + em_scoot_time + em_scoot_cost + em_ticket_cost + em_ride_time |
                 Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                 RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                 RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                 Q65_female + Q65_nonbinary + Q65_other +
                 Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6_activetransport + RECODED_Q6_publictransit + RECODED_Q6_other +
                 Q13 + Q17 + Q23 + Q30 +
                 attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                 Q8_FAC1 + Q8_FAC2)
build6 <- mnlogit(fm6,
                  d_complete,
                  maxiter = 1000,
                  choiceVar = "mode")
print(build6, what = "modsize")

#Print the build summaries
dep.var.labels = c("1. Stated choice experiment, Attributes",
                   "2. Stated choice experiment, Attributes + Demographics",
                   "3. Stated choice experiment, Attributes + Demographics + Current travel behavior",
                   "4. Stated choice experiment, Attributes + Demographics + Current travel behavior + Mode Attitudes",
                   "5. Stated choice experiment, Attributes + Demographics + Current travel behavior + Mode Attitudes + Decision factors")

#Odds Ratio
stargazer(build1, build2, build3, build4, build5,
          type = "html",
          t.auto = F,
          apply.coef = exp,
          out = "Exports/MNL/mnlogit/MNL_build_OR.html",
          column.labels = dep.var.labels,
          order = mnlogit_var_labels_clean$name,
          covariate.labels = mnlogit_var_labels_clean$label,
          dep.var.caption = "Mode Choice",
          report = "vc*")

#logit
stargazer(build1, build2, build3, build4, build5,
          type = "html",
          t.auto = F,
          out = "Exports/MNL/mnlogit/MNL_build_logit.html",
          column.labels = dep.var.labels,
          order = mnlogit_var_labels_clean$name,
          covariate.labels = mnlogit_var_labels_clean$label,
          dep.var.caption = "Mode Choice")

#Print the model fits
summarize_MNL_model_fit(build0, build1, build2, build3, build4, build5)

#Save the model results for use with mapping
save(d_complete, d_complete_wide, build0, build1, build2, build3, build4, build5, build6, file = "Exports/MNL/mnlogit/models.RData")
