#Propensity to Switch Models
#Mike McQueen

#=====================#
#Setup####
#=====================#

#Load packages
library(sjPlot)
library(dplyr)
library(tidyr)

#Set current data frame from last script
d <- step4_output

#Load DV label table
var_labels_clean <- read.csv("labels/var_labels_clean.csv", fileEncoding = "UTF-8-BOM")
var_labels_clean <- var_labels_clean %>% #turn the loaded table into a named character vector for sjplot
  pivot_wider(names_from = name, values_from = label)

#Helper function for model summary tables
summarize_OLS_models <- function(..., mode, pred.labels) {
  tab_model(...,
            file = paste("Exports/Switch/", mode, ".html", sep = ""),
            show.ci = F,
            show.se = T,
            show.std = T,
            p.style = "asterisk",
            pred.labels = pred.labels,
            dv.labels = c(paste("1. Propensity to switch to", mode, "OLS with summed mode attitude items"),
                          paste("2. Propensity to switch to", mode, "OLS with averaged mode attitude items"),
                          paste("3. Propensity to switch to", mode, "OLS with EFA factor scores calculated using regression method")),
            wrap.labels = 1000)
}
summarize_OLS_build_models <- function(..., mode, pred.labels) {
  tab_model(...,
            file = paste("Exports/Switch/", mode, "_build.html", sep = ""),
            show.ci = F,
            show.se = T,
            show.std = T,
            p.style = "asterisk",
            pred.labels = pred.labels,
            dv.labels = c(paste("1. Propensity to switch to", mode, "Demographics"),
                          paste("2. Propensity to switch to", mode, "Demographics + Current travel behavior"),
                          paste("3. Propensity to switch to", mode, "Demographics + Current travel behavior + Mode Attitudes"),
                          paste("4. Propensity to switch to", mode, "Demographics + Current travel behavior + Mode Attitudes + Decision factors")),
            wrap.labels = 1000)
}

#=====================#
#Switch to Car####
#=====================#

#First, try building up the model using calculated decision factors
#Dem
car_build1 <- lm(Q35_1 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                   RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                   RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                   Q65_female + Q65_nonbinary + Q65_other, d)
#Dem + current travel behavior
car_build2 <- lm(Q35_1 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                   RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                   RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                   Q65_female + Q65_nonbinary + Q65_other +
                   Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                   Q13 + Q17 + Q23 + Q30, d)
#Dem + current travel behavior + attitudes (fac)
car_build3 <- lm(Q35_1 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                   RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                   RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                   Q65_female + Q65_nonbinary + Q65_other +
                   Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                   Q13 + Q17 + Q23 + Q30 +
                   attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX, d)
#Dem + current travel behavior + attitudes (fac) + decision factors
car_build4 <- lm(Q35_1 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                   RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                   RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                   Q65_female + Q65_nonbinary + Q65_other +
                   Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                   Q13 + Q17 + Q23 + Q30 +
                   attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                   Q8_FAC1 + Q8_FAC2, d)

#Summarize results in a table
summarize_OLS_build_models(car_build1, car_build2, car_build3, car_build4, mode = "Car", pred.labels = var_labels_clean)

#Now try different strategies of how to calculate the latent factors
#Dem + current travel behavior + attitudes (sum) + decision factors
car1 <- lm(Q35_1 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
              RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
              RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
              Q65_female + Q65_nonbinary + Q65_other +
              Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
              Q13 + Q17 + Q23 + Q30 +
              attitude_car + attitude_bike + attitude_escooter + attitude_MAX +
              Q8_FAC1 + Q8_FAC2, d)
#Dem + current travel behavior + attitudes (mean) + decision factors
car2 <- lm(Q35_1 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_mean_car + attitude_mean_bike + attitude_mean_escooter + attitude_mean_MAX +
             Q8_FAC1 + Q8_FAC2, d)

#Dem + current travel behavior + attitudes (fac) + decision factors
car3 <- lm(Q35_1 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
             Q8_FAC1 + Q8_FAC2, d)

#Summarize results in a table
summarize_OLS_models(car1, car2, car3, mode = "Car", pred.labels = var_labels_clean)


#=====================#
#Switch to Bike####
#=====================#

#First, try building up the model using calculated decision factors
#Dem
bike_build1 <- lm(Q35_2 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                   RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                   RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                   Q65_female + Q65_nonbinary + Q65_other, d)
#Dem + current travel behavior
bike_build2 <- lm(Q35_2 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                   RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                   RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                   Q65_female + Q65_nonbinary + Q65_other +
                   Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                   Q13 + Q17 + Q23 + Q30, d)
#Dem + current travel behavior + attitudes (fac)
bike_build3 <- lm(Q35_2 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                   RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                   RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                   Q65_female + Q65_nonbinary + Q65_other +
                   Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                   Q13 + Q17 + Q23 + Q30 +
                   attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX, d)
#Dem + current travel behavior + attitudes (fac) + decision factors
bike_build4 <- lm(Q35_2 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                   RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                   RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                   Q65_female + Q65_nonbinary + Q65_other +
                   Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                   Q13 + Q17 + Q23 + Q30 +
                   attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                   Q8_FAC1 + Q8_FAC2, d)

#Summarize results in a table
summarize_OLS_build_models(bike_build1, bike_build2, bike_build3, bike_build4, mode = "Bike", pred.labels = var_labels_clean)

#Now try different strategies of how to calculate the latent factors
#Dem + current travel behavior + attitudes (sum) + decision factors
bike1 <- lm(Q35_2 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_car + attitude_bike + attitude_escooter + attitude_MAX +
             Q8_FAC1 + Q8_FAC2, d)
#Dem + current travel behavior + attitudes (mean) + decision factors
bike2 <- lm(Q35_2 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_mean_car + attitude_mean_bike + attitude_mean_escooter + attitude_mean_MAX +
             Q8_FAC1 + Q8_FAC2, d)

#Dem + current travel behavior + attitudes (fac) + decision factors
bike3 <- lm(Q35_2 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
             Q8_FAC1 + Q8_FAC2, d)

#Summarize results in a table
summarize_OLS_models(bike1, bike2, bike3, mode = "Bike", pred.labels = var_labels_clean)

#=====================#
#Switch to E-Scooter####
#=====================#

#First, try building up the model using calculated decision factors
#Dem
escooter_build1 <- lm(Q35_3 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                    RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                    RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                    Q65_female + Q65_nonbinary + Q65_other, d)
#Dem + current travel behavior
escooter_build2 <- lm(Q35_3 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                    RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                    RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                    Q65_female + Q65_nonbinary + Q65_other +
                    Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                    Q13 + Q17 + Q23 + Q30, d)
#Dem + current travel behavior + attitudes (fac)
escooter_build3 <- lm(Q35_3 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                    RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                    RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                    Q65_female + Q65_nonbinary + Q65_other +
                    Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                    Q13 + Q17 + Q23 + Q30 +
                    attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX, d)
#Dem + current travel behavior + attitudes (fac) + decision factors
escooter_build4 <- lm(Q35_3 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                    RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                    RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                    Q65_female + Q65_nonbinary + Q65_other +
                    Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                    Q13 + Q17 + Q23 + Q30 +
                    attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                    Q8_FAC1 + Q8_FAC2, d)

#Summarize results in a table
summarize_OLS_build_models(escooter_build1, escooter_build2, escooter_build3, escooter_build4, mode = "E-Scooter", pred.labels = var_labels_clean)

#Now try different strategies of how to calculate the latent factors
#Dem + current travel behavior + attitudes (sum) + decision factors
escooter1 <- lm(Q35_3 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_car + attitude_bike + attitude_escooter + attitude_MAX +
             Q8_FAC1 + Q8_FAC2, d)
#Dem + current travel behavior + attitudes (mean) + decision factors
escooter2 <- lm(Q35_3 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_mean_car + attitude_mean_bike + attitude_mean_escooter + attitude_mean_MAX +
             Q8_FAC1 + Q8_FAC2, d)

#Dem + current travel behavior + attitudes (fac) + decision factors
escooter3 <- lm(Q35_3 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
             Q8_FAC1 + Q8_FAC2, d)

#Summarize results in a table
summarize_OLS_models(escooter1, escooter2, escooter3, mode = "E-Scooter", pred.labels = var_labels_clean)

#=====================#
#Switch to MAX####
#=====================#

#First, try building up the model using calculated decision factors
#Dem
MAX_build1 <- lm(Q35_4 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                        RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                        RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                        Q65_female + Q65_nonbinary + Q65_other, d)
#Dem + current travel behavior
MAX_build2 <- lm(Q35_4 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                        RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                        RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                        Q65_female + Q65_nonbinary + Q65_other +
                        Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                        Q13 + Q17 + Q23 + Q30, d)
#Dem + current travel behavior + attitudes (fac)
MAX_build3 <- lm(Q35_4 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                        RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                        RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                        Q65_female + Q65_nonbinary + Q65_other +
                        Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                        Q13 + Q17 + Q23 + Q30 +
                        attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX, d)
#Dem + current travel behavior + attitudes (fac) + decision factors
MAX_build4 <- lm(Q35_4 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
                        RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
                        RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
                        Q65_female + Q65_nonbinary + Q65_other +
                        Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
                        Q13 + Q17 + Q23 + Q30 +
                        attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
                        Q8_FAC1 + Q8_FAC2, d)

#Summarize results in a table
summarize_OLS_build_models(MAX_build1, MAX_build2, MAX_build3, MAX_build4, mode = "MAX", pred.labels = var_labels_clean)

#Now try different strategies of how to calculate the latent factors
#Dem + current travel behavior + attitudes (sum) + decision factors
MAX1 <- lm(Q35_4 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_car + attitude_bike + attitude_escooter + attitude_MAX +
             Q8_FAC1 + Q8_FAC2, d)
#Dem + current travel behavior + attitudes (mean) + decision factors
MAX2 <- lm(Q35_4 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_mean_car + attitude_mean_bike + attitude_mean_escooter + attitude_mean_MAX +
             Q8_FAC1 + Q8_FAC2, d)

#Dem + current travel behavior + attitudes (fac) + decision factors
MAX3 <- lm(Q35_4 ~ Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
             RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + 
             RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
             Q65_female + Q65_nonbinary + Q65_other +
             Q11 + Q64 + Q66 + Q67 + Q69 + Q70 + Q5 + Q7_4 + RECODED_Q6 +
             Q13 + Q17 + Q23 + Q30 +
             attitude_fac_car + attitude_fac_bike + attitude_fac_escooter + attitude_fac_MAX +
             Q8_FAC1 + Q8_FAC2, d)

#Summarize results in a table
summarize_OLS_models(MAX1, MAX2, MAX3, mode = "MAX", pred.labels = var_labels_clean)
