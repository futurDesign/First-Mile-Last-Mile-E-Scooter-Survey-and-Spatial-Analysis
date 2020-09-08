#predict using mnlogit model type

mnlogit_predict <- function(newdata, model) {

#load the model coefficients into a named vector
coefs <- model$coefficients

#Calculate the utility, odds, and probability for each mode and indicate the predicted discrete choice
newdata2 <- newdata %>% 
  mutate(v_car = 0 + coefs["car_walk_time"] * car_walk_time + coefs["car_drive_time"] * car_drive_time + coefs["car_parking_cost"] * car_parking_cost,
         v_bike = coefs["(Intercept):2.bike"] + coefs["bike_ride_time"] * bike_ride_time + 
           coefs["Q68_inc_2:2.bike"] * Q68_inc_2 + coefs["Q68_inc_3:2.bike"] * Q68_inc_3 + coefs["Q68_inc_4:2.bike"] * Q68_inc_4 + coefs["Q68_inc_5:2.bike"] * Q68_inc_5 + coefs["Q68_inc_6:2.bike"] * Q68_inc_6 +
           coefs["RECODED_Q63_am_ind_ala:2.bike"] * RECODED_Q63_am_ind_ala + coefs["RECODED_Q63_asian:2.bike"] * RECODED_Q63_asian + coefs["RECODED_Q63_black:2.bike"] * RECODED_Q63_black + coefs["RECODED_Q63_hisp_lat:2.bike"] * RECODED_Q63_hisp_lat + coefs["RECODED_Q63_more_than_one_race:2.bike"] * RECODED_Q63_more_than_one_race + 
           coefs["RECODED_Q63_nat_haw_pac:2.bike"] * RECODED_Q63_nat_haw_pac + coefs["RECODED_Q63_other:2.bike"] * RECODED_Q63_other +
           coefs["Q65_female:2.bike"] * Q65_female + coefs["Q65_nonbinary:2.bike"] * Q65_nonbinary + coefs["Q65_other:2.bike"] * Q65_other +
           coefs["Q11:2.bike"] * Q11 + coefs["Q64:2.bike"] * Q64 + coefs["Q66:2.bike"] * Q66 + coefs["Q67:2.bike"] * Q67 + coefs["Q69:2.bike"] * Q69 + coefs["Q70:2.bike"] * Q70 + coefs["Q5:2.bike"] * Q5 + coefs["Q7_4:2.bike"] * Q7_4 + coefs["RECODED_Q6_activetransport:2.bike"] * RECODED_Q6_activetransport + coefs["RECODED_Q6_publictransit:2.bike"] * RECODED_Q6_publictransit + coefs["RECODED_Q6_other:2.bike"] * RECODED_Q6_other +
           coefs["Q13:2.bike"] * Q13 + coefs["Q17:2.bike"] * Q17 + coefs["Q23:2.bike"] * Q23 + coefs["Q30:2.bike"] * Q30 +
           coefs["attitude_fac_car:2.bike"] * attitude_fac_car + coefs["attitude_fac_bike:2.bike"] * attitude_fac_bike + coefs["attitude_fac_escooter:2.bike"] * attitude_fac_escooter + coefs["attitude_fac_MAX:2.bike"] * attitude_fac_MAX +
           coefs["Q8_FAC1:2.bike"] * Q8_FAC1 + coefs["Q8_FAC2:2.bike"] * Q8_FAC2,
         v_em = coefs["(Intercept):3.em"] + coefs["em_walk_time"] * em_walk_time + coefs["em_scoot_time"] * em_scoot_time + coefs["em_scoot_cost"] * em_scoot_cost + coefs["em_ticket_cost"] * em_ticket_cost + coefs["em_ride_time"] * em_ride_time +
           coefs["Q68_inc_2:3.em"] * Q68_inc_2 + coefs["Q68_inc_3:3.em"] * Q68_inc_3 + coefs["Q68_inc_4:3.em"] * Q68_inc_4 + coefs["Q68_inc_5:3.em"] * Q68_inc_5 + coefs["Q68_inc_6:3.em"] * Q68_inc_6 +
           coefs["RECODED_Q63_am_ind_ala:3.em"] * RECODED_Q63_am_ind_ala + coefs["RECODED_Q63_asian:3.em"] * RECODED_Q63_asian + coefs["RECODED_Q63_black:3.em"] * RECODED_Q63_black + coefs["RECODED_Q63_hisp_lat:3.em"] * RECODED_Q63_hisp_lat + coefs["RECODED_Q63_more_than_one_race:3.em"] * RECODED_Q63_more_than_one_race + 
           coefs["RECODED_Q63_nat_haw_pac:3.em"] * RECODED_Q63_nat_haw_pac + coefs["RECODED_Q63_other:3.em"] * RECODED_Q63_other +
           coefs["Q65_female:3.em"] * Q65_female + coefs["Q65_nonbinary:3.em"] * Q65_nonbinary + coefs["Q65_other:3.em"] * Q65_other +
           coefs["Q11:3.em"] * Q11 + coefs["Q64:3.em"] * Q64 + coefs["Q66:3.em"] * Q66 + coefs["Q67:3.em"] * Q67 + coefs["Q69:3.em"] * Q69 + coefs["Q70:3.em"] * Q70 + coefs["Q5:3.em"] * Q5 + coefs["Q7_4:3.em"] * Q7_4 + coefs["RECODED_Q6_activetransport:3.em"] * RECODED_Q6_activetransport + coefs["RECODED_Q6_publictransit:3.em"] * RECODED_Q6_publictransit + coefs["RECODED_Q6_other:3.em"] * RECODED_Q6_other +
           coefs["Q13:3.em"] * Q13 + coefs["Q17:3.em"] * Q17 + coefs["Q23:3.em"] * Q23 + coefs["Q30:3.em"] * Q30 +
           coefs["attitude_fac_car:3.em"] * attitude_fac_car + coefs["attitude_fac_bike:3.em"] * attitude_fac_bike + coefs["attitude_fac_escooter:3.em"] * attitude_fac_escooter + coefs["attitude_fac_MAX:3.em"] * attitude_fac_MAX +
           coefs["Q8_FAC1:3.em"] * Q8_FAC1 + coefs["Q8_FAC2:3.em"] * Q8_FAC2) %>% 
  #calculate the odds by exponentiating the utility function
  mutate(odds_car = exp(v_car),
         odds_bike = exp(v_bike),
         odds_em = exp(v_em)) %>% 
  #calculate the probability with odds_i / sum(odds_j)
  mutate(p_car = odds_car / (odds_car + odds_bike + odds_em),
         p_bike = odds_bike / (odds_car + odds_bike + odds_em),
         p_em = odds_em / (odds_car + odds_bike + odds_em)) %>%
  #indicate which mode was predicted (had highest probability of being selected)
  mutate(predicted_choice = case_when(
    p_car > p_bike & p_car > p_em ~ "car",
    p_bike > p_car & p_bike > p_em ~ "bike",
    p_em > p_car & p_em > p_bike ~ "em")) %>% 
  #remove rows with na's (indicates that a variable was missing and thus the prediction could not be made)
  drop_na(v_car, v_bike, v_em)

return(newdata2 %>% select(v_car:predicted_choice))
}
