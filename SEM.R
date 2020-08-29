#SEM
#Mike McQueen

#Load packages
library(lavaan)
library(semPlot)

#Define active dataset
d <- step4_output

#==============================================
#SEM - linear regression, propensity to switch
#==============================================
carSwitchModel <- '
  # measurement model
    comf_saf =~ Q8_8 + Q8_9 + Q8_10 + Q8_11 + Q8_11 + Q8_12 + Q8_15
    ex_env =~ Q8_1 + Q8_2 + Q8_3 + Q8_13
    MAX_attitude =~ Q34_1 + Q34_2 + Q34_3 + Q34_4
    bike_attitude =~ Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q21_8 + Q21_9 + Q21_10
    car_attitude =~ Q15_1 + Q15_2 + Q15_4 + Q15_6 + Q15_8
    escooter_attitude =~ Q28_1 + Q28_2 + Q28_3_r + Q28_4 + Q28_8 + Q28_9 + Q28_10 + Q28_11
  # regressions
    Q35_1 ~ comf_saf + ex_env + car_attitude + MAX_attitude + bike_attitude + escooter_attitude +
    RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
    Q65_female + Q65_nonbinary + Q65_other +
    Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
    Q11 +
    Q64 +
    Q66 +
    Q67 +
    Q69 +
    Q70 +
    Q5 +
    Q7_4 +
    RECODED_Q6_activetransport + RECODED_Q6_other + RECODED_Q6_publictransit +
    Q13 +
    Q17 +
    Q23 +
    Q30
    Q35_1 ~ 1
    '
carSwitchFit <- sem(carSwitchModel, data = d)
summary(carSwitchFit)

bikeSwitchModel <- '
  # measurement model
    comf_saf =~ Q8_8 + Q8_9 + Q8_10 + Q8_11 + Q8_11 + Q8_12 + Q8_15
    ex_env =~ Q8_1 + Q8_2 + Q8_3 + Q8_13
    MAX_attitude =~ Q34_1 + Q34_2 + Q34_3 + Q34_4
    bike_attitude =~ Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q21_8 + Q21_9 + Q21_10
    car_attitude =~ Q15_1 + Q15_2 + Q15_4 + Q15_6 + Q15_8
    escooter_attitude =~ Q28_1 + Q28_2 + Q28_3_r + Q28_4 + Q28_8 + Q28_9 + Q28_10 + Q28_11
  # regressions
    Q35_2 ~ comf_saf + ex_env + car_attitude + MAX_attitude + bike_attitude + escooter_attitude +
    RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
    Q65_female + Q65_nonbinary + Q65_other +
    Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
    Q11 +
    Q64 +
    Q66 +
    Q67 +
    Q69 +
    Q70 +
    Q5 +
    Q7_4 +
    RECODED_Q6_activetransport + RECODED_Q6_other + RECODED_Q6_publictransit +
    Q13 +
    Q17 +
    Q23 +
    Q30
    Q35_2 ~ 1
    '
bikeSwitchFit <- sem(bikeSwitchModel, data = d)
summary(bikeSwitchFit)

MAXSwitchModel <- '
  # measurement model
    comf_saf =~ Q8_8 + Q8_9 + Q8_10 + Q8_11 + Q8_11 + Q8_12 + Q8_15
    ex_env =~ Q8_1 + Q8_2 + Q8_3 + Q8_13
    MAX_attitude =~ Q34_1 + Q34_2 + Q34_3 + Q34_4
    bike_attitude =~ Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q21_8 + Q21_9 + Q21_10
    car_attitude =~ Q15_1 + Q15_2 + Q15_4 + Q15_6 + Q15_8
    escooter_attitude =~ Q28_1 + Q28_2 + Q28_3_r + Q28_4 + Q28_8 + Q28_9 + Q28_10 + Q28_11
  # regressions
    Q35_4 ~ comf_saf + ex_env + car_attitude + MAX_attitude + bike_attitude + escooter_attitude +
    RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
    Q65_female + Q65_nonbinary + Q65_other +
    Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
    Q11 +
    Q64 +
    Q66 +
    Q67 +
    Q69 +
    Q70 +
    Q5 +
    Q7_4 +
    RECODED_Q6_activetransport + RECODED_Q6_other + RECODED_Q6_publictransit +
    Q13 +
    Q17 +
    Q23 +
    Q30
    Q35_4 ~ 1
    '
MAXSwitchFit <- sem(MAXSwitchModel, data = d)
summary(MAXSwitchFit)

escooterSwitchModel <- '
  # measurement model
    comf_saf =~ Q8_8 + Q8_9 + Q8_10 + Q8_11 + Q8_11 + Q8_12 + Q8_15
    ex_env =~ Q8_1 + Q8_2 + Q8_3 + Q8_13
    MAX_attitude =~ Q34_1 + Q34_2 + Q34_3 + Q34_4
    bike_attitude =~ Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q21_8 + Q21_9 + Q21_10
    car_attitude =~ Q15_1 + Q15_2 + Q15_4 + Q15_6 + Q15_8
    escooter_attitude =~ Q28_1 + Q28_2 + Q28_3_r + Q28_4 + Q28_8 + Q28_9 + Q28_10 + Q28_11
  # regressions
    Q35_3 ~ comf_saf + ex_env + car_attitude + MAX_attitude + bike_attitude + escooter_attitude +
    RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
    Q65_female + Q65_nonbinary + Q65_other +
    Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
    Q11 +
    Q64 +
    Q66 +
    Q67 +
    Q69 +
    Q70 +
    Q5 +
    Q7_4 +
    RECODED_Q6_activetransport + RECODED_Q6_other + RECODED_Q6_publictransit +
    Q13 +
    Q17 +
    Q23 +
    Q30
    Q34_3 ~ 1
    '
escooterSwitchFit <- sem(escooterSwitchModel, data = d)
summary(escooterSwitchFit)

red_escooterSwitchModel <- '
  # measurement model
    MAX_attitude =~ Q34_1 + Q34_2 + Q34_3 + Q34_4
  # regressions
    Q35_4 ~ MAX_attitude +
    RECODED_Q63_more_than_one_race +
    Q64 +
    RECODED_Q6_activetransport + RECODED_Q6_other + RECODED_Q6_publictransit +
    Q30
    '
red_escooterSwitchFit <- sem(red_escooterSwitchModel, data = d)
summary(red_escooterSwitchFit)

#Export estimates to file
write.csv(parameterEstimates(carSwitchFit),"Exports/Switch/carSEM.csv")
write.csv(standardizedsolution(carSwitchFit),"Exports/Switch/carSEM_std.csv")
write.csv(parameterEstimates(bikeSwitchFit),"Exports/Switch/bikeSEM.csv")
write.csv(standardizedsolution(bikeSwitchFit),"Exports/Switch/bikeSEM_std.csv")
write.csv(parameterEstimates(escooterSwitchFit),"Exports/Switch/escooterSEM.csv")
write.csv(standardizedsolution(escooterSwitchFit),"Exports/Switch/escooterSEM_std.csv")
write.csv(parameterEstimates(MAXSwitchFit),"Exports/Switch/MAXSEM.csv")
write.csv(standardizedsolution(MAXSwitchFit),"Exports/Switch/MAXSEM_std.csv")

#Make model plots
semPaths(carSwitchFit, "std", "Estimates")
semPaths(bikeSwitchFit, "std", "Estimates")
semPaths(MAXSwitchFit, "std", "Estimates")
semPaths(escooterSwitchFit, "std", "Estimates")
semPaths(red_escooterSwitchFit, "std", "Estimates")

#Get number of observations FYI
lavInspect(carSwitchFit, "nobs")
lavInspect(bikeSwitchFit, "nobs")
lavInspect(MAXSwitchFit, "nobs")
lavInspect(escooterSwitchFit, "nobs")

#==============================================
###BROKEN###
#SEM - ordinal regression, number of trips in last 7 days
#==============================================

#escooterTripsModel <- '
  # measurement model
    #comf_saf =~ Q8_8 + Q8_9 + Q8_10 + Q8_11 + Q8_11 + Q8_12 + Q8_15
    #ex_env =~ Q8_1 + Q8_2 + Q8_3 + Q8_13
    #MAX_attitude =~ Q34_1 + Q34_2 + Q34_3 + Q34_4
    #bike_attitude =~ Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q21_8 + Q21_9 + Q21_10
    #car_attitude =~ Q15_1 + Q15_2 + Q15_4 + Q15_6 + Q15_8
    #escooter_attitude =~ Q28_1 + Q28_2 + Q28_3_r + Q28_4 + Q28_8 + Q28_9 + Q28_10 + Q28_11
  # regressions
    #Q23 ~ comf_saf + ex_env + car_attitude + MAX_attitude + bike_attitude + escooter_attitude +
    #RECODED_Q63_am_ind_ala + RECODED_Q63_asian + RECODED_Q63_black + RECODED_Q63_hisp_lat + RECODED_Q63_more_than_one_race + RECODED_Q63_nat_haw_pac + RECODED_Q63_other +
    #Q65_female + Q65_nonbinary + Q65_other +
    #Q68_inc_2 + Q68_inc_3 + Q68_inc_4 + Q68_inc_5 + Q68_inc_6 +
    #Q11 +
    #Q64 +
    #Q66 +
    #Q67 +
    #Q69 +
    #Q70 +
    #Q5 +
    #Q7_4 +
    #RECODED_Q6_activetransport + RECODED_Q6_other + RECODED_Q6_publictransit +
    #Q13 +
    #Q17 +
    #Q23 +
    #Q30
    #'
#escooterTripsFit <- sem(escooterTripsModel, data = d, ordered = "Q23", estimator = "MML", link = "logit")
