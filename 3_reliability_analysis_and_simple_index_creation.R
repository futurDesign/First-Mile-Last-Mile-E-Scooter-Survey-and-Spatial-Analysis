#Reliability Analysis and Simple Index Creation
#Mike McQueen

#Set current data frame from last script
d <- step2_output

#=====================#
#Reliability Analsyis
#=====================#

#did this in SPSS... if necessary can write code to do same thing here.
#When we did this in SPSS, we decided to remove an item or two from each index because
#it improved the reliability score

#=====================#
#Index Creation
#=====================#

#Create sum and mean index scores. Will include all items answered and ignore items not answered.
d2 <- d %>%
  mutate(attitude_car = rowSums(select(., Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6, Q15_8), na.rm = T),
         attitude_mean_car = rowMeans(select(., Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6, Q15_8), na.rm = T),
         attitude_bike = rowSums(select(., Q21_1:Q21_4, Q21_5_r, Q21_7_r, Q21_8:Q21_10), na.rm = T),
         attitude_mean_bike = rowMeans(select(., Q21_1:Q21_4, Q21_5_r, Q21_7_r, Q21_8:Q21_10), na.rm = T),
         attitude_escooter = rowSums(select(., Q28_1:Q28_2, Q28_3_r, Q28_4, Q28_5_r, Q28_7_r, Q28_8:Q28_11), na.rm = T),
         attitude_mean_escooter = rowMeans(select(., Q28_1:Q28_2, Q28_3_r, Q28_4, Q28_5_r, Q28_7_r, Q28_8:Q28_11), na.rm = T),
         attitude_MAX = rowSums(select(., Q34_1:Q34_9, Q34_11), na.rm = T),
         attitude_mean_MAX = rowMeans(select(., Q34_1:Q34_9, Q34_11)), na.rm = T) %>% 
  mutate_at(.vars = vars(attitude_car:attitude_mean_MAX), ~na_if(., 0)) %>% #R will understand all items unanswered as "0," but it should actually be system na. Fix this
  mutate_at(.vars = vars(attitude_car:attitude_mean_MAX), ~na_if(., "NaN")) #Fix "NaN" by making it system na

#Export this data frame for use in other scripts
step3_output <- d2

#remove the other data frames so nothing gets screwed up.
rm(d)
rm(d2)


            