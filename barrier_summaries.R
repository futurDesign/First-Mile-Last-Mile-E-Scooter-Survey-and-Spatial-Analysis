#Barrier Summaries
#Mike McQueen

#=====================#
#Load Packages####
#=====================#
library(dplyr)
library(tidyr)

#=====================#
#Load clean var names####
#=====================#

var_labels_clean <- read.csv("labels/var_labels_clean.csv", fileEncoding = "UTF-8-BOM")

#=====================#
#Load step4 output####
#=====================#

d <- step4_output

#=====================#
#Summarize Barriers####
#=====================#

barrier_summary <- d %>% #Select barrier columns only
  select(Q14_2:Q14_7,
         Q19_1:Q19_7,
         Q20_2:Q20_7,
         Q25_1:Q25_7,
         Q26_2:Q26_7,
         Q32_1:Q32_7,
         Q33_2:Q33_7) %>% 
  summarize_all(.funs = mean, na.rm = T) %>%  #Summarize the percent of respondents experiencing this
  pivot_longer(cols = Q14_2:Q26_6, names_to = "barriers", values_to = "percent") %>% #Turn several columns into one columns
  mutate(barriers = recode(barriers, !!!var_labels)) %>%  #Recode the values from question numbers to question text
  select(barriers, percent)

write.csv(barrier_summary, "Exports/Barriers/barrier_summary.csv")
