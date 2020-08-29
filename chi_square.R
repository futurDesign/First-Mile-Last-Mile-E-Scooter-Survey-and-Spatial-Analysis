#Chi Square
#Mike McQueen

#=====================#
#Load packages####
#=====================#
library(dplyr)
library(tidyr)
library(MASS)

#=====================#
#Load data####
#=====================#
d <- step4_output %>% 
  filter(RECODED_Q63 == "white" | RECODED_Q63 == "hisp_lat" | RECODED_Q63 == "black" | RECODED_Q63 == "asian" ,
         Q65 == "male" | Q65 == "female") %>% 
  droplevels

#=====================#
#Helper Functions####
#=====================#
assess <- function(x, y) {
  tbl <- table(x, y)
  print(tbl)
  print(prop.table(tbl, margin = 2))
  chisq.test(tbl)
}

#=====================#
#Assess proportions experiencing barriers####
#=====================#

#Never tried riding an e-scooter before
#gender
assess(d$Q25_1, d$Q65)

#race
assess(d$Q25_1, d$RECODED_Q63)

#Don't feel comfortable riding in traffic
#gender
assess(d$Q25_11, d$Q65)

#race
assess(d$Q25_11, d$RECODED_Q63)

#Don't want to ride when the weather is bad
#gender
assess(d$Q25_8, d$Q65)

#race
assess(d$Q25_8, d$RECODED_Q63)

#Can't count on an e-scooter being around when they need it
#gender
assess(d$Q26_5, d$Q65)

#race
assess(d$Q26_5, d$RECODED_Q63)

#Can't count afford to ride an e-scooter regularly
#gender
assess(d$Q25_12, d$Q65)

#race
assess(d$Q25_12, d$RECODED_Q63)

#Not enough dedicated lanes to ride in
#gender
assess(d$Q26_3, d$Q65)

#race
assess(d$Q26_3, d$RECODED_Q63)

#I live outside of an e-scooter service area
#gender
assess(d$Q26_2, d$Q65)

#race
assess(d$Q26_2, d$RECODED_Q63)

#I am satisifed with my current amount of e-scooter use
#gender
assess(d$Q26_8, d$Q65)

#race
assess(d$Q26_8, d$RECODED_Q63)
