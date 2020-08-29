#Descriptives (and cleaning)
#Mike McQueen

#Load Packages
library(dplyr)

#=====================#
#Cleaning
#=====================#

#Set current data frame from last script
d <- step1_output

#Get rid of non-students and distance learners
d2 <- d %>% 
  filter(Q5 > 0 & Q3_2 == 1)

#=====================#
#Descriptives
#=====================#

#did this in SPSS... if necessary can write code to do same thing here.



#Export this data frame for use in other scripts
step2_output <- d2

#remove the other data frames so nothing gets screwed up.
rm(d)
rm(d2)