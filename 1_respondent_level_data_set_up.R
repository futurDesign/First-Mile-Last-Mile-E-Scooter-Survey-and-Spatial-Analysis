#Respondent Level Data Set Up
#Mike McQueen

#Load Packages
library(dplyr)
library(labelled)
library(sjlabelled)
library(haven)
library(fastDummies)

#Load data
draw <- read_spss("../../SPSS/cleandata from R/respondent_clean_032320.sav")

#Extract Variable Labels
var_labels <- var_label(draw)

#Turn data in SPSS format into R friendly format
d <- draw %>% 
  mutate_at(.vars = vars(Q2, Q3_2:Q3_9, Q5, Q6, Q7_4, Q8_1:Q8_15, Q11, Q12, Q13, Q14_2: Q14_7, Q15_1:Q15_8, Q17, Q19_1:Q19_7,
                         Q20_2:Q20_7, Q21_1:Q21_10, Q23, Q25_1:Q26_7, Q26_2:Q26_7, Q27, Q28_1:Q28_11, Q30, Q32_1:Q32_7,
                         Q33_2:Q33_7, Q34_1:Q35_4,  Q63_1:Q63_6, Q64, Q67, Q66, Q69, Q70), .funs = zap_labels) %>% #Zap labels of dummy variables and Likert scales so that the 0/1 and 1-5 behavior is preserved
  as_factor(only_labelled = TRUE) %>% #Factor appropriate vectors
  set_na(na = c(-7, -1, "Refused", "Appropriate Skip", "Refused or Prefer not to answer"), drop.levels = TRUE) %>% #Make missing values system missing and drop corresponding levels
  zap_label() %>% #Since we exported our variable labels, can delete them
  zap_labels() %>%  #Since we converted labelled columns to factor columns, can delete value labels
  zap_formats() %>% #deletes format attribute (for cleanliness only)
  zap_widths() #deletes display width attribute (for cleanliness only)

#Create PSU-style ethnicity/race field
d1 <- d %>% 
  mutate(RECODED_Q63 = case_when(
    Q63_1 == 1  & Q63_2 == 0  &  Q63_3  == 0 & Q63_7 == 0  &  Q63_8 == 0  & Q63_4 == 0 & Q63_5 == 0  &  Q63_6 == 0 ~ "American Indian or Alaska Native",
    Q63_2 = 1  & Q63_1 == 0  &  Q63_3  == 0 & Q63_7 == 0  &  Q63_8 == 0  &  Q63_4 == 0 & Q63_5 == 0  &  Q63_6 == 0 ~ "Asian",
    Q63_3 = 1  & Q63_1 == 0  &  Q63_2  == 0 & Q63_7 == 0  &  Q63_8 == 0  &  Q63_4 == 0 & Q63_5 == 0  &  Q63_6 == 0 ~ "Black",
    Q63_7 = 1  & Q63_1 == 0  &  Q63_2  == 0 & Q63_3 == 0  &  Q63_8 == 0  &  Q63_4 == 0 & Q63_5 == 0  &  Q63_6 == 0 ~ "White",
    Q63_8 = 1  & Q63_1 == 0  &  Q63_2  == 0 & Q63_3 == 0  &  Q63_7 == 0  &  Q63_4 == 0 & Q63_5 == 0 &  Q63_6 == 0 ~ "Native Hawaiian or Pacific Islander",
    Q63_4 = 1  & Q63_1 == 0  &  Q63_2  == 0 & Q63_3 == 0  &  Q63_7 == 0  &  Q63_8 == 0 & Q63_5 == 0  &  Q63_6 == 0 ~ "Hispanic or Latinx/Latino/Latina",
    Q63_5 == 1 ~ "Prefer not to say",
    Q63_6 == 1  & Q63_1 == 0  &  Q63_2  == 0 & Q63_3 == 0  &  Q63_7 == 0  &  Q63_8 == 0 & Q63_5 == 0 &  Q63_4 == 0 ~ "Other",
    sum(Q63_1, Q63_2, Q63_3, Q63_7, Q63_8, Q63_4, Q63_6, na.rm = TRUE) > 1 & Q63_5 == 0 ~ "More than one race or ethnicity")
    ) %>%
  mutate(RECODED_Q63 = ifelse(Q63_5 == 1, NA, RECODED_Q63)) %>% #Convert prefer not to say to system missing
  mutate(RECODED_Q63 = factor(RECODED_Q63)) #Convert these to factors

#Reverse code appropriate items
#First, create helper function
reverse_code <-  function(x) {
  x <- case_when(
    x == 1 ~ 5,
    x == 2 ~ 4,
    x == 3 ~ 3,
    x == 4 ~ 2,
    x == 5 ~ 3
  )
  return(x)
}

#Next, recode appropriate reversed items into new columns
d2 <- d1 %>%
  mutate_at(.vars = vars(Q15_7, Q21_5, Q21_6, Q21_7, Q28_3, Q28_5, Q28_6, Q28_7, Q34_10),
            .funs = list(r = reverse_code))

#Combine primary mode of transport variables into fewer categories
d3 <- d2 %>% 
  mutate(RECODED_Q6 = recode(Q6, "3" = "1", "8" = "1", "9" = "1", "11" = "1", #Personal Vehicle
                             "1" = "2", "2" = "2", "6" = "2", "7" = "2", "10" = "2", "15" = "2", #Active Transit
                             "4" = "3", "5" = "3", "13" = "3", #Public Transit
                             "14" = "4")) %>% #Other
  mutate(RECODED_Q6 = factor(RECODED_Q6, labels = c("personalvehicle", "activetransport", "publictransit", "other")))

#Adjust labels of certain variables to play nicer with syntax
d4 <- d3 %>% 
  mutate(RECODED_Q63 = recode_factor(RECODED_Q63, "American Indian or Alaska Native" = "am_ind_ala",
            "Asian" = "asian",
            "Black" = "black",
            "Hispanic or Latinx/Latino/Latina" = "hisp_lat",
            "More than one race or ethnicity" = "more_than_one_race",
            "Native Hawaiian or Pacific Islander" = "nat_haw_pac",
            "Other" = "other",
            "White" = "white"),
         Q65 = recode_factor(Q65, "Male" = "male",
                                    "Female" = "female",
                                    "Non-binary" = "nonbinary",
                                    "Other: (type response in box)" = "other"),
         Q68 = recode_factor(Q68, "Less than $25,000" = "inc_1",
                             "$25,000 - $49,999" = "inc_2",
                             "$50,000 - $74,999" = "inc_3",
                             "$75,000 - $99,999" = "inc_4",
                             "$100,000 - $124,999" = "inc_5",
                             "Greater than $125,000" = "inc_6"))
         
#Recode ordinal variables so that numbering starts at 0 instead of 1 where necessary
d5 <- d4 %>% 
  mutate(Q13 = recode(Q13, `1` = 0, `2` = 1, `3` = 2, `4` = 3), #These are the # of trips in last 7 days variables
         Q17 = recode(Q17, `1` = 0, `2` = 1, `3` = 2, `4` = 3),
         Q23 = recode(Q23, `1` = 0, `2` = 1, `3` = 2, `4` = 3),
         Q30 = recode(Q30, `1` = 0, `2` = 1, `3` = 2, `4` = 3))

#Make dummy variabes as necessary
d6 <- d5 %>%
  dummy_cols(select_columns = c("RECODED_Q63", "RECODED_Q6", "Q65", "Q68"))

#Export this data frame for use in other scripts
step1_output <- d6

#remove the other data frames so nothing gets screwed up.
rm(draw)
rm(d)
rm(d1)
rm(d2)
rm(d3)
rm(d4)
rm(d5)
rm(d6)
