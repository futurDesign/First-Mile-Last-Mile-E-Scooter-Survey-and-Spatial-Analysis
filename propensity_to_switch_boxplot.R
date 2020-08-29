#boxplot for distribution of propensity to switch responses

#packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

#Rearrange data for boxplot
d <- step4_output %>% 
  select(Q35_1:Q35_4) %>% 
  pivot_longer(cols = Q35_1:Q35_4,
               names_to = "Mode",
               values_to = "Score") %>% 
  mutate(Mode = recode(Mode, `Q35_1` = "Car", `Q35_2` = "Bike", `Q35_3` = "E-Scooter", `Q35_4` = "MAX")) %>% 
  drop_na()

#Make boxplot
boxplot <- ggplot(d, aes(Score, Mode)) +
  geom_boxplot(fill = viridis(4))

#Print boxplot
ggsave(boxplot,
       filename = "Exports/propensity_to_switch_box/switch.png",
       units = "in",
       height = 4,
       width = 4,
       dpi = 500)
