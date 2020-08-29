#Demonstration of Weighting Function
#Mike McQueen

#Load required packages
library(ggplot2)

#Create data set
x <- 1:15
y <- tanh(x/5)
data <- tibble(x,y)

#plot
g <- ggplot(data, aes(x = x, y = y)) +
  geom_line() +
  geom_point() +
  labs(title = "Demonstration of Hyperbolic Tangent Weighting Function",
       x = "Count of Respondents",
       y = "Weight (tanh(x/5))") +
  theme_bw()

#print
ggsave(g,
       filename = "Exports/Barriers/weighting_function.png",
       height = 4,
       units = "in",
       dpi = 450)
