#EFA
#Mike McQueen


#Load packages
library(psych)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(EFA.dimensions) #for MAP, PARALLEL functions

#Set current data frame from last script
d <- step3_output

#Define scree plot plotting function
plotscree <- function(eigenvalues, par_eigenvalues, mode) {
  data <- data.frame(eigenvalues = eigenvalues)
  data$xval <- 1:nrow(data)
  data$par_eigenvalues <- par_eigenvalues
  g <- ggplot(data, aes(xval, eigenvalues)) +
    geom_point(size = 2, color = "slateblue4") +
    geom_line(size = 1.25, color = "slateblue4") +
    geom_hline(aes(yintercept = 1), color = "red", size = 1, linetype = "dotted") +
    geom_point(aes(xval, par_eigenvalues), size = 2, color = "orange") +
    geom_line(aes(xval, par_eigenvalues), size = 1.25, color = "orange", linetype = "longdash") +
    scale_x_continuous(labels = as.character(data$xval), breaks = data$xval) +
    ylim(0, 5) +
    labs(title = paste(mode, "Item Eigenvalues"), x = "Component", y = "Eigenvalue")
  return (g)
}

#=====================
#Mode Decision Factors
#=====================

#data
mdf <- d %>% select(Q8_1:Q8_15)

#Perform PCA
mdf_PCA <- principal(mdf, 15, rotate = "varimax")

#Perform Parallel Analysis test
mdf_parallel <- PARALLEL(Nvars = mdf_PCA$factors, Ncases = nrow(mdf_PCA$scores), verbose = F)

#Perform EFA and make scree plot
mdf_EFA <- fa(mdf, nfactors = 2, rotate = "varimax")
mdf_EFA_plot <- plotscree(mdf_EFA$e.values, mdf_parallel$eigenvalues[,3], "Mode Decision Factor")
ggsave("Exports/ScreePlots/mdf_EFA.png", mdf_EFA_plot, dpi = 700, scale = .75)
write.csv(mdf_EFA$loadings, "Exports/EFA_Loadings/mdf_EFA.csv")

#Perform MAP test
mdf_map <- MAP(mdf, corkind = "pearson")

#=====================
#Personal Car
#=====================

#data
car <- d %>% select(Q15_1:Q15_6, Q15_7_r, Q15_8)


#Perform PCA
car_PCA <- principal(car, 8, rotate = "varimax")

#Perform Parallel Analysis test
car_parallel <- PARALLEL(Nvars = car_PCA$factors, Ncases = nrow(car_PCA$scores), verbose = F)

#Perform EFA
car_EFA <- fa(car, nfactors = 1, rotate = "varimax")
car_EFA_plot <- plotscree(car_EFA$e.values, car_parallel$eigenvalues[,3], "Personal Car Attitude")
ggsave("Exports/ScreePlots/car_EFA.png", car_EFA_plot, dpi = 700, scale = .75)
write.csv(car_EFA$loadings, "Exports/EFA_Loadings/car_EFA.csv")

#Perform MAP test
car_map <- MAP(car, corkind = "pearson")

#=====================
#Bike
#=====================

#data
bike <- d %>% select(Q21_1:Q21_4, Q21_5_r, Q21_6_r, Q21_7_r, Q21_8:Q21_10)

#Perform PCA
bike_PCA <- principal(bike, 10, rotate = "varimax")

#Perform Parallel Analysis test
bike_parallel <- PARALLEL(Nvars = bike_PCA$factors, Ncases = nrow(bike_PCA$scores), verbose = F)

#Perform EFA
bike_EFA <- fa(bike, nfactors = 1, rotate = "varimax")
bike_EFA_plot <- plotscree(bike_EFA$e.values, bike_parallel$eigenvalues[,3], "Bike Attitude")
ggsave("Exports/ScreePlots/bike_EFA.png", bike_EFA_plot, dpi = 700, scale = .75)
write.csv(bike_EFA$loadings, "Exports/EFA_Loadings/bike_EFA.csv")

#Perform MAP test
bike_map <- MAP(bike, corkind = "pearson")

#=====================
#E-Scooter
#=====================

#data
escooter <- d %>% select(Q28_1:Q28_2, Q28_3_r, Q28_4, Q28_5_r, Q28_6_r, Q28_7_r, Q28_8:Q28_11)

#Perform PCA
escooter_PCA <- principal(escooter, 11, rotate = "varimax")

#Perform Parallel Analysis test
escooter_parallel <- PARALLEL(Nvars = escooter_PCA$factors, Ncases = nrow(escooter_PCA$scores), verbose = F)

#Perform EFA
escooter_EFA <- fa(escooter, nfactors = 1, rotate = "varimax")
escooter_EFA_plot <- plotscree(escooter_EFA$e.values, escooter_parallel$eigenvalues[,3], "E-Scooter Attitude")
ggsave("Exports/ScreePlots/escooter_EFA.png", escooter_EFA_plot, dpi = 700, scale = .75)
write.csv(escooter_EFA$loadings, "Exports/EFA_Loadings/escooter_EFA.csv")

#Perform MAP test
escooter_map <- MAP(escooter, corkind = "pearson")

#=====================
#MAX
#=====================

#data
MAX <- d %>% select(Q34_1:Q34_9, Q34_10_r, Q34_11)

#Perform PCA
MAX_PCA <- principal(MAX, 11, rotate = "varimax")

#Perform Parallel Analysis test
MAX_parallel <- PARALLEL(Nvars = MAX_PCA$factors, Ncases = nrow(MAX_PCA$scores), verbose = F)

#Perform EFA
MAX_EFA <- fa(MAX, nfactors = 1, rotate = "varimax")
MAX_EFA_plot <- plotscree(MAX_EFA$e.values, MAX_parallel$eigenvalues[,3], "MAX Attitude")
ggsave("Exports/ScreePlots/max_EFA.png", MAX_EFA_plot, dpi = 700, scale = .75)
write.csv(MAX_EFA$loadings, "Exports/EFA_Loadings/MAX_EFA.csv")

#Perform MAP test
MAX_map <- MAP(MAX, corkind = "pearson")

#=====================
#Wrap all plots to individual item
#=====================

g <- grid.arrange(mdf_EFA_plot, car_EFA_plot, bike_EFA_plot, escooter_EFA_plot, MAX_EFA_plot, nrow = 3)
ggsave("Exports/ScreePlots/all.png", g, dpi = 700, width = 7, units = "in", scale = 1.25)

#=====================
#Print MAP test results into table
#=====================

MAP <- data.frame(Scale = c("Mode Decision Factor", "Personal Car", "Bike", "E-Scooter", "MAX"),
                  "Number of Factors" = c(mdf_map$nfMAP, car_map$nfMAP, bike_map$nfMAP, escooter_map$nfMAP, MAX_map$nfMAP))
write.csv(MAP, "Exports/ScreePlots/MAPtest.csv")

#=====================
#Add factor scores to main data frame
#=====================
d2 <- cbind(d,
            Q8_FAC1 = mdf_EFA$scores[,1],
            Q8_FAC2 = mdf_EFA$scores[,2],
            attitude_fac_car = car_EFA$scores[,1],
            attitude_fac_bike = bike_EFA$scores[,1],
            attitude_fac_escooter = escooter_EFA$scores[,1],
            attitude_fac_MAX = MAX_EFA$scores[,1])

#Export this data frame for use in other scripts
step4_output <- d2

write.csv(d2, "Exports/cleaned_data/step4_output.csv")
save(step4_output, file = "Exports/cleaned_data/step4_output.RData")

#remove the other data frames so nothing gets screwed up.
#rm(list = ls()[ls() != "step4_output" & ls() != "var_labels"])

