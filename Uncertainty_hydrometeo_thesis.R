#Author: Luiza Freitas 
#Objective: assess uncertainty of estimated hydrometeorological data
#Date: 07/04/2021

# INSTALL AND LOAD PACKAGES ################################
install.packages("pastecs")
install.packages("hrbrthemes")
install.packages("lubridate")
install.packages("RANN")
install.packages("tibbletime")


library(datasets)  # Load base packages manually
library(devtools)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(pastecs)
library(caret)
library(lubridate)
library(tidyr)

####################### SET WORK DIRECTORY ##############################

setwd("C:/Data")

############################ IMPORT DATA ###########################

hydrometeo <- read.csv("hydrometeo_combined_copy.csv", header = TRUE)
hm <- as.data.frame(hydrometeo)
View(hm)
str(hm)

data_num <- as.data.frame(apply(hm, 2, as.numeric))
View(data_num)

uncertainty_BA <- read.csv("BA_uncertainty_copy.csv", header = TRUE)
uncertainty_BA <- as.data.frame(uncertainty_BA)
View(uncertainty_BA)

recharge_spatial_avg <- read.csv("GWR_spatial_avg_2000-2019_copy.csv", header = TRUE)
recharge_spatial_avg <- as.data.frame(recharge_spatial_avg)
View(recharge_spatial_avg)
  
#calculate GWR (GWR = P-ET-R) and replace negative values with 0
# hm$GWR_FLDAS <- with(hm, P_FLDAS - ET_FLDAS - R_FLDAS)
# hm$GWR_FLDAS[hm$GWR_FLDAS<0] <- 0
# hm$GWR_TERRA <- with(hm, P_TERRA - ET_TERRA - R_TERRA)
# hm$GWR_TERRA[hm$GWR_TERRA<0] <- 0

# set strings as factors to false
options(stringsAsFactors = FALSE)

# Create a new data.frame with the newly formatted date field
hm_format <- hm %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
str(hm_format)

#create new column to store original date
hm_format[["Date_original"]] <- hm_format[["Date"]]

#separate date into year, month and day columns
hm_format <- separate( hm_format, 
                       Date,
                       sep="-", 
                       into = c("year", "month", "day"))
colnames(hm_format)
View(hm_format)

#create dry and wet season datasets by filtering
hm_dry = filter(hm_format, month >= ("04") & month <("10"))
View(hm_dry)

hm_wet = filter(hm_format, month < ("04") | month >=("10"))
View(hm_wet)

######################### ANALYSIS ##########################

sink("stat_desc_hydrometeo.txt") #export to txt file
stat.desc(data_num) #descriptive statistics
sink()

sink("stat_desc_hm_dry.txt") #export to txt file
stat.desc(hm_dry) #descriptive statistics
sink()

sink("stat_desc_hm_wet.txt") #export to txt file
stat.desc(hm_wet) #descriptive statistics
sink()

sink("stat_desc_recharge_spatial_avg.txt") #export to txt file
stat.desc(recharge_spatial_avg) #descriptive statistics
sink()

stat.desc(hm_format)

#####calculate RMSE, R² and MAE ##############
observed_P_SENAMHI <- data_num$P_SENAMHI
predicted_P_FLDAS <- data_num$P_FLDAS
postResample(observed_P_SENAMHI,predicted_P_FLDAS)

predicted_P_TERRA <- data_num$P_TERRA
postResample(observed_P_SENAMHI,predicted_P_TERRA)

observed_ET_SENAMHI <- data_num$ET_SENAMHI
predicted_ET_FLDAS <- data_num$ET_FLDAS
postResample(observed_ET_SENAMHI,predicted_ET_FLDAS)

predicted_ET_TERRA <- data_num$ET_TERRA
postResample(observed_ET_SENAMHI,predicted_ET_TERRA)


##### create plots with confidence interval########
# calculate and store r2 in a lable###
mod1 = lm(P_FLDAS~P_SENAMHI, data = data_num)
modsum1 = summary(mod1)
r2_FLDAS = modsum1$adj.r.squared
r2_FLDAS
mylabel1 = bquote(italic(r)^2 == .(format(r2_FLDAS, digits = 3)))

mod2 = lm(P_TERRA~P_SENAMHI, data = data_num)
modsum2 = summary(mod2)
r2_TERRA = modsum2$adj.r.squared
r2_TERRA
mylabel2 = bquote(italic(r)^2 == .(format(r2_TERRA, digits = 3)))

mod3 = lm(ET_FLDAS~ET_SENAMHI, data = data_num)
modsum3 = summary(mod3)
r2_ET_FLDAS = modsum3$adj.r.squared
r2_ET_FLDAS
mylabel3 = bquote(italic(r)^2 == .(format(r2_ET_FLDAS, digits = 3)))

mod4 = lm(ET_TERRA~ET_SENAMHI, data = data_num)
modsum4 = summary(mod4)
r2_ET_TERRA = modsum4$adj.r.squared
r2_ET_TERRA
mylabel4 = bquote(italic(r)^2 == .(format(r2_ET_TERRA, digits = 3)))

#create plots###
ggplot(data_num, aes(x=P_SENAMHI, y=P_TERRA, background = FALSE)) +
  geom_point(color = "#ED7D31") + # orange 
  geom_smooth(method=lm , color="red", fill="gray", se=TRUE, show.legend	=TRUE) +
  xlab ("precipitation SENAMHI (mm)") +
  ylab ("precipitation TERRA (mm)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 550)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 370)) +
  annotate("text", x= 440, y=25, label= mylabel2) 


ggplot(data_num, aes(x=P_SENAMHI, y=P_FLDAS,)) +
  geom_point(color = "#4472C4") + # blue 
  geom_smooth(method=lm , color="red", fill="gray", se=TRUE, show.legend	=TRUE) +
  xlab ("precipitation SENAMHI (mm)") +
  ylab ("precipitation FLDAS (mm)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 550)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 370)) +
  annotate("text", x= 440, y=25, label= mylabel1) 
  
ggplot(data_num, aes(x=ET_SENAMHI, y=ET_TERRA, background = FALSE)) +
  geom_point(color = "#ED7D31") + # orange 
  geom_smooth(method=lm , color="red", fill="gray", se=TRUE, show.legend	=TRUE) +
  xlab ("evapotranspiration SENAMHI (mm)") +
  ylab ("evapotranspiration TERRA (mm)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(50, 210)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  annotate("text", x= 175, y=10, label= mylabel4) 


ggplot(data_num, aes(x=ET_SENAMHI, y=ET_FLDAS,)) +
  geom_point(color = "#4472C4") + # blue 
  geom_smooth(method=lm , color="red", fill="gray", se=TRUE, show.legend	=TRUE) +
  xlab ("evapotranspiration SENAMHI (mm)") +
  ylab ("evapotranspiration FLDAS (mm)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(50, 210)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  annotate("text", x= 175, y=10, label= mylabel3) 


########### UNCERTAINTY GWR VIA ERROR PROPAGATION ######

#log Ugwr = sqrt((Up)²+(Uet)²+(Ur)²)
#Up = annual standard deviation from log of monthly precipitation series
#Uet = annual standard deviation from monthly log of evapotranspiration series
#Ur = annual standard deviation from monthly log of runoff series

sd_P_FLDAS <- sd(log(data_num$P_FLDAS)) 
sd_ET_FLDAS <- sd(log(data_num$ET_FLDAS))
data_num$R_FLDAS[data_num$R_FLDAS == 0] <- 0.00001 #zero gives infinite log and the resulting sd is NaN, so replace 0 by 0.00001
sd_R_FLDAS <- sd(log(data_num$R_FLDAS))
logU_GWR_FLDAS <- sqrt((sd_P_FLDAS^2)+(sd_ET_FLDAS^2)+(sd_R_FLDAS^2))
U_GWR_FLDAS <- exp(logU_GWR_FLDAS) 
U_GWR_FLDAS

data_num$P_TERRA[data_num$P_TERRA == 0] <- 0.00001 #zero gives infinite log and the resulting sd is NaN, so replace 0 by 0.00001
sd_P_TERRA <- sd(log(data_num$P_TERRA)) 
sd_ET_TERRA <- sd(log(data_num$ET_TERRA))
data_num$R_TERRA[data_num$R_TERRA == 0] <- 0.00001 #zero gives infinite log and the resulting sd is NaN, so replace 0 by 0.00001
sd_R_TERRA <- sd(log(data_num$R_TERRA))
logU_GWR_TERRA <- sqrt((sd_P_TERRA^2)+(sd_ET_TERRA^2)+(sd_R_TERRA^2))
U_GWR_TERRA <- exp(logU_GWR_TERRA) 
U_GWR_TERRA

#####UNCERTAINTY BURNED AREA####
# Frequency histogram 
h <- hist(uncertainty_BA$uncertainty.BA,
     col = "gray",  
     main = "Histogram uncertainty BA",
     xlab = "uncertainty (days)",
     ylab = "frequency", 
     cex.main=1, 
     cex.lab=0.75, 
     cex.axis=0.75)

summary(uncertainty_BA$uncertainty.BA)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L



