#Author: Luiza Freitas 
#Objective: analyze data from master thesis
#Date: 07/04/2021

################################################################################
############           DATA ANALYSIS           #################################
################################################################################


################################## INSTALL PACKAGES ################################

# Install these packages only once
install.packages("corrplot")
install.packages("Hmisc")
install.packages("PerformanceAnalytics")
install.packages("nortest")
install.packages("xlsx")
install.packages("compositions")

# Execute libraries every time the script is open
require(foreign)

library("corrplot")
library("Hmisc")
library("PerformanceAnalytics")
library("nortest")
library("xlsx")
library("pastecs")
library("ggbiplot")
library("compositions")

####################### SET WORK DIRECTORY ##############################
 
setwd("C:/Data")

############################ IMPORT DATA ###########################

data <- read.csv("data_Recharge_Fire_LandUse_copy.csv")
View(data)

##################### ORGANIZE DATA ####################

my_df <- as.data.frame(data)
df <- my_df[,-1] #make first column the index
rownames(df) <- my_df[,1] #make row name the index
View(df)
df_no2008 <- df[-9,] #[-9,] deletes 2008
View(df_no2008)
logRatioVar = df_no2008[,3:5]
View(logRatioVar)

######################### DESCRIPTIVE ANALYSIS ##########################

# Show summary statistics for each variable 
sink("stat_desc_df.txt") #export to txt file
stat.desc(df) #descriptive statistics
sink()

sink("stat_desc_df_no2008.txt") #export to txt file
stat.desc(df_no2008) #descriptive statistics
sink()

# Frequency histogram 
hist(df$GWR.FLDAS,
     col = "#4472C4",  # blue
     main = "Histogram GWR FLDAS",
     xlab = "recharge (mm)",
     ylab = "frequency")

hist(df$GWR.TERRA,
     col = "#ED7D31",  # orange
     main = "Histogram GWR TERRA",
     xlab = "recharge (mm)",
     ylab = "frequency")


# Draw normal distribution curve of a variable
x = df$GWR.FLDAS
mean = mean(df$GWR.FLDAS)
sd = sd(df$GWR.FLDAS)
hist(df$GWR.FLDAS,
     col = "#4472C4",  # blue
     main = "Histogram GWR FLDAS",
     xlab = "recharge (mm)",
     ylab = "density",
     freq = FALSE)
curve(dnorm(x, mean, sd), col = 2, add = TRUE)

x = df$GWR.TERRA
mean = mean(df$GWR.TERRA)
sd = sd(df$GWR.TERRA)
hist(df$GWR.TERRA,
     col = "#ED7D31",  # orange
     main = "Histogram GWR TERRA",
     xlab = "recharge (mm)",
     ylab = "density",
     freq = FALSE)
curve(dnorm(x, mean, sd), col = 2, add = TRUE)

############################# NORMALITY TESTS #############################

# Hypothesis:
# H0 : the data is normally distributed
# H1 : the data is NOT normally distributed

# With: p = significance level, if p-value >= p, H0 is not rejected (i.e. the data can be considered normally distributed)

# Execute Anderson-Darling normality test
ad.test(x) 

# Execute Kolmogorov-Smirnov normality test
ks.test(x, "pnorm", mean, sd)


# Execute Qui-square Pearson normality test
pearson.test(x)


################ DATA TRANSFORMATION (log10, sqrt, 1/x etc) ##################

# Add new column with log of the chosen variable
df_no2008$logGWR.FLDAS<- c(log(df_no2008$GWR.FLDAS+0.0001))
df_no2008$logGWR.TERRA<- c(log(df_no2008$GWR.TERRA+0.0001))

df_no2008$logP_FLDAS<- c(log(df_no2008$P_FLDAS+0.0001))
df_no2008$logP_TERRA<- c(log(df_no2008$P_TERRA+0.0001))

df_no2008$logET_FLDAS<- c(log(df_no2008$ET_FLDAS+0.0001))
df_no2008$logET_TERRA<- c(log(df_no2008$ET_TERRA+0.0001))

df_no2008$logR_FLDAS<- c(log(df_no2008$R_FLDAS+0.0001))
df_no2008$logR_TERRA<- c(log(df_no2008$R_TERRA+0.0001))

View(df_no2008)

#Add new column with centered log ratio of burned area to natural vegetation area
clr <- clr(logRatioVar)
clr
df_no2008$CLR_BA <- c(clr$BA)
df_no2008$CLR_VA <- c(clr$VA)
df_no2008$CLR_notVA <- c(clr$notVA)
View(df_no2008)

################################################################################
############          CORRELATION ANALYSIS SCRIPT          ################
################################################################################


######################## CORRELATION BETWEEN 2 VARIABLES #######################

#Execute correlation with 2 variables and prepare to add r as text to the plot
########### FLDAS ##############
# recharge and burned area #####
cor_GWR_BA_FLDAS_pearson <- cor(df_no2008$logGWR.FLDAS, df_no2008$CLR_BA, method = "pearson") 
cor_GWR_BA_FLDAS_pearson
cor_GWR_BA_FLDAS_spearman <- cor(df_no2008$logGWR.FLDAS, df_no2008$CLR_BA, method = "spearman")
cor_GWR_BA_FLDAS_spearman
mylabel1 = bquote(italic(rho) == .(format(cor_GWR_BA_FLDAS_spearman, digits = 3)))

# recharge and natural vegetation ####
cor_GWR_VA_FLDAS_pearson <- cor(df_no2008$logGWR.FLDAS, df_no2008$CLR_VA, method = "pearson") 
cor_GWR_VA_FLDAS_pearson
cor_GWR_VA_FLDAS_spearman <- cor(df_no2008$logGWR.FLDAS, df_no2008$CLR_VA, method = "spearman") 
cor_GWR_VA_FLDAS_spearman
mylabel2 = bquote(italic(rho) == .(format(cor_GWR_VA_FLDAS_spearman, digits = 3)))

########### TERRA ################
# recharge and burned area #####
cor_GWR_BA_TERRA_pearson <- cor(df_no2008$logGWR.TERRA, df_no2008$CLR_BA, method = "pearson") 
cor_GWR_BA_TERRA_pearson
cor_GWR_BA_TERRA_spearman <- cor(df_no2008$logGWR.TERRA, df_no2008$CLR_BA, method = "spearman")
cor_GWR_BA_TERRA_spearman
mylabel3 = bquote(italic(rho) == .(format(cor_GWR_BA_TERRA_spearman, digits = 3)))

# recharge and natural vegetation ####
cor_GWR_VA_TERRA_pearson <- cor(df_no2008$logGWR.TERRA, df_no2008$CLR_VA, method = "pearson") 
cor_GWR_VA_TERRA_pearson
cor_GWR_VA_TERRA_spearman <- cor(df_no2008$logGWR.TERRA, df_no2008$CLR_VA, method = "spearman") 
cor_GWR_VA_TERRA_spearman
mylabel4 = bquote(italic(rho) == .(format(cor_GWR_VA_TERRA_spearman, digits = 3)))

###########Execute significance test#############

# Hypothesis:
# H0 : There is no correlation
# H1 : There is correlation

# With: 0,05 = significance level, if p-value >>>>, H0 is not rejected (i.e. there is no correlation)

########### FLDAS ##############
# recharge and burned area #####
cor.test(df_no2008$logGWR.FLDAS, df_no2008$CLR_BA, method = "pearson")
cor.test(df_no2008$logGWR.FLDAS, df_no2008$CLR_BA, method = "spearman")

# recharge and natural vegetation ####
cor.test(df_no2008$logGWR.FLDAS, df_no2008$CLR_VA, method = "pearson")
cor.test(df_no2008$logGWR.FLDAS, df_no2008$CLR_VA, method = "spearman")

########### TERRA ################
# recharge and burned area #####
cor.test(df_no2008$logGWR.TERRA, df_no2008$CLR_BA, method = "pearson")
cor.test(df_no2008$logGWR.TERRA, df_no2008$CLR_BA, method = "spearman")

# recharge and natural vegetation ####
cor.test(df_no2008$logGWR.TERRA, df_no2008$CLR_VA, method = "pearson")
cor.test(df_no2008$logGWR.TERRA, df_no2008$CLR_VA, method = "spearman")


###################### CORRELATION WITH ALL VARIABLES #######################

#Execute correlation analysis

cor <- (round(cor(df_no2008,  method = "spearman"), 10)) #Not normal and linear data
View(cor)

################# CORRELATION MATRIX WITH P-values #####################

corp <- rcorr(as.matrix(df_no2008), type = "spearman") 
View(corp[["P"]])

#################### PLOT RELATIONSHIP BETWEEN 2 VARIABLES #######################

# scatter plot recharge vs BA ########
   ###FLDAS###
plot(df_no2008$BA, df_no2008$GWR.FLDAS,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "#4472C4",  # blue
     main = "Recharge FLDAS vs burned area",
     xlab = "burned area(%)",
     ylab = "recharge (mm)")
text(x = 0.055, y = 45, labels = mylabel1)
abline(lm(df_no2008$GWR.FLDAS ~ df_no2008$BA), col = "blue") #regression line

plot(df_no2008$CLR_BA, df_no2008$logGWR.FLDAS,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "#4472C4",  # blue
     main = "Recharge FLDAS vs burned area",
     xlab = "CLR burned area",
     ylab = "ln recharge (mm)")
text(x = -0.5, y = 4, labels = mylabel1)
abline(lm(df_no2008$logGWR.FLDAS ~ df_no2008$CLR_BA), col = "blue") #regression line


   ###TERRA###
plot(df_no2008$BA, df_no2008$GWR.TERRA,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "#ED7D31",  # orange
     main = "Recharge TERRA vs burned area",
     xlab = "burned area(%)",
     ylab = "recharge (mm)")
text(x = 0.055, y = 20, labels = mylabel3)
abline(lm(df_no2008$GWR.TERRA ~ df_no2008$BA), col = "blue")

plot(df_no2008$CLR_BA, df_no2008$logGWR.TERRA,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "#ED7D31",  # orange
     main = "Recharge TERRA vs burned area",
     xlab = "CLR burned area",
     ylab = "ln recharge (mm)")
text(x = -0.5, y = 2.5, labels = mylabel3)
abline(lm(df_no2008$logGWR.TERRA ~ df_no2008$CLR_BA), col = "blue")

# scatter plot recharge vs VA #######
   ###FLDAS###
plot(df$VA, df$GWR.FLDAS,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "#4472C4",  # blue
     main = "Recharge FLDAS vs natural vegetation area",
     xlab = "nat. vegetation area(%)",
     ylab = "recharge (mm)")
text(x = 0.86, y = 50, labels = mylabel2)
abline(lm(df$GWR.FLDAS ~ df$VA), col = "blue")

plot(df$CLR_VA, df$logGWR.FLDAS,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "#4472C4",  # blue
     main = "Recharge FLDAS vs natural vegetation area",
     xlab = "CLR nat. vegetation area",
     ylab = "ln recharge (mm)")
text(x = 0.86, y = 50, labels = mylabel2)
abline(lm(df$logGWR.FLDAS ~ df$CLR_VA), col = "blue")


   ###TERRA###
plot(df$VA, df$GWR.TERRA,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "#ED7D31",  # orange
     main = "Recharge TERRA vs natural vegetation area",
     xlab = "nat. vegetation area(%)",
     ylab = "recharge (mm)")
text(x = 0.86, y = 20, labels = mylabel4)
abline(lm(df$GWR.TERRA ~ df$VA), col = "blue")

plot(df$CLR_VA, df$logGWR.TERRA,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "#ED7D31",  # orange
     main = "Recharge TERRA vs natural vegetation area",
     xlab = "nat. vegetation area(%)",
     ylab = "recharge (mm)")
text(x = 0.86, y = 20, labels = mylabel4)
abline(lm(df$logGWR.TERRA ~ df$CLR_VA), col = "blue")

#scatter plot VA vs BA #####
plot(df_no2008$VA, df_no2008$BA,
     pch = 19,         # Solid circle
     cex = 1.2,        # Make 120% size
     col = "red",  
     main = "burned area vs \n natural vegetation area",
     xlab = "nat. vegetation area(%)",
     ylab = "burned area (%)")
abline(lm(df_no2008$BA ~ df_no2008$VA), col = "blue")


#################### PLOT RELATIONSHIP BETWEEN ALL VARIABLES #######################

#Option 1######
#FLDAS
pairs(df_no2008[, c(1,3,4)],
      pch = 19,         # Solid circle
      col = "#4472C4",  # blue
      main = "FLDAS") 

#TERRA
pairs(df_no2008[, c(2,3,4)],
      pch = 19,         # Solid circle
      col = "#ED7D31",  # orange
      main = "TERRA")

#Option 2######
df_plot <- df_no2008[, c(1,3,4)] #FLDAS
chart.Correlation(df_plot, 
                  histogram=TRUE, 
                  pch=19) 

df_plot <- df_no2008[, c(2,3,4)] #TERRA
chart.Correlation(df_plot, 
                  histogram=TRUE, 
                  pch=19)

######################### EXPORT CORRELATION MATRIX  ########################

sink("correlation_with_hm.xls")
cor
sink()

sink("correlation_p-value_with_hm.xls")
corp[["P"]]
sink()

################################# CORRELOGRAM ##################################

corrplot(cor(df_no2008[, c(1, 3, 4, 6, 8, 10, 12, 13, 15, 17, 19, 21)]),
         method = "number",
         title = "Correlation matrix FLDAS",
         type = "lower",
         diag = FALSE,
         addCoefasPercent = TRUE,
         tl.cex = 0.7)

corrplot(cor(df_no2008[, c(3,4,6)]),
         method = "number",
         title = "Correlation matrix TERRA",
         type = "upper",
         diag = FALSE,
         addCoefasPercent = TRUE,
         tl.cex = 0.7)

##################### PERFORM PCA ###########################################

pca_TERRA <- prcomp(~#logGWR.TERRA 
                    + logP_TERRA 
                    + logET_TERRA
                    + logR_TERRA
                    + CLR_BA 
                    + CLR_VA,
                    data = df_no2008, 
                    center = TRUE, 
                    scale. = TRUE)
pca_TERRA

pca_FLDAS <- prcomp(~#logGWR.FLDAS 
                    + logP_FLDAS 
                    + logET_FLDAS
                    + logR_FLDAS
                    + CLR_BA 
                    + CLR_VA,
                    data = df_no2008, 
                    center = TRUE, 
                    scale. = TRUE)
pca_FLDAS

#####Export PCA data######
sink("PCA_FLDAS_noGWR.xls")
pca_FLDAS
summary(pca_FLDAS)
sink()

sink("PCA_TERRA_noGWR.xls")
pca_TERRA
summary(pca_TERRA)
sink()

#####build biplot######
ggbiplot(pca_FLDAS, 
         labels=rownames(df_no2008), 
         labels.size = 2.5, 
         varname.size = 2.5,
         varname.adjust = 1.3, 
         varname.abbrev = TRUE) + 
   ggtitle("PCA FLDAS") +
   theme_classic()

ggbiplot(pca_TERRA, labels=rownames(df_no2008), 
         labels.size = 2.5, 
         varname.size = 2.5,
         varname.adjust = 1.3, 
         varname.abbrev = TRUE) + 
   ggtitle("PCA TERRA") +
   theme_classic() 


################################################################################
################ SCRIPT FOR REGRESSION ANALYSIS ###########################
################################################################################


#################### LINEAR MULTIPLE REGRESSION  ########################
#regression dropping the intercept (baseline recharge if there was no P or VA)
reg_FLDAS <- lm(log(GWR.FLDAS) #log makes the model better (after trial and error)
                ~ -1 #-1 serves to drop the intercept
                + CLR_VA
                + logP_FLDAS,
                data = df_no2008)
reg_FLDAS

reg_TERRA <- lm(logGWR.TERRA 
                ~ -1 #-1 serves to drop the intercept
                + CLR_VA 
                + logP_TERRA,
                data = df_no2008)
reg_TERRA

#Export regression summary
sink("reg_FLDAS_VA_P.xls")
summary(reg_FLDAS)
sink()

sink("reg_TERRA_VA_P.xls")
summary(reg_TERRA)
sink()


############################### CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

