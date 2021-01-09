# script to calculate correlation coefficients between variables in the model and to get a first look at 
# what data transformations are necessary

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("Hmisc")
library(Hmisc)
library(tidyverse)


#############  create correlation matrices for daily fDOM ############################

data_daily <- read.csv("./Data/variables_all_pluslag_fDOM_daily_final_RAINLAGS.csv") #will need to change if transformed in 14 are used. Currently justt going to use z transfomred values
head(data_daily)
# get rid of desciptor columns (depth, week identifiers, etc.), so only possible drivers are left
data_daily_ZT <- data_daily %>%  
  select(daily_EXO_fdom, fdom_daily_ARlag1, daily_EXO_chla_ugL_ZT, daily_EXO_do_mgL_ZT,
                         daily_EXO_do_persat_ZT, daily_EXO_wtr_temp_ZT, daily_SRup_mean_ZT, daily_SRup_max_ZT,
                         daily_rain_mm_ZT, daily_rain_mm_lag1_ZT, daily_rain_mm_lag2_ZT, WVWA_Flow_cms_daily_mean_ZT,
         WRT_days_daily_ZT) #just z transformed variables and fDOM 

data_daily_norm <- data_daily %>%  
  select(daily_EXO_fdom, fdom_daily_ARlag1, daily_EXO_chla_ugL, daily_EXO_do_mgL,
         daily_EXO_do_persat, daily_EXO_wtr_temp, daily_SRup_mean, daily_SRup_max,
         daily_rain_mm, WVWA_Flow_cms_daily_mean,
         WRT_days_daily) #remove non z transformed variables and date 

# not using chart.Correlation because there are too many variables to assess in a table like this
x <- chart.Correlation(data_daily_ZT, method = "spearman", histogram = TRUE)

cor <- rcorr(as.matrix(data_daily_ZT), type = "spearman")
spear <- cor$r
write.csv(spear, "./Data/correlation_matrix_fdom_daily_final_RAINLAGS.csv")




#############  create correlation matrices for monthly fDOM ############################

data_monthly <- read.csv("./Data/variables_all_pluslag_fDOM_monthly_final.csv") 
head(data_monthly)
# get rid of desciptor columns (depth, week identifiers, etc.), so only possible drivers are left
data_monthly <- data_monthly %>%  
  select(monthly_EXOfdom, fdom_monthly_ARlag1, monthly_EXOchla_ugL_ZT, monthly_EXOdo_mgL_ZT, monthly_EXOdo_persat_ZT,
         monthly_EXOwtr_temp_ZT, monthly_rain_mm_ZT, monthly_SRup_mean_ZT, monthly_SRup_max_ZT,
         WVWA_Flow_cms_monthly_mean_ZT, WRT_days_monthly_ZT) #remove non z transformed variables and date 


# not using chart.Correlation because there are too many variables to assess in a table like this
x <- chart.Correlation(data_monthly, method = "spearman", histogram = TRUE)

cor <- rcorr(as.matrix(data_monthly), type = "spearman")
spear <- cor$r
write.csv(spear, "./Data/correlation_matrix_fdom_monthly_final.csv")




##############################################################################################################################################
# eliminate variables that are correlated through 1) visually seeing which has a stronger relationship with fDOM, 
# if no clear relationship:
# 2) choosing the variable with the higher spearman's r value with chl
# if no clear difference between the r values:
# 3) choosing the variable that would have more of a biological importance on fDOM 
# take a look at some of the variables that are correlated vs. fdom
#this can be done in this script or just looking through created csv files 

#####################################################################################################################################################
############# Relation plots##############################
plot(data$daily_EXOwtr_temp, data$fdom_log)
plot(data$mean_flow_sqrt , data$fdom_log)

plot(data$daily_EXOdo_persat, data$fdom_log)
plot(data$daily_EXOdo_mgL, data$fdom_log)
plot(data$daily_SRup_mean, data$fdom_log)

plot(data_daily$daily_EXO_wtr_temp_ZT, data_daily$daily_EXO_fdom)
z <- lm(data_daily$daily_EXO_fdom ~ data_daily$daily_EXO_wtr_temp_ZT)
summary(z)
abline(z)

plot(data_daily$daily_SRup_mean_ZT, data_daily$daily_EXO_fdom)
y <- lm(data_daily$daily_EXO_fdom ~ data_daily$daily_SRup_mean_ZT)
summary(y)
abline(y)


# create a function for ease in comparing variables
compare <- function(variab){
  x <- lm(fdom_log ~ variab)
  plot(variab, fdom)
  abline(x)
  summary(x)
}

# looking at predictable variables only
compare(daily_EXOdo_mgL)
compare(daily_SRup_mean)
compare(EXOchla_log)
compare(daily_EXOdo_persat)





