# examine data within astsa package to determine what time lag is appropriate for an auto-regressive time series model
# add in a new column in the dataset with the AR-lag response variable (fDOM t-?)

#install.packages("astsa")
library(astsa) 
pacman::p_load(tidyverse, cowplot, lubridate, dplyr, dplR, zoo)
library(naniar)
# install.packages("DescTools")
library(DescTools) #lets you plot the Timeseries, ACF and PACF in one window using PlotACF function 


### Geting lag for daily fDOM 

daily_fDOM <- read_csv("./Data/ar_daily_data_joined_RAINLAGS.csv")
head(daily_fDOM)
tail(daily_fDOM)

##for describing seasaonal paterns in manuscript 
{
mon <- read_csv("./Final_Data_Scripts/Data/ar_monthly_data_joined.csv")
mon <- mon %>% 
  filter(Date > "2018-09-01",
         Date < "2019-10-01")
median(daily_fDOM$WRT_days_daily, na.rm = TRUE)
mean(daily_fDOM$WRT_days_daily, na.rm = TRUE)
sd(daily_fDOM$WRT_days_daily, na.rm = TRUE)
min(daily_fDOM$WRT_days_daily, na.rm = TRUE)
median(mon$WRT_days_monthly, na.rm = TRUE)
max(daily_fDOM$WRT_days_daily, na.rm = TRUE)
sd(mon$WRT_days_monthly, na.rm = TRUE)

min(daily_fDOM$daily_EXO_do_persat, na.rm = TRUE)
max(daily_fDOM$daily_EXO_do_persat, na.rm = TRUE)
do_check <- daily_fDOM %>% 
  filter(daily_EXO_do_persat > 150)


max(daily_fDOM$WVWA_Flow_cms_daily_mean, na.rm = TRUE)
temp_check <- daily_fDOM %>% 
  filter(daily_EXO_wtr_temp > 28)
}
##end
              
    
 ##USE THIS for finding lags needed for daily fDOM 
 plot(daily_fDOM$daily_EXO_fdom, type="b") #time series plot of x with points marked as “o”
 lag1.plot(daily_fDOM$daily_EXO_fdom, 10) # Plots x versus lag 1 of x.
 PlotACF(daily_fDOM$daily_EXO_fdom)
 acf2(daily_fDOM$daily_EXO_fdom, xlim=c(1,20), na.action = na.pass) # Plots the ACF of x for lags 1 to 19
 pacf(daily_fDOM$daily_EXO_fdom, xlim = c(1,20), na.action = na.pass)
 xlag1=lag(daily_fDOM$daily_EXO_fdom, 1) # Creates a lag 1 of x variable
 y=cbind(daily_fDOM$daily_EXO_fdom,xlag1) # binds fDOM column w/ fDOM lag 
 ar1fit=lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
 summary(ar1fit) # This lists the regression results
 plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
 acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18


 # add AR lag of 1 timestep to dataframe (object: y)  for FDOM
 colnames(y) <- c("fdom_QSU", "fdom_daily_ARlag1")
 data_surf <- cbind(daily_fDOM, y)
 data_surf <- data_surf %>% 
   select(-fdom_QSU) 
 data_surf <- data_surf %>% 
   select(Date, daily_EXO_fdom, fdom_daily_ARlag1, everything())
 head(data_surf)
 
write.csv(data_surf, row.names = FALSE, "./Data/variables_all_pluslag_fDOM_daily_final_RAINLAGS.csv")
 



### Geting lag for monthly fDOM 

monthly_fDOM <- read_csv("./Data/ar_monthly_data_joined.csv")
head(monthly_fDOM)
tail(monthly_fDOM)

monthly_fDOM <- monthly_fDOM %>% 
  select(-yr, -mon, -yr.y, -mon.y, -yr.x, -mon.x) #removing the year and month columns that were used for grouping in compilation scripts
head(monthly_fDOM)



##USE THIS for finding lags needed for daily fDOM 
plot(monthly_fDOM$monthly_EXOfdom, type="b") #time series plot of x with points marked as “o”
PlotACF(monthly_fDOM$monthly_EXOfdom)
lag1.plot(monthly_fDOM$monthly_EXOfdom, 10) # Plots x versus lag 1 of x.
acf2(monthly_fDOM$monthly_EXOfdom, xlim=c(1,20), na.action = na.pass) # Plots the ACF of x for lags 1 to 19
pacf(daily_fDOM$daily_EXO_fdom, xlim = c(1,20), na.action = na.pass, main = "Monthly fDOM PACF")
xlag1=lag(monthly_fDOM$monthly_EXOfdom, 1) # Creates a lag 1 of x variable. 
y=cbind(monthly_fDOM$monthly_EXOfdom,xlag1) 
ar1fit=lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
summary(ar1fit) # This lists the regression results
plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,10)) # ACF of the residuals for lags 1 to 10


# add AR lag of 1 timestep to dataframe (object: y)  for FDOM
colnames(y) <- c("fdom_QSU", "fdom_monthly_ARlag1")
data_surf <- cbind(monthly_fDOM, y)
data_surf <- data_surf %>% 
  select(-fdom_QSU) 
data_surf <- data_surf %>% 
  select(Date, monthly_EXOfdom, fdom_monthly_ARlag1, everything())
head(data_surf)

write.csv(data_surf, row.names = FALSE, "./Data/variables_all_pluslag_fDOM_monthly_final.csv")




##plot of PACFs
library(patchwork)

daily_pacf <- pacf(daily_fDOM$daily_EXO_fdom, xlim = c(1,20), na.action = na.pass, main = "Daily fDOM PACF")
monthly_pacf <- pacf(monthly_fDOM$monthly_EXOfdom, xlim = c(1,20), na.action = na.pass, main = "Monthly fDOM PACF")


#manually save image in export tab using 661 x 484 or  ratio save as monthly_PACF.tiff
par(mfrow=c(1,2))
plot(daily_pacf,  las = 1, main = "Daily fDOM PACF",xlab = "Lag (days)") #las rotates the y axis numbers 
title("a)", adj = 0)

plot(monthly_pacf,  las = 1, main = "Monthly fDOM PACF",xlab = "Lag (months)") 
title("b)", adj = 0)









