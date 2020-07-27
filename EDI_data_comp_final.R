# Joining EXO, Met, and Inflow data from EDI for AR and Wavelet analyses 
# Dexter Howard
# 26june2020

#load packages 
pacman::p_load(tidyverse, lubridate, xts, cowplot)

#read in csv's downloaded from EDI 
getwd()
exo <- read_csv("./Data/Data_EDI/catdata_edited_withDO.csv", col_types = cols(.default = "d", 
                                                                                   Reservoir = "c",
                                                                                   DateTime = "T"))  #defining column type to prevent logical errors

inflow <- read_csv("./Data/Data_EDI/inflow_for_EDI_2013_06Mar2020.csv", col_types = cols(.default = "d", 
                                                                                              Reservoir = "c",
                                                                                              DateTime = "T")) #defining column type to prevent logical errors

#reading in met from dropbox since GitHub doesn't accept files this large 
met <- read_csv("C:/Users/dwh18/Dropbox/Thesis/Final_Data_Scripts/Data/edi_met/Met_final_2015_2019.csv", col_types = cols(.default = "d", 
                                                                                 Reservoir = "c",
                                                                                 DateTime = "T",
                                                                                 Note_PAR_Average_umol_s_m2 = "c", Note_PAR_Total_mmol_m2 = "c", Note_BP_Average_kPa = "c", Note_AirTemp_Average_C = "c",
                                                                                 Note_RH_percent = "c", Note_Rain_Total_mm = "c", Note_WindSpeed_Average_m_s = "c", Note_WindDir_degrees = "c",
                                                                                 Note_ShortwaveRadiationUp_Average_W_m2 = "c", Note_ShortwaveRadiationDown_Average_W_m2 = "c", 
                                                                                 Note_InfaredRadiationUp_Average_W_m2 = "c", Note_InfaredRadiationDown_Average_W_m2 = "c", Note_Albedo_Average_W_m2 = "c")) #defining column type to prevent logical errors 




#### Selecting EXO data for AR and formating for wavelet ####
head(exo)

exo_filt <- slice(exo, -c(40656:40660)) #removing duplicated hour of data from 15 April 2019

exo_filt <- exo_filt %>% 
  select(DateTime, EXOTemp_C_1, EXODOsat_percent_1, EXODO_mgL_1, EXOChla_ugL_1, EXOfDOM_QSU_1,
         EXO_depth) %>% # could have RFU for chla and fdom 
  filter(DateTime >= as.POSIXct("2018-08-06 17:30:00", tz = "UTC")) #first data with exo is 2018-08-06 17:30:00, need to define tz as UTC otherwise will select time four hours eariler becasue of EST timezone difference 

head(exo_filt)


#plot(exo_filt$DateTime, exo_filt$EXOfDOM_QSU_1)
hist(exo_filt$EXOfDOM_QSU_1)
mean(exo_filt$EXOfDOM_QSU_1, na.rm = TRUE)
median(exo_filt$EXOfDOM_QSU_1, na.rm = TRUE)

sd <- sd(exo_filt$EXOfDOM_QSU_1, na.rm = TRUE)


#running QAQC and removing values greater than 2 S.D. from previous and subsequent values
QAQC <- exo_filt %>% 
  mutate(current = lag(EXOfDOM_QSU_1, 0),
         before = lag(EXOfDOM_QSU_1, 1),
         after = lead(EXOfDOM_QSU_1, 1)) %>% 
  mutate(EXO_fdom_qsu = ifelse(
    ( abs(before - current) > (2*sd)   )  & ( abs(after - current) > (2*sd)   ), NA, EXOfDOM_QSU_1
  )) %>% 
  select(-current, -before, -after, -EXOfDOM_QSU_1)
head(QAQC)

sd(QAQC$EXO_fdom_qsu, na.rm = TRUE)


#selecting desired study timeframe 
exo_filta <- QAQC %>% 
  filter(DateTime >= as.POSIXct("2018-10-02 00:00:00", tz = "UTC"),
         DateTime < as.POSIXct("2019-10-02 00:00:00", tz = "UTC"))

head(exo_filta)
tail(exo_filta)

#determining number of NAs in dataset 
exo_na <- exo_filta %>%
  filter(is.na(EXO_fdom_qsu))
head(exo_na)
# 337 of 52544 data points were NAs. == 0.6%


#plot(exo_filta$DateTime, exo_filta$EXO_fdom_qsu)
mean(exo_filta$EXO_fdom_qsu, na.rm = TRUE)
median(exo_filta$EXO_fdom_qsu, na.rm = TRUE)
sd(exo_filta$EXO_fdom_qsu, na.rm = TRUE)
hist(exo_filta$EXO_fdom_qsu)


### Exo for wavelet ###
#formating Exo sonde data for wavelet analysis and creating csv 

exo_wavelet <- exo_filta %>% 
  select(DateTime, EXO_fdom_qsu, EXOTemp_C_1) %>% 
  mutate(Year = format(as.Date(DateTime), "%Y"),
         Year = as.numeric(Year)) %>% 
  mutate(hour = format(as_datetime(DateTime), "%H"),
         hour = as.numeric(hour)) 
head(exo_wavelet)
tail(exo_wavelet)

exo_wavelet_final <- exo_wavelet %>% 
  mutate(Time = as.numeric(c(1:nrow(exo_wavelet)))) %>% 
  mutate(fdom = as.numeric(EXO_fdom_qsu))
head(exo_wavelet_final)

#write.csv(exo_wavelet_final, "./Data/exo_for_wavelet.csv", row.names = FALSE) #write csv for wavelet 




### Getting daily and monthly exo for AR ###

exo_daily <- QAQC %>% 
  mutate(Date = as.Date(DateTime)) %>% 
  group_by(Date) %>%
  summarize(daily_EXO_fdom = mean(EXO_fdom_qsu, na.rm = TRUE),
            daily_EXO_chla_ugL = mean(EXOChla_ugL_1, na.rm = TRUE),
            daily_EXO_do_mgL = mean(EXODO_mgL_1 , na.rm = TRUE),
            daily_EXO_do_persat = mean(EXODOsat_percent_1, na.rm = TRUE),
            daily_EXO_wtr_temp = mean(EXOTemp_C_1, na.rm = TRUE),
            daily_EXO_depth = mean(EXO_depth, na.rm = TRUE)) %>% 
  filter(Date < "2019-10-02") 

head(exo_daily)
tail(exo_daily)


exo_monthly <- QAQC %>% 
  group_by(yr = year(DateTime), mon = month(DateTime))%>% 
  summarise(monthly_EXOfdom = mean(EXO_fdom_qsu, na.rm = TRUE),
            monthly_EXOchla_ugL = mean(EXOChla_ugL_1, na.rm = TRUE),
            monthly_EXOdo_mgL = mean(EXODO_mgL_1, na.rm = TRUE),
            monthly_EXOdo_persat = mean(EXODOsat_percent_1, na.rm = TRUE),
            monthly_EXOwtr_temp = mean(EXOTemp_C_1, na.rm = TRUE),  
            monthly_depth = mean(EXO_depth, na.rm = TRUE)) %>% 
  mutate(day = sum(0+1)) %>%                                  #creates a day value just to create a date function
  mutate(Date = as.Date( paste(yr, mon, day,sep="-"), "%Y-%m-%d")) %>% 
  filter(Date > "2018-08-01",
         Date < "2019-10-01") %>% 
  mutate(monthly_EXOchla_ugL_ZT = scale(monthly_EXOchla_ugL, center = TRUE, scale = TRUE),
         monthly_EXOdo_mgL_ZT = scale(monthly_EXOdo_mgL, center = TRUE, scale = TRUE),
         monthly_EXOdo_persat_ZT = scale(monthly_EXOdo_persat, center = TRUE, scale = TRUE),
         monthly_EXOwtr_temp_ZT = scale(monthly_EXOwtr_temp, center = TRUE, scale = TRUE))


head(exo_monthly)
tail(exo_monthly)

exo_monthly_final <- exo_monthly %>% 
  select(Date, monthly_EXOfdom, monthly_EXOchla_ugL, monthly_EXOchla_ugL_ZT, monthly_EXOdo_mgL, monthly_EXOdo_mgL_ZT,
         monthly_EXOdo_persat, monthly_EXOdo_persat_ZT,
         monthly_EXOwtr_temp, monthly_EXOwtr_temp_ZT,
         monthly_depth, yr, mon)  #have to keep yr and mon since their currently grouping variables. can filter out during AR analysis after reading in new csv

head(exo_monthly_final)
tail(exo_monthly_final)



#### Selecting met data for AR ####
head(met)

met_filt <- met %>% 
  filter(DateTime > as.POSIXct("2018-08-01 00:00:00", tz = "UTC")) %>% 
  select(DateTime, ShortwaveRadiationUp_Average_W_m2, Rain_Total_mm) %>% 
  mutate(Date = as.Date(DateTime))
head(met_filt)

met_daily <- met_filt %>% 
  group_by(Date) %>%
  summarize(daily_rain_mm = sum(Rain_Total_mm, na.rm = TRUE),
            daily_SRup_mean = mean(ShortwaveRadiationUp_Average_W_m2, na.rm = TRUE),
            daily_SRup_max = max(ShortwaveRadiationUp_Average_W_m2, na.rm = TRUE)) %>% 
  filter(Date < "2019-10-02") 


head(met_daily)
tail(met_daily)

#visualizing timeseries for selected met variables 
plot(met_daily$Date, met_daily$daily_SRup_mean, type = "l")
plot(met_daily$Date, met_daily$daily_rain_mm, type = "l")



met_monthly <- met_filt %>% 
  group_by(yr = year(DateTime), mon = month(DateTime))%>% 
  summarize(monthly_rain_mm = sum(Rain_Total_mm, na.rm = TRUE),
            monthly_SRup_mean = mean(ShortwaveRadiationUp_Average_W_m2, na.rm = TRUE),
            monthly_SRup_max = max(ShortwaveRadiationUp_Average_W_m2, na.rm = TRUE)) %>% 
  mutate(day = sum(0+1)) %>%                                  #creates a day value just to create a date function
  mutate(Date = as.Date( paste(yr, mon, day,sep="-"), "%Y-%m-%d")) %>% 
  filter(Date > "2018-08-01",
         Date < "2019-10-01") %>% 
  mutate(monthly_SRup_mean_ZT = scale(monthly_SRup_mean, center = TRUE, scale = TRUE),
         monthly_SRup_max_ZT = scale(monthly_SRup_max, center = TRUE, scale = TRUE),
         monthly_rain_mm_ZT = scale(monthly_rain_mm, center = TRUE, scale = TRUE))

head(met_monthly)
tail(met_monthly)

met_monthly_final <- met_monthly %>% 
  select(Date, monthly_rain_mm, monthly_rain_mm_ZT, monthly_SRup_mean, monthly_SRup_mean_ZT,
         monthly_SRup_max, monthly_SRup_max_ZT, yr, mon)  #have to keep yr and mon since their curretnly grouping variables. can filter out during AR after reading in new csv

head(met_monthly_final)
tail(met_monthly_final)




#### Selecting inflow data for AR ####

head(inflow) 
tail(inflow)

inf_filt <- inflow %>% 
  select(DateTime, WVWA_Pressure_psia, WVWA_Flow_cms) %>% 
  filter(DateTime > as.POSIXct("2018-08-01 00:00:00", tz = "UTC")) %>%         
  mutate(Date = as.Date(DateTime)) #creates date column for averaging by day later 
head(inf_filt)

inf_daily <- inf_filt %>% 
  group_by(Date) %>% 
  summarise(WVWA_Flow_cms_daily_mean = mean(WVWA_Flow_cms, na.rm = TRUE),
            WVWA_Pressure_psia_daily_mean = mean(WVWA_Pressure_psia, na.rm = TRUE)) %>% 
  mutate(WRT_days_daily = ( (3.1E5/WVWA_Flow_cms_daily_mean) * (1/60) * (1/60) * (1/24) ) ) %>% #calculating WRT 
  filter(Date < "2019-10-02") 

#inf_daily$WVWA_Flow_cms_daily_mean[is.na(inf_daily$WVWA_Flow_cms_daily_mean)] <- 0  #this works for all columns in data frame 

head(inf_daily)
tail(inf_daily)



inf_monthly <- inf_filt %>% 
  group_by(yr = year(DateTime), mon = month(DateTime))%>% 
  summarise(WVWA_Flow_cms_monthly_mean = mean(WVWA_Flow_cms, na.rm = TRUE),
            WVWA_Pressure_psia_monthly_mean = mean(WVWA_Pressure_psia, na.rm = TRUE)) %>% 
  mutate(WRT_days_monthly = ( (3.1E5/WVWA_Flow_cms_monthly_mean) * (1/60) * (1/60) * (1/24) ) ) %>% #calc WRT
  mutate(day = sum(0+1)) %>%                                  #creates a day value just to create a date function
  mutate(Date = as.Date( paste(yr, mon, day,sep="-"), "%Y-%m-%d")) %>% 
  filter(Date > "2018-08-01",
         Date < "2019-10-01") %>% 
  mutate(WVWA_Flow_cms_monthly_mean_ZT = scale(WVWA_Flow_cms_monthly_mean, center = TRUE, scale = TRUE),
         WRT_days_monthly_ZT = scale(WRT_days_monthly, center = TRUE, scale = TRUE))
head(inf_monthly)
tail(inf_monthly)

inf_monthly_final <- inf_monthly %>% 
  select(Date, WVWA_Flow_cms_monthly_mean,WVWA_Flow_cms_monthly_mean_ZT, WVWA_Pressure_psia_monthly_mean,
         WRT_days_monthly,WRT_days_monthly_ZT,
         yr, mon)    #have to keep yr and mon since their curretnly grouping variables. can filter out during AR after reading in new csv

head(inf_monthly_final)
tail(inf_monthly_final)




####   DAILY JOIN     ####
#join daily data and create csv for AR analysis 

join_daily <- full_join(exo_daily, met_daily, by = "Date")

join_daily_a <- full_join(join_daily, inf_daily, by = "Date")

join_daily_final <- join_daily_a[order(ymd(join_daily_a$Date)),]  #just orders by date for some dates that had missing values 

head(join_daily_final)
tail(join_daily_final)

day_fin <- join_daily_final %>% 
  filter(Date >= "2018-10-01",
         Date < "2019-10-02") %>% 
  mutate(daily_EXO_chla_ugL_ZT = scale(daily_EXO_chla_ugL, center = TRUE, scale = TRUE),
         daily_EXO_do_mgL_ZT = scale(daily_EXO_do_mgL, center = TRUE, scale = TRUE),
         daily_EXO_do_persat_ZT = scale(daily_EXO_do_persat, center = TRUE, scale = TRUE),
         daily_EXO_wtr_temp_ZT = scale(daily_EXO_wtr_temp, center = TRUE, scale = TRUE),
         daily_SRup_mean_ZT = scale(daily_SRup_mean, center = TRUE, scale = TRUE),
         daily_SRup_max_ZT = scale(daily_SRup_max, center = TRUE, scale = TRUE),
         daily_rain_mm_ZT = scale(daily_rain_mm, center = TRUE, scale = TRUE),
         WVWA_Flow_cms_daily_mean_ZT = scale(WVWA_Flow_cms_daily_mean, center = TRUE, scale = TRUE),
         WRT_days_daily_ZT = scale(WRT_days_daily, center = TRUE, scale = TRUE))  #mutate gets ztransformed data for each driver 

head(day_fin)
tail(day_fin)

day_fin[is.na(day_fin)] <- NA  #sets NaN from inflow days with no data to NA  


#write.csv(day_fin, "./Data/ar_daily_data_joined.csv", row.names = FALSE)


#### MONTHLY JOIN  ####
#join monthly data and create csv for AR analysis 

join_monthly <- full_join(exo_monthly_final, met_monthly_final, by = "Date")

join_monthly_final <- full_join(join_monthly, inf_monthly_final, by = "Date")

head(join_monthly_final)
tail(join_monthly_final)

#write.csv(join_monthly_final, "./Data/ar_monthly_data_joined.csv", row.names = FALSE) 




#### 1-15 min TS plots ####
#Plot of fDOM and driver data from EDI data sets
#Not used in MS but used for preliminary visualization


par(mfrow = c(1,4))
library(tidyverse)
library(lubridate)
library(cowplot)
library(scales)
library(patchwork)

mytheme_AS <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    legend.key = element_blank(),legend.background = element_blank(),
                    legend.title = element_text(size = 12),  #all sizes were 10 previously 
                    legend.text=element_text(size=12),
                    axis.text=element_text(size=12),
                    axis.title=element_text(size=12,
                                            #face="bold"
                    ),
                    plot.title = element_text(size = 12, face = "bold", hjust = 0.5))


#10 min fDOM timeseries for VLWA and 10-15 min drivers 

tenminfdom <- exo_filta 

fdom_ts <- ggplot(data = tenminfdom, mapping = aes(x = DateTime, y = EXO_fdom_qsu))+
  geom_line(color = "maroon", size = 1)+
  labs(x = "Date",
       y = "fDOM (QSU)")+
  ggtitle("fDOM Concentration")+
  #ylim(c(0,0.17))+
  scale_x_datetime( labels = date_format("%b")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0))+
  mytheme
fdom_ten_ts
#ggsave(filename = "./Final_Data_Scripts/Images/tenmin_fDOM_OCTtoOCT.png", fdom_ten_ts, device = "png", width = 15, height = 10, units = "in")


DO_ts <- ggplot(data = tenminfdom, mapping = aes(x = DateTime, y = EXODOsat_percent_1))+
  geom_line(color = "blue", size = 1)+
  labs(x = "Date",
       y = "DO (%)")+
  ggtitle("Dissolved Oxygen")+
  #ylim(c(0,0.17))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme
DO_ts
#ggsave(filename = "./GLEON Poster/images/do_timeseries_fdomtimeframe.png", DO_ts, device = "png")

temp_ts <- ggplot(data = tenminfdom, mapping = aes(x = DateTime, y = EXOTemp_C_1))+
  geom_line(color = "black", size = 1)+
  labs(x = "Date",
       y = "Temperature (°C)")+
  ggtitle("Temperature")+
  #ylim(c(0,0.17))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme
temp_ts
#ggsave(filename = "./GLEON Poster/images/temp_timeseries_fdomtimeframe.png", temp_ts, device = "png")

chla_ts <- ggplot(data = tenminfdom, mapping = aes(x = DateTime, y = EXOChla_ugL_1))+
  geom_line(color = "green", size = 1)+
  labs(x = "Date",
       y = "Chl-a (ug/L)")+
  ggtitle("Chl-a")+
  ylim(c(0, 60))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme
chla_ts
#ggsave(filename = "./GLEON Poster/images/chla_timeseries_fdomtimeframe.png", chla_ts, device = "png")


tenminmet <- met_filt %>% 
  filter(DateTime >= as.POSIXct("2018-10-02 00:00:00", tz = "UTC"),
         DateTime <= as.POSIXct("2019-10-02 00:00:00", tz = "UTC"))

head(tenminmet)

SR_ts <- ggplot(data = tenminmet, mapping = aes(x = DateTime, y = ShortwaveRadiationUp_Average_W_m2))+
  geom_line(color = "black", size = 1)+
  labs(x = "Date",
       y = "SR (W/m^2)")+
  ggtitle("Shortwave Radiation")+
  ylim(c(0, 400))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme
SR_ts
#ggsave(filename = "./GLEON Poster/images/shortwave_timeseries_fdomtimeframe.png", SR_ts, device = "png")

rain_ts <- ggplot(data = tenminmet, mapping = aes(x = DateTime, y = Rain_Total_mm))+
  geom_line(color = "black", size = 1)+
  labs(x = "Date",
       y = "Precip (mm)")+
  ggtitle("Precipitation")+
  #ylim(c(0,0.17))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme
rain_ts
#ggsave(filename = "./GLEON Poster/images/rain_timeseries_fdomtimeframe.png", rain_ts, device = "png")



tenmininf <- inf_filt %>% 
  filter(DateTime >= as.POSIXct("2018-10-02 00:00:00", tz = "UTC"),
         DateTime <= as.POSIXct("2019-10-02 00:00:00", tz = "UTC")) %>% 
  mutate(WRT_days_tenmin = ( (3.1E5/WVWA_Flow_cms) * (1/60) * (1/60) * (1/24) ) ) 

head(tenmininf)

inf_ts <- ggplot(data = tenmininf, mapping = aes(x = DateTime, y = WVWA_Flow_cms))+
  geom_line(color = "black", size = 1)+
  labs(x = "Date",
       y = "Flow (cms)")+
  ggtitle("Inflow")+
  ylim(c(0,0.15))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme
inf_ts
#ggsave(filename = "./GLEON Poster/images/inf_timeseries_fdomtimeframe.png", inf_ts, device = "png")

WRT_ts <- ggplot(data = tenmininf, mapping = aes(x = DateTime, y = WRT_days_tenmin))+
  geom_line(color = "black", size = 1)+
  labs(x = "Date",
       y = "WRT (days)")+
  ggtitle("WRT")+
  #ylim(c(0,0.17))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  mytheme
WRT_ts
#ggsave(filename = "./GLEON Poster/images/WRT_timeseries_fdomtimeframe.png", WRT_ts, device = "png")


drivers <- plot_grid(fdom_ts, DO_ts, SR_ts, temp_ts, chla_ts, inf_ts, rain_ts, WRT_ts,
                     ncol=4, nrow=2, align="hv")
drivers

#ggsave(filename = "./Final_Data_Scripts/Images/driverscomp_timeseries_fdomtimeframe_TENMIN.png", drivers, device = "png",
#      width = 20, height = 10, units = "in")








