# read in selected correlation matrices for each year and entire dataset to pair selected variables back with dataset
# and then run iteratire linear model selection

#install.packages("MuMIn")
#install.packages("rsq")
library(MuMIn)
library(knitr)
library(rsq)
library(tidyverse)
library(Metrics)
library(zoo)
library(stats)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(scales)

mytheme_AS <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    legend.key = element_blank(),legend.background = element_blank(),
                    legend.title = element_text(size = 10),
                    legend.text=element_text(size=10),
                    axis.text=element_text(size=10),
                    axis.title=element_text(size=10,
                                            #face="bold"
                                            ),
                    plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

####################################  fDOM daily AR model results  #################################


data <- read.csv("./Data/variables_all_pluslag_fDOM_daily_final_RAINLAGS.csv")

data1316 <- data %>% 
  select(Date, daily_EXO_fdom, fdom_daily_ARlag1, daily_EXO_chla_ugL_ZT, daily_EXO_do_persat_ZT,
           daily_EXO_wtr_temp_ZT,
         daily_rain_mm_ZT, daily_rain_mm_lag1_ZT,
         daily_SRup_mean_ZT, WRT_days_daily_ZT) %>% 
  mutate(Date = as.Date(Date))

data1316 <- na.omit(data1316) #removes all rows w/ NAs that prevent model from running 
head(data1316)


# build a global model with the selected variables and then use dredge to see which combinations have the lowest AICc values
model_fdom <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT +
                    daily_EXO_wtr_temp_ZT +
                    daily_rain_mm_ZT + daily_rain_mm_lag1_ZT +
                    daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')


glm_1316 <- dredge(model_fdom, rank = "AICc", fixed = "fdom_daily_ARlag1") #scroll through glm_1316 to get lag only model

##Lines 52-61 used to created table with all models in supplement 
# export <- glm_1316
# export <- round(export, digits = 3)
# 
# export[is.na(export)] <- 0  #sets NA to 0's
# export$equation <- paste("fDOM = ", export$`(Intercept)`, export$fdom_daily_ARlag1, "(fDOM ARlag1)", export$daily_EXO_chla_ugL_ZT, "(Chla)",
#                          export$daily_EXO_do_persat_ZT, "(DO)", export$daily_EXO_wtr_temp_ZT, "(Temp)", export$daily_rain_mm_ZT, "(Precip)",
#                          export$daily_rain_mm_lag1_ZT, "(Precip lag1)", export$daily_SRup_mean_ZT, "(SW)",
#                          export$WRT_days_daily_ZT, "(WRT)",
#                          sep = " + ")
# write.csv(export, "./Data/Table_all_Daily_models_withequationformat_RAINLAGS.csv" , row.names = FALSE)

select_1316 <- subset(glm_1316, delta<2 )



mod1_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_rain_mm_lag1_ZT +
                   daily_rain_mm_ZT + daily_SRup_mean_ZT ,
                 data = data1316, family = gaussian, na.action = 'na.fail')


mod2_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT +
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT + daily_EXO_do_persat_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')


mod3_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT +
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')


 mod4_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT +
                  daily_rain_mm_lag1_ZT + daily_rain_mm_ZT + daily_EXO_chla_ugL_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
 
 mod5_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT +
                    daily_rain_mm_lag1_ZT + daily_EXO_do_persat_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')

modARonly_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1,
                 data = data1316, family = gaussian, na.action = 'na.fail')


# make predictions with the models
pred1_1316 <- predict(mod1_1316, newdata = data1316)
pred2_1316 <- predict(mod2_1316, newdata = data1316)
pred3_1316 <- predict(mod3_1316, newdata = data1316)
pred4_1316 <- predict(mod4_1316, newdata = data1316)
pred5_1316 <- predict(mod5_1316, newdata = data1316)
#pred6_1316 <- predict(mod6_1316, newdata = data1316)
predARonly_1316 <- predict(modARonly_1316, newdata = data1316)



# plot the predictions for the 2014 training dataset
plot(data1316$Date, data1316$daily_EXO_fdom, type = 'l', ylab = "fDOM (QSU)", xlab = "Date", pch = 16)
points(data1316$Date, pred1_1316, col = 'dodgerblue', type = 'l')
points(data1316$Date, pred2_1316, col = 'orange', type = 'l')
points(data1316$Date, pred3_1316, col = 'gold', type = 'l')
points(data1316$Date, pred4_1316, col = 'blue', type = 'l')
points(data1316$Date, pred5_1316, col = 'pink', type = 'l')
#points(data1316$Date, pred6_1316, col = 'purple', type = 'l')
points(data1316$Date, predARonly_1316, col = 'purple', type = 'l')
title("Selected models for daily fDOM")
legend("topright", legend=c("actual", "modeled"),
       col=c("black", "dodgerblue"), lty=1:2, cex=1.3)

plot(data1316$daily_EXO_fdom, type = 'l')
points(pred1_1316, col = 'red', type = 'l')
points(pred2_1316, col = 'orange', type = 'l')
points(pred3_1316, col = 'gold', type = 'l')
points(pred4_1316, col = 'blue', type = 'l')
points(pred5_1316, col = 'pink', type = 'l')
#points(pred6_1316, col = 'purple', type = 'l')
title("Selected models fDOM dataset")

# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_1316, type = 'sse')), digits = 2)
round((rsq(mod2_1316, type = 'sse')), digits = 2)
round((rsq(mod3_1316, type = 'sse')), digits = 2)
round((rsq(mod4_1316, type = 'sse')), digits = 2)
round((rsq(mod5_1316, type = 'sse')), digits = 2)
#round((rsq(mod6_1316, type = 'sse')), digits = 2)
round((rsq(modARonly_1316, type = 'sse')), digits = 2)


round(rmse(pred1_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred2_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred3_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred4_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred5_1316, data1316$daily_EXO_fdom), digits = 1)
#round(rmse(pred6_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(predARonly_1316, data1316$daily_EXO_fdom), digits = 1)

#Standard deviation 
sd(pred1_1316)
sd(pred2_1316)
sd(predARonly_1316)


#first diff data
diff <- diff(data$daily_EXO_fdom)
plot(diff, type = "l")


##get residual for plots 
resid <- data1316$daily_EXO_fdom - pred1_1316

binded_daily <- cbind(data1316, resid)
head(binded_daily)
binded_daily <- cbind(binded_daily, pred1_1316)
head(binded_daily)

# # old plotting lines for plotting actual fDOM vs modeled fdOM 
# par(mfrow = c(2,1))
# 
# plot(data1316$Date, data1316$daily_EXO_fdom, type = 'l', ylab = "fDOM (QSU)", 
#      #xlab = "Date", 
#      pch = 16, xaxt='n')
#      #, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
# points(data1316$Date, pred1_1316, col = 'dodgerblue', type = 'l', lty = "dashed")
# title("Selected model for daily fDOM")
# legend("topright", legend=c("observed", "predicted"),
#        col=c("black", "dodgerblue"), lty=1:2, cex= 0.7)

#plotting fDOM vs top ranked model predictions 
daily_selected <- ggplot()+
  geom_line(data = binded_daily, aes(x = Date, y = daily_EXO_fdom, color = "Observed"), size = 0.5)+
  geom_line( data = binded_daily, aes(x = Date, y = pred1_1316, color = "Predicted"), linetype = "dashed", size = 0.5)+
  labs(x = "Date",
       y = "fDOM (QSU)")+
  #ggtitle("Daily fDOM Concentration")+
  scale_y_continuous(limits = c(5,30), breaks = c(5,10,15,20,25,30))+
  scale_x_date(expand = c(0, 10), labels = date_format("%b"), date_breaks = "1 month")+ 
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
   theme(axis.title.x=element_blank(),
   axis.text.x=element_blank())+
  scale_color_manual("Model", limits=c("Observed", "Predicted"), values = c("black","blue")) +
  guides(colour = guide_legend(override.aes = list(pch = c(20, 25), fill = c("black", "blue"), size = 4)))+
  theme(legend.position = c(0.85, 0.85))+
  mytheme_AS
daily_selected


# # old plot command using base R to visualize residual plot. 
# plot(binded_daily$Date, binded_daily$resid, type = 'p', ylab = "Residual", xlab = "Date", pch = 16)
# title("Residual of Observed - Predicted Daily fDOM")
# # legend("topright", legend=c("observed", "predicted"),
# #        col=c("black", "dodgerblue"), lty=1:2, cex= 0.7)

#Plotting residual for daily models over time 
daily_resid <- ggplot()+
  geom_point(data = binded_daily, aes(x = Date, y = resid), color = "black", size = 1)+
  labs(x = "Date",
       y = "Residual (QSU)")+
  #ggtitle("Daily fDOM Concentration")+
  scale_y_continuous(limits = c(-5,10), breaks = c(-10, -5, 0, 5, 10))+
  scale_x_date(expand = c(0, 10), labels = date_format("%b"), date_breaks = "1 month")+ 
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  #theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  # theme(axis.title.x=element_blank(),
  # axis.text.x=element_blank())+
  #scale_color_manual("Model", limits=c("Observed", "Predicted"), values = c("black","blue")) +
  #guides(colour = guide_legend(override.aes = list(pch = c(20, 25), fill = c("black", "blue"), size = 4)))+
  #theme(legend.position = c(0.85, 0.85))+
  geom_hline(yintercept = 0, linetype="dashed")+
  mytheme_AS
daily_resid



##Residual stats UPDATED NOW FOR MODELS WITH BOTH RAIN TERMS
mean(binded_daily$resid, na.rm = TRUE)  ## mean resid = 2.06 x 10^-15
sd(binded_daily$resid, na.rm = TRUE)  ## sd resid = 0.76

nov_april_resid <- binded_daily %>% 
  filter(Date > "2018-10-31",
         Date < "2019-05-01")
mean(nov_april_resid$resid, na.rm = TRUE)  ## mean resid = - 0.002
sd(nov_april_resid$resid, na.rm = TRUE)  ## sd resid = 0.49

oct_resid <- binded_daily %>% 
  filter(Date >= "2018-10-01",
         Date < "2018-11-01")
mean(oct_resid$resid, na.rm = TRUE)  ## mean resid = 0.09
sd(oct_resid$resid, na.rm = TRUE)  ## sd resid = 0.80

may_sep_resid <- binded_daily %>% 
  filter(Date >= "2019-05-01")
mean(may_sep_resid$resid, na.rm = TRUE)  ## mean resid = - 0.015
sd(may_sep_resid$resid, na.rm = TRUE)  ## sd resid = 1.00



####################################  fDOM monthly AR model results  #################################


data <- read.csv("./Data/variables_all_pluslag_fDOM_monthly_final.csv")


#selecting data for either Temp or WRT, keeping all other driver variables the same 
data_temp <- data %>% 
  select(Date, monthly_EXOfdom, fdom_monthly_ARlag1, monthly_EXOchla_ugL_ZT, monthly_EXOdo_persat_ZT,
           monthly_EXOwtr_temp_ZT,  monthly_rain_mm_ZT) %>% 
  mutate(Date = as.Date(Date))

data1316_temp <- data_temp %>% 
  filter(Date > "2018-09-01")  #getting dates just for study period 
data1316_temp




# build a global model with the selected variables and then use dredge to see which combinations have the lowest AICc values
##global model w/ TEMP
model_fdom_T <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_EXOdo_persat_ZT +
                    monthly_EXOwtr_temp_ZT +  monthly_rain_mm_ZT,
                  family = gaussian, na.action = 'na.pass', data = data1316_temp)

glm_1316_T <- dredge(model_fdom_T, rank = "AICc", fixed = "fdom_monthly_ARlag1") #scroll through glm_1316 to get lag only model
#write.csv(glm_1316_T, "./Data/Table_all_Monthly_models.csv" , row.names = FALSE)
select_1316_T <- subset(glm_1316_T, delta<2 )

##global model w/ WRT
# model_fdom_W <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_EXOdo_persat_ZT +
#                     monthly_rain_mm_ZT + WRT_days_monthly_ZT,
#                   family = gaussian, na.action = 'na.pass', data = data1316_wrt)
# 
# glm_1316_W <- dredge(model_fdom_W, rank = "AICc", fixed = "fdom_monthly_ARlag1") #scroll through glm_1316 to get lag only model
# select_1316_W <- subset(glm_1316_W, delta<2 )




# build the two individual models selected from dredge, These are the four different rows created in select_1316

##models, preds, and stats for TEMP
{
  mod1_1316_T <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOwtr_temp_ZT,
                     data = data1316_temp, family = gaussian, na.action = 'na.fail')
  
  
  # mod2_1316_T <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOwtr_temp_ZT + monthly_EXOdo_persat_ZT + monthly_rain_mm_ZT,
  #                    data = data1316_temp, family = gaussian, na.action = 'na.fail')
  
  mod3_1316_T <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1,
                     data = data1316_temp, family = gaussian, na.action = 'na.fail')
  
  pred1_1316_T <- predict(mod1_1316_T, newdata = data1316_temp)
  #pred2_1316_T <- predict(mod2_1316_T, newdata = data1316_temp)
  pred3_1316_T <- predict(mod3_1316_T, newdata = data1316_temp)
  
  round((rsq(mod1_1316_T, type = 'sse')), digits = 2)
  #round((rsq(mod2_1316_T, type = 'sse')), digits = 2)
  round((rsq(mod3_1316_T, type = 'sse')), digits = 2)
  
  
  round(rmse(pred1_1316_T, data1316_temp$monthly_EXOfdom), digits = 1)
  #round(rmse(pred2_1316_T, data1316_temp$monthly_EXOfdom), digits = 1)
  round(rmse(pred3_1316_T, data1316_temp$monthly_EXOfdom), digits = 1)
  
  plot(data1316_temp$Date, data1316_temp$monthly_EXOfdom, type = 'l', ylab = "fDOM (QSU)", xlab = "Date")
  points(data1316_temp$Date, pred1_1316_T, col = 'dodgerblue', type = 'l')
  #points(data1316_temp$Date, pred2_1316_T, col = 'orange', type = 'l')
  points(data1316_temp$Date, pred3_1316_T, col = 'red', type = 'l')
  title("Selected model for monthly fDOM")
  
  
  ##get residual for plots 
  resid <- data1316_temp$monthly_EXOfdom - pred1_1316_T
  
  binded_monthly <- cbind(data1316_temp, resid)
  head(binded_monthly)
  binded_monthly <- cbind(binded_monthly, pred1_1316_T)
  head(binded_monthly)
  
  par(mfrow = c(2,1))
  
  plot(data1316_temp$Date, data1316_temp$monthly_EXOfdom, type = 'l', ylab = "fDOM (QSU)", 
       xlab = "Date", pch = 16)
       #, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  points(data1316_temp$Date, pred1_1316_T, col = 'blue', type = 'l', lty = "dashed")
  title("Selected model for monthly fDOM")
  legend("topright", legend=c("observed", "predicted"),
         col=c("black", "blue"), lty=1:2, cex= 0.7)
  
  plot(binded_monthly$Date, binded_monthly$resid, type = 'l', ylab = "Residual", xlab = "Date", pch = 16)
  title("Residual of Observed - Predicted fDOM")
  
  #Residual stats 
  mean(binded_monthly$resid, na.rm = TRUE)  ## mean resid = -3.85 x 10^-15
  sd(binded_monthly$resid, na.rm = TRUE)  ## sd resid = 2.61 
view(binded_monthly)
  
  
  
  
  monthly_selected <- ggplot()+
    geom_line(data = binded_monthly, aes(x = Date, y = monthly_EXOfdom, color = "Observed"), size = 0.5)+
    geom_line( data = binded_monthly, aes(x = Date, y = pred1_1316_T, color = "Predicted"), linetype = "dashed", size = 0.5)+
    labs(x = "Date",
         y = "fDOM (QSU)")+
    #ggtitle("Daily fDOM Concentration")+
    scale_y_continuous(limits = c(5,30), breaks = c(5,10,15,20,25,30))+
    scale_x_date(expand = c(0, 10), labels = date_format("%b %Y"), date_breaks = "1 month")+ 
        #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
    theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank())+
    scale_color_manual("Model", limits=c("Observed", "Predicted"), values = c("black","blue")) +
    guides(colour = guide_legend(override.aes = list(pch = c(20, 25), fill = c("black", "blue"), size = 4)))+
    theme(legend.position = c(0.85, 0.85))+
    mytheme_AS
  monthly_selected
  
  monthly_resid <- ggplot()+
    geom_point(data = binded_monthly, aes(x = Date, y = resid), color = "black", size = 1)+
    labs(x = "Date",
         y = "Residual (QSU)")+
    #ggtitle("Daily fDOM Concentration")+
    scale_y_continuous(limits = c(-5, 10), breaks = c(-5, 0, 5, 10))+
    scale_x_date(expand = c(0, 15), labels = date_format("%b"), date_breaks = "1 month")+ 
    #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
    #theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
    # theme(axis.title.x=element_blank(),
    # axis.text.x=element_blank())+
    #scale_color_manual("Model", limits=c("Observed", "Predicted"), values = c("black","blue")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(20, 25), fill = c("black", "blue"), size = 4)))+
    #theme(legend.position = c(0.85, 0.85))+
    geom_hline(yintercept = 0, linetype="dashed")+
    mytheme_AS
  monthly_resid
  
  
}

##models, preds, and stats for WRt
{
  # mod1_1316_W <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1,
  #                    data = data1316_wrt, family = gaussian, na.action = 'na.fail')
  # 
  # 
  # mod2_1316_W <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + WRT_days_monthly_ZT,
  #                    data = data1316_wrt, family = gaussian, na.action = 'na.fail')
  # 
  # mod3_1316_W <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_rain_mm_ZT,
  #                    data = data1316_wrt, family = gaussian, na.action = 'na.fail')
  # 
  # pred1_1316_W <- predict(mod1_1316_W, newdata = data1316_wrt)
  # pred2_1316_W <- predict(mod2_1316_W, newdata = data1316_wrt)
  # pred3_1316_W <- predict(mod3_1316_W, newdata = data1316_wrt)
  # 
  # round((rsq(mod1_1316_W, type = 'sse')), digits = 3)
  # round((rsq(mod2_1316_W, type = 'sse')), digits = 3)
  # round((rsq(mod3_1316_W, type = 'sse')), digits = 3)
  # 
  # 
  # round(rmse(pred1_1316_W, data1316_wrt$monthly_EXOfdom), digits = 3)
  # round(rmse(pred2_1316_W, data1316_wrt$monthly_EXOfdom), digits = 3)
  # round(rmse(pred3_1316_W, data1316_wrt$monthly_EXOfdom), digits = 3)
  # 
  # plot(data1316_wrt$Date, data1316_wrt$monthly_EXOfdom, type = 'l', ylab = "fDOM (QSU)", xlab = "Date")
  # points(data1316_wrt$Date, pred1_1316_W, col = 'dodgerblue', type = 'l')
  # points(data1316_wrt$Date, pred2_1316_W, col = 'orange', type = 'l')
  # points(data1316_wrt$Date, pred3_1316_W, col = 'red', type = 'l')
  # title("Selected model for monthly fDOM")
  
}







#####  Final model selected and resid plot  #####

# fig6 <- ggarrange(daily_selected, monthly_selected, daily_resid, monthly_resid,
#                      labels = c("a", "b", "c", "d"),
#                      ncol = 2, nrow = 2)

 fig6 <- plot_grid(daily_selected, monthly_selected, daily_resid, monthly_resid,
                        labels = c("a", "b", "c", "d"),
                        ncol = 2, nrow = 2, align="hv")

#drivers <- (DO_ts | SR_ts | temp_ts | chla_ts) / (inf_ts | rain_ts | WRT_ts)

fig6

ggsave(filename = "./Images/Fig6.tiff", 
       fig6, device = "tiff", width = 230, height = 200, units = "mm")




#### Getting R2 and RMSE for all daily models ####  

mod1_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred1_1316 <- predict(mod1_1316, newdata = data1316)

mod2_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred2_1316 <- predict(mod2_1316, newdata = data1316)


mod3_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred3_1316 <- predict(mod3_1316, newdata = data1316)


mod4_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred4_1316 <- predict(mod4_1316, newdata = data1316)


mod5_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred5_1316 <- predict(mod5_1316, newdata = data1316)


mod6_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred6_1316 <- predict(mod6_1316, newdata = data1316)


mod7_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred7_1316 <- predict(mod7_1316, newdata = data1316)


mod8_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred8_1316 <- predict(mod8_1316, newdata = data1316)


mod9_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +  daily_EXO_do_persat_ZT +
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred9_1316 <- predict(mod9_1316, newdata = data1316)


mod10_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + 
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred10_1316 <- predict(mod10_1316, newdata = data1316)


mod11_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred11_1316 <- predict(mod11_1316, newdata = data1316)


mod12_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred12_1316 <- predict(mod12_1316, newdata = data1316)


mod13_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred13_1316 <- predict(mod13_1316, newdata = data1316)


mod14_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred14_1316 <- predict(mod14_1316, newdata = data1316)


mod15_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +  daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred15_1316 <- predict(mod15_1316, newdata = data1316)


mod16_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred16_1316 <- predict(mod16_1316, newdata = data1316)


mod17_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred17_1316 <- predict(mod17_1316, newdata = data1316)


mod18_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred18_1316 <- predict(mod18_1316, newdata = data1316)


mod19_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT   + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred19_1316 <- predict(mod19_1316, newdata = data1316)


mod20_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred20_1316 <- predict(mod20_1316, newdata = data1316)


mod21_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred21_1316 <- predict(mod21_1316, newdata = data1316)


mod22_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred22_1316 <- predict(mod22_1316, newdata = data1316)


mod23_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred23_1316 <- predict(mod23_1316, newdata = data1316)


mod24_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred24_1316 <- predict(mod24_1316, newdata = data1316)


mod25_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred25_1316 <- predict(mod25_1316, newdata = data1316)


mod26_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred26_1316 <- predict(mod26_1316, newdata = data1316)


mod27_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + 
                   daily_rain_mm_lag1_ZT +  WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred27_1316 <- predict(mod27_1316, newdata = data1316)


mod28_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT +  WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred28_1316 <- predict(mod28_1316, newdata = data1316)


mod29_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred29_1316 <- predict(mod29_1316, newdata = data1316)


mod30_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred30_1316 <- predict(mod30_1316, newdata = data1316)


mod31_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +  daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred31_1316 <- predict(mod31_1316, newdata = data1316)


mod32_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +  
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred32_1316 <- predict(mod32_1316, newdata = data1316)


mod33_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred33_1316 <- predict(mod33_1316, newdata = data1316)

mod34_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                   daily_rain_mm_lag1_ZT +  WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred34_1316 <- predict(mod34_1316, newdata = data1316)

mod35_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred35_1316 <- predict(mod35_1316, newdata = data1316)

mod36_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred36_1316 <- predict(mod36_1316, newdata = data1316)

mod37_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred37_1316 <- predict(mod37_1316, newdata = data1316)

mod38_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred38_1316 <- predict(mod38_1316, newdata = data1316)

mod39_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred39_1316 <- predict(mod39_1316, newdata = data1316)

mod40_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred40_1316 <- predict(mod40_1316, newdata = data1316)

mod41_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred41_1316 <- predict(mod41_1316, newdata = data1316)

mod42_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred42_1316 <- predict(mod42_1316, newdata = data1316)

mod43_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred43_1316 <- predict(mod43_1316, newdata = data1316)

mod44_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + 
                   daily_rain_mm_lag1_ZT  +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred44_1316 <- predict(mod44_1316, newdata = data1316)

mod45_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred45_1316 <- predict(mod45_1316, newdata = data1316)

mod46_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred46_1316 <- predict(mod46_1316, newdata = data1316)

mod47_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred47_1316 <- predict(mod47_1316, newdata = data1316)

mod48_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred48_1316 <- predict(mod48_1316, newdata = data1316)

mod49_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred49_1316 <- predict(mod49_1316, newdata = data1316)

mod50_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + 
                   daily_rain_mm_lag1_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred50_1316 <- predict(mod50_1316, newdata = data1316)

mod51_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + 
                   daily_rain_mm_lag1_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred51_1316 <- predict(mod51_1316, newdata = data1316)

mod52_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred52_1316 <- predict(mod52_1316, newdata = data1316)

mod53_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred53_1316 <- predict(mod53_1316, newdata = data1316)

mod54_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred54_1316 <- predict(mod54_1316, newdata = data1316)

mod55_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                   daily_rain_mm_lag1_ZT +    daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred55_1316 <- predict(mod55_1316, newdata = data1316)

mod56_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred56_1316 <- predict(mod56_1316, newdata = data1316)

mod57_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred57_1316 <- predict(mod57_1316, newdata = data1316)

mod58_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred58_1316 <- predict(mod58_1316, newdata = data1316)

mod59_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred59_1316 <- predict(mod59_1316, newdata = data1316)

mod60_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred60_1316 <- predict(mod60_1316, newdata = data1316)

mod61_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred61_1316 <- predict(mod61_1316, newdata = data1316)

mod62_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred62_1316 <- predict(mod62_1316, newdata = data1316)

mod63_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred63_1316 <- predict(mod63_1316, newdata = data1316)

mod64_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_lag1_ZT + daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred64_1316 <- predict(mod64_1316, newdata = data1316)

mod65_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_wtr_temp_ZT + 
                    daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred65_1316 <- predict(mod65_1316, newdata = data1316)

mod66_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_wtr_temp_ZT + 
                    daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred66_1316 <- predict(mod66_1316, newdata = data1316)

mod67_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                     daily_SRup_mean_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred67_1316 <- predict(mod67_1316, newdata = data1316)

mod68_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred68_1316 <- predict(mod68_1316, newdata = data1316)

mod69_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                      daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred69_1316 <- predict(mod69_1316, newdata = data1316)

mod70_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + 
                     daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred70_1316 <- predict(mod70_1316, newdata = data1316)

mod71_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred71_1316 <- predict(mod71_1316, newdata = data1316)

mod72_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred72_1316 <- predict(mod72_1316, newdata = data1316)

mod73_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                     daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred73_1316 <- predict(mod73_1316, newdata = data1316)

mod74_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred74_1316 <- predict(mod74_1316, newdata = data1316)

mod75_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                     daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred75_1316 <- predict(mod75_1316, newdata = data1316)

mod76_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred76_1316 <- predict(mod76_1316, newdata = data1316)

mod77_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                     daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred77_1316 <- predict(mod77_1316, newdata = data1316)

mod78_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred78_1316 <- predict(mod78_1316, newdata = data1316)

mod79_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred79_1316 <- predict(mod79_1316, newdata = data1316)

mod80_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_wtr_temp_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred80_1316 <- predict(mod80_1316, newdata = data1316)

mod81_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                     daily_SRup_mean_ZT ,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred81_1316 <- predict(mod81_1316, newdata = data1316)

mod82_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred82_1316 <- predict(mod82_1316, newdata = data1316)

mod83_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                     daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred83_1316 <- predict(mod83_1316, newdata = data1316)

mod84_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred84_1316 <- predict(mod84_1316, newdata = data1316)

mod85_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred85_1316 <- predict(mod85_1316, newdata = data1316)

mod86_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred86_1316 <- predict(mod86_1316, newdata = data1316)

mod87_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred87_1316 <- predict(mod87_1316, newdata = data1316)

mod88_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred88_1316 <- predict(mod88_1316, newdata = data1316)

mod89_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT +  WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred89_1316 <- predict(mod89_1316, newdata = data1316)

mod90_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred90_1316 <- predict(mod90_1316, newdata = data1316)

mod91_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred91_1316 <- predict(mod91_1316, newdata = data1316)

mod92_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred92_1316 <- predict(mod92_1316, newdata = data1316)

mod93_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred93_1316 <- predict(mod93_1316, newdata = data1316)

mod94_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                    WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred94_1316 <- predict(mod94_1316, newdata = data1316)

mod95_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred95_1316 <- predict(mod95_1316, newdata = data1316)

mod96_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1   + daily_EXO_do_persat_ZT  + 
                     daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred96_1316 <- predict(mod96_1316, newdata = data1316)

mod97_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred97_1316 <- predict(mod97_1316, newdata = data1316)

mod98_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred98_1316 <- predict(mod98_1316, newdata = data1316)

mod99_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred99_1316 <- predict(mod99_1316, newdata = data1316)

mod100_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred100_1316 <- predict(mod100_1316, newdata = data1316)

mod101_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred101_1316 <- predict(mod101_1316, newdata = data1316)

mod102_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + 
                   daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred102_1316 <- predict(mod102_1316, newdata = data1316)

mod103_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred103_1316 <- predict(mod103_1316, newdata = data1316)

mod104_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                      WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred104_1316 <- predict(mod104_1316, newdata = data1316)

mod105_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                     daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred105_1316 <- predict(mod105_1316, newdata = data1316)

mod106_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred106_1316 <- predict(mod106_1316, newdata = data1316)

mod107_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred107_1316 <- predict(mod107_1316, newdata = data1316)

mod108_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred108_1316 <- predict(mod108_1316, newdata = data1316)

mod109_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                     WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred109_1316 <- predict(mod109_1316, newdata = data1316)

mod110_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred110_1316 <- predict(mod110_1316, newdata = data1316)

mod111_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                     daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred111_1316 <- predict(mod111_1316, newdata = data1316)

mod112_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                     daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred112_1316 <- predict(mod112_1316, newdata = data1316)

mod113_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred113_1316 <- predict(mod113_1316, newdata = data1316)

mod114_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred114_1316 <- predict(mod114_1316, newdata = data1316)

mod115_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred115_1316 <- predict(mod115_1316, newdata = data1316)

mod116_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred116_1316 <- predict(mod116_1316, newdata = data1316)

mod117_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                    WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred117_1316 <- predict(mod117_1316, newdata = data1316)

mod118_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +   daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred118_1316 <- predict(mod118_1316, newdata = data1316)

mod119_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred119_1316 <- predict(mod119_1316, newdata = data1316)

mod120_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + 
                   daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred120_1316 <- predict(mod120_1316, newdata = data1316)

mod121_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +
                   daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred121_1316 <- predict(mod121_1316, newdata = data1316)

mod122_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +  
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred122_1316 <- predict(mod122_1316, newdata = data1316)

mod123_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred123_1316 <- predict(mod123_1316, newdata = data1316)

mod124_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred124_1316 <- predict(mod124_1316, newdata = data1316)

mod125_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT  + 
                   daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred125_1316 <- predict(mod125_1316, newdata = data1316)

mod126_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred126_1316 <- predict(mod126_1316, newdata = data1316)

mod127_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + 
                   daily_rain_mm_ZT +  daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred127_1316 <- predict(mod127_1316, newdata = data1316)

mod128_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT + 
                   daily_rain_mm_ZT  + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred128_1316 <- predict(mod128_1316, newdata = data1316)




round((rsq(mod1_1316, type = 'sse')), digits = 2)
round(rmse(pred1_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod2_1316, type = 'sse')), digits = 2)
round(rmse(pred2_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod3_1316, type = 'sse')), digits = 2)
round(rmse(pred3_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod4_1316, type = 'sse')), digits = 2)
round(rmse(pred4_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod5_1316, type = 'sse')), digits = 2)
round(rmse(pred5_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod6_1316, type = 'sse')), digits = 2)
round(rmse(pred6_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod7_1316, type = 'sse')), digits = 2)
round(rmse(pred7_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod8_1316, type = 'sse')), digits = 2)
round(rmse(pred8_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod9_1316, type = 'sse')), digits = 2)
round(rmse(pred9_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod10_1316, type = 'sse')), digits = 2)
round(rmse(pred10_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod11_1316, type = 'sse')), digits = 2)
round(rmse(pred11_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod12_1316, type = 'sse')), digits = 2)
round(rmse(pred12_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod13_1316, type = 'sse')), digits = 2)
round(rmse(pred13_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod14_1316, type = 'sse')), digits = 2)
round(rmse(pred14_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod15_1316, type = 'sse')), digits = 2)
round(rmse(pred15_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod16_1316, type = 'sse')), digits = 2)
round(rmse(pred16_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod17_1316, type = 'sse')), digits = 2)
round(rmse(pred17_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod18_1316, type = 'sse')), digits = 2)
round(rmse(pred18_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod19_1316, type = 'sse')), digits = 2)
round(rmse(pred19_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod20_1316, type = 'sse')), digits = 2)
round(rmse(pred20_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod21_1316, type = 'sse')), digits = 2)
round(rmse(pred21_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod22_1316, type = 'sse')), digits = 2)
round(rmse(pred22_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod23_1316, type = 'sse')), digits = 2)
round(rmse(pred23_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod24_1316, type = 'sse')), digits = 2)
round(rmse(pred24_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod24_1316, type = 'sse')), digits = 2)
round(rmse(pred24_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod25_1316, type = 'sse')), digits = 2)
round(rmse(pred25_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod26_1316, type = 'sse')), digits = 2)
round(rmse(pred26_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod27_1316, type = 'sse')), digits = 2)
round(rmse(pred27_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod28_1316, type = 'sse')), digits = 2)
round(rmse(pred28_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod29_1316, type = 'sse')), digits = 2)
round(rmse(pred29_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod30_1316, type = 'sse')), digits = 2)
round(rmse(pred30_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod31_1316, type = 'sse')), digits = 2)
round(rmse(pred31_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod32_1316, type = 'sse')), digits = 2)
round(rmse(pred32_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod33_1316, type = 'sse')), digits = 2)
round(rmse(pred33_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod34_1316, type = 'sse')), digits = 2)
round(rmse(pred34_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod35_1316, type = 'sse')), digits = 2)
round(rmse(pred35_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod36_1316, type = 'sse')), digits = 2)
round(rmse(pred36_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod37_1316, type = 'sse')), digits = 2)
round(rmse(pred37_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod38_1316, type = 'sse')), digits = 2)
round(rmse(pred38_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod39_1316, type = 'sse')), digits = 2)
round(rmse(pred39_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod40_1316, type = 'sse')), digits = 2)
round(rmse(pred40_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod41_1316, type = 'sse')), digits = 2)
round(rmse(pred41_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod42_1316, type = 'sse')), digits = 2)
round(rmse(pred42_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod43_1316, type = 'sse')), digits = 2)
round(rmse(pred43_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod44_1316, type = 'sse')), digits = 2)
round(rmse(pred44_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod45_1316, type = 'sse')), digits = 2)
round(rmse(pred45_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod46_1316, type = 'sse')), digits = 2)
round(rmse(pred46_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod47_1316, type = 'sse')), digits = 2)
round(rmse(pred47_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod48_1316, type = 'sse')), digits = 2)
round(rmse(pred48_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod49_1316, type = 'sse')), digits = 2)
round(rmse(pred49_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod50_1316, type = 'sse')), digits = 2)
round(rmse(pred50_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod51_1316, type = 'sse')), digits = 2)
round(rmse(pred51_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod52_1316, type = 'sse')), digits = 2)
round(rmse(pred52_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod53_1316, type = 'sse')), digits = 2)
round(rmse(pred53_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod54_1316, type = 'sse')), digits = 2)
round(rmse(pred54_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod55_1316, type = 'sse')), digits = 2)
round(rmse(pred55_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod56_1316, type = 'sse')), digits = 2)
round(rmse(pred56_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod57_1316, type = 'sse')), digits = 2)
round(rmse(pred57_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod58_1316, type = 'sse')), digits = 2)
round(rmse(pred58_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod59_1316, type = 'sse')), digits = 2)
round(rmse(pred59_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod60_1316, type = 'sse')), digits = 2)
round(rmse(pred60_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod61_1316, type = 'sse')), digits = 2)
round(rmse(pred61_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod62_1316, type = 'sse')), digits = 2)
round(rmse(pred62_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod63_1316, type = 'sse')), digits = 2)
round(rmse(pred63_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod64_1316, type = 'sse')), digits = 2)
round(rmse(pred64_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod65_1316, type = 'sse')), digits = 2)
round(rmse(pred65_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod66_1316, type = 'sse')), digits = 2)
round(rmse(pred66_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod67_1316, type = 'sse')), digits = 2)
round(rmse(pred67_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod68_1316, type = 'sse')), digits = 2)
round(rmse(pred68_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod69_1316, type = 'sse')), digits = 2)
round(rmse(pred69_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod70_1316, type = 'sse')), digits = 2)
round(rmse(pred70_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod71_1316, type = 'sse')), digits = 2)
round(rmse(pred71_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod72_1316, type = 'sse')), digits = 2)
round(rmse(pred72_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod73_1316, type = 'sse')), digits = 2)
round(rmse(pred73_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod74_1316, type = 'sse')), digits = 2)
round(rmse(pred74_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod75_1316, type = 'sse')), digits = 2)
round(rmse(pred75_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod76_1316, type = 'sse')), digits = 2)
round(rmse(pred76_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod77_1316, type = 'sse')), digits = 2)
round(rmse(pred77_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod78_1316, type = 'sse')), digits = 2)
round(rmse(pred78_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod79_1316, type = 'sse')), digits = 2)
round(rmse(pred79_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod80_1316, type = 'sse')), digits = 2)
round(rmse(pred80_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod81_1316, type = 'sse')), digits = 2)
round(rmse(pred81_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod82_1316, type = 'sse')), digits = 2)
round(rmse(pred82_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod83_1316, type = 'sse')), digits = 2)
round(rmse(pred83_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod84_1316, type = 'sse')), digits = 2)
round(rmse(pred84_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod85_1316, type = 'sse')), digits = 2)
round(rmse(pred85_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod86_1316, type = 'sse')), digits = 2)
round(rmse(pred86_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod87_1316, type = 'sse')), digits = 2)
round(rmse(pred87_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod88_1316, type = 'sse')), digits = 2)
round(rmse(pred88_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod89_1316, type = 'sse')), digits = 2)
round(rmse(pred89_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod90_1316, type = 'sse')), digits = 2)
round(rmse(pred90_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod91_1316, type = 'sse')), digits = 2)
round(rmse(pred91_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod92_1316, type = 'sse')), digits = 2)
round(rmse(pred92_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod93_1316, type = 'sse')), digits = 2)
round(rmse(pred93_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod94_1316, type = 'sse')), digits = 2)
round(rmse(pred94_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod95_1316, type = 'sse')), digits = 2)
round(rmse(pred95_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod96_1316, type = 'sse')), digits = 2)
round(rmse(pred96_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod97_1316, type = 'sse')), digits = 2)
round(rmse(pred97_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod98_1316, type = 'sse')), digits = 2)
round(rmse(pred98_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod99_1316, type = 'sse')), digits = 2)
round(rmse(pred99_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod100_1316, type = 'sse')), digits = 2)
round(rmse(pred100_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod101_1316, type = 'sse')), digits = 2)
round(rmse(pred101_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod102_1316, type = 'sse')), digits = 2)
round(rmse(pred102_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod103_1316, type = 'sse')), digits = 2)
round(rmse(pred103_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod104_1316, type = 'sse')), digits = 2)
round(rmse(pred104_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod105_1316, type = 'sse')), digits = 2)
round(rmse(pred105_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod106_1316, type = 'sse')), digits = 2)
round(rmse(pred106_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod107_1316, type = 'sse')), digits = 2)
round(rmse(pred107_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod108_1316, type = 'sse')), digits = 2)
round(rmse(pred108_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod109_1316, type = 'sse')), digits = 2)
round(rmse(pred109_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod110_1316, type = 'sse')), digits = 2)
round(rmse(pred110_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod111_1316, type = 'sse')), digits = 2)
round(rmse(pred111_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod112_1316, type = 'sse')), digits = 2)
round(rmse(pred112_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod113_1316, type = 'sse')), digits = 2)
round(rmse(pred113_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod114_1316, type = 'sse')), digits = 2)
round(rmse(pred114_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod115_1316, type = 'sse')), digits = 2)
round(rmse(pred115_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod116_1316, type = 'sse')), digits = 2)
round(rmse(pred116_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod117_1316, type = 'sse')), digits = 2)
round(rmse(pred117_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod118_1316, type = 'sse')), digits = 2)
round(rmse(pred118_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod119_1316, type = 'sse')), digits = 2)
round(rmse(pred119_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod120_1316, type = 'sse')), digits = 2)
round(rmse(pred120_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod121_1316, type = 'sse')), digits = 2)
round(rmse(pred121_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod122_1316, type = 'sse')), digits = 2)
round(rmse(pred122_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod123_1316, type = 'sse')), digits = 2)
round(rmse(pred123_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod124_1316, type = 'sse')), digits = 2)
round(rmse(pred124_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod125_1316, type = 'sse')), digits = 2)
round(rmse(pred125_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod126_1316, type = 'sse')), digits = 2)
round(rmse(pred126_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod127_1316, type = 'sse')), digits = 2)
round(rmse(pred127_1316, data1316$daily_EXO_fdom), digits = 1)

round((rsq(mod128_1316, type = 'sse')), digits = 2)
round(rmse(pred128_1316, data1316$daily_EXO_fdom), digits = 1)


#### Getting R2 and RMSE for all monthly models ####

#renaming data to fit prior code 
data1316 <- data1316_temp
head(data1316)

mod1_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOwtr_temp_ZT,
                 data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred1_1316 <- predict(mod1_1316, newdata = data1316)

mod2_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred2_1316 <- predict(mod2_1316, newdata = data1316)

mod3_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOdo_persat_ZT + monthly_EXOwtr_temp_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred3_1316 <- predict(mod3_1316, newdata = data1316)

mod4_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOdo_persat_ZT +
                    monthly_EXOwtr_temp_ZT + monthly_rain_mm_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred4_1316 <- predict(mod4_1316, newdata = data1316)

mod5_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_EXOwtr_temp_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred5_1316 <- predict(mod5_1316, newdata = data1316)

mod6_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOwtr_temp_ZT + monthly_rain_mm_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred6_1316 <- predict(mod6_1316, newdata = data1316)

mod7_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_rain_mm_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred7_1316 <- predict(mod7_1316, newdata = data1316)

mod8_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOdo_persat_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred8_1316 <- predict(mod8_1316, newdata = data1316)

mod9_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred9_1316 <- predict(mod9_1316, newdata = data1316)

mod10_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_EXOdo_persat_ZT +
                    monthly_EXOwtr_temp_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred10_1316 <- predict(mod10_1316, newdata = data1316)

mod11_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT +
                    monthly_EXOwtr_temp_ZT + monthly_rain_mm_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred11_1316 <- predict(mod11_1316, newdata = data1316)

mod12_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_rain_mm_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred12_1316 <- predict(mod12_1316, newdata = data1316)

mod13_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_EXOdo_persat_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred13_1316 <- predict(mod13_1316, newdata = data1316)

mod14_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOdo_persat_ZT + monthly_rain_mm_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred14_1316 <- predict(mod14_1316, newdata = data1316)


mod15_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_EXOdo_persat_ZT +
                    monthly_EXOwtr_temp_ZT + monthly_rain_mm_ZT,
                 data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred15_1316 <- predict(mod15_1316, newdata = data1316)

mod16_1316 <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_EXOdo_persat_ZT +
                     monthly_rain_mm_ZT,
                  data = data1316_temp, family = gaussian, na.action = 'na.fail')
pred16_1316 <- predict(mod16_1316, newdata = data1316)



round((rsq(mod1_1316, type = 'sse')), digits = 2)
round(rmse(pred1_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod2_1316, type = 'sse')), digits = 2)
round(rmse(pred2_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod3_1316, type = 'sse')), digits = 2)
round(rmse(pred3_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod4_1316, type = 'sse')), digits = 2)
round(rmse(pred4_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod5_1316, type = 'sse')), digits = 2)
round(rmse(pred5_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod6_1316, type = 'sse')), digits = 2)
round(rmse(pred6_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod7_1316, type = 'sse')), digits = 2)
round(rmse(pred7_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod8_1316, type = 'sse')), digits = 2)
round(rmse(pred8_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod9_1316, type = 'sse')), digits = 2)
round(rmse(pred9_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod10_1316, type = 'sse')), digits = 2)
round(rmse(pred10_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod11_1316, type = 'sse')), digits = 2)
round(rmse(pred11_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod12_1316, type = 'sse')), digits = 2)
round(rmse(pred12_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod13_1316, type = 'sse')), digits = 2)
round(rmse(pred13_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod14_1316, type = 'sse')), digits = 2)
round(rmse(pred14_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod15_1316, type = 'sse')), digits = 2)
round(rmse(pred15_1316, data1316$monthly_EXOfdom), digits = 1)

round((rsq(mod16_1316, type = 'sse')), digits = 2)
round(rmse(pred16_1316, data1316$monthly_EXOfdom), digits = 1)
