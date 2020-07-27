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


data <- read.csv("./Data/variables_all_pluslag_fDOM_daily_final.csv")

data1316 <- data %>% 
  select(Date, daily_EXO_fdom, fdom_daily_ARlag1, daily_EXO_chla_ugL_ZT, daily_EXO_do_persat_ZT,
           daily_EXO_wtr_temp_ZT,  daily_rain_mm_ZT, daily_SRup_mean_ZT, WRT_days_daily_ZT) %>% 
  mutate(Date = as.Date(Date))

data1316 <- na.omit(data1316) #removes all rows w/ NAs that prevent model from running 
head(data1316)


# build a global model with the selected variables and then use dredge to see which combinations have the lowest AICc values
model_fdom <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT +
                    daily_EXO_wtr_temp_ZT +  daily_rain_mm_ZT  + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')


glm_1316 <- dredge(model_fdom, rank = "AICc", fixed = "fdom_daily_ARlag1") #scroll through glm_1316 to get lag only model

##Lines 52-59 used to created table with all models in supplement 
# export <- glm_1316
# export <- round(export, digits = 3)
# 
# export[is.na(export)] <- 0  #sets NA to 0's  
# export$equation <- paste(export$`(Intercept)`, export$fdom_daily_ARlag1, export$daily_EXO_chla_ugL_ZT, export$daily_EXO_do_persat_ZT,
#                          export$daily_EXO_wtr_temp_ZT, export$daily_rain_mm_ZT, export$daily_SRup_mean_ZT, export$WRT_days_daily_ZT,
#                          sep = "() + ")
#write.csv(export, "./Data/Table_all_Daily_models_withequationformat.csv" , row.names = FALSE)

select_1316 <- subset(glm_1316, delta<2 )



mod1_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')


mod2_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')


mod3_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + daily_EXO_chla_ugL_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')


mod4_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + daily_EXO_chla_ugL_ZT +
                   daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')

mod5_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + daily_EXO_do_persat_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')

mod6_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')

modARonly_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1,
                 data = data1316, family = gaussian, na.action = 'na.fail')


# make predictions with the models
pred1_1316 <- predict(mod1_1316, newdata = data1316)
pred2_1316 <- predict(mod2_1316, newdata = data1316)
pred3_1316 <- predict(mod3_1316, newdata = data1316)
pred4_1316 <- predict(mod4_1316, newdata = data1316)
pred5_1316 <- predict(mod5_1316, newdata = data1316)
pred6_1316 <- predict(mod6_1316, newdata = data1316)
predARonly_1316 <- predict(modARonly_1316, newdata = data1316)



# plot the predictions for the 2014 training dataset
plot(data1316$Date, data1316$daily_EXO_fdom, type = 'l', ylab = "fDOM (QSU)", xlab = "Date", pch = 16)
points(data1316$Date, pred1_1316, col = 'dodgerblue', type = 'l')
points(data1316$Date, pred2_1316, col = 'orange', type = 'l')
points(data1316$Date, pred3_1316, col = 'gold', type = 'l')
points(data1316$Date, pred4_1316, col = 'blue', type = 'l')
points(data1316$Date, pred5_1316, col = 'pink', type = 'l')
points(data1316$Date, pred6_1316, col = 'purple', type = 'l')
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
points(pred6_1316, col = 'purple', type = 'l')
title("Selected models fDOM dataset")

# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_1316, type = 'sse')), digits = 2)
round((rsq(mod2_1316, type = 'sse')), digits = 2)
round((rsq(mod3_1316, type = 'sse')), digits = 2)
round((rsq(mod4_1316, type = 'sse')), digits = 2)
round((rsq(mod5_1316, type = 'sse')), digits = 2)
round((rsq(mod6_1316, type = 'sse')), digits = 2)
round((rsq(modARonly_1316, type = 'sse')), digits = 2)


round(rmse(pred1_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred2_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred3_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred4_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred5_1316, data1316$daily_EXO_fdom), digits = 1)
round(rmse(pred6_1316, data1316$daily_EXO_fdom), digits = 1)
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


par(mfrow = c(2,1))

plot(data1316$Date, data1316$daily_EXO_fdom, type = 'l', ylab = "fDOM (QSU)", 
     #xlab = "Date", 
     pch = 16, xaxt='n')
     #, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
points(data1316$Date, pred1_1316, col = 'dodgerblue', type = 'l', lty = "dashed")
title("Selected model for daily fDOM")
legend("topright", legend=c("observed", "predicted"),
       col=c("black", "dodgerblue"), lty=1:2, cex= 0.7)


daily_selected <- ggplot()+
  geom_line(data = binded_daily, aes(x = Date, y = daily_EXO_fdom, color = "Observed"), size = 0.5)+
  geom_line( data = binded_daily, aes(x = Date, y = pred1_1316, color = "Predicted"), linetype = "dashed", size = 0.5)+
  labs(x = "Date",
       y = "fDOM (QSU)")+
  #ggtitle("Daily fDOM Concentration")+
  #ylim(c(0,0.17))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
   theme(axis.title.x=element_blank(),
   axis.text.x=element_blank())+
  scale_color_manual("Model", limits=c("Observed", "Predicted"), values = c("black","blue")) +
  guides(colour = guide_legend(override.aes = list(pch = c(20, 25), fill = c("black", "blue"), size = 4)))+
  theme(legend.position = c(0.85, 0.85))+
  mytheme_AS
daily_selected


plot(binded_daily$Date, binded_daily$resid, type = 'p', ylab = "Residual", xlab = "Date", pch = 16)
title("Residual of Observed - Predicted Daily fDOM")
# legend("topright", legend=c("observed", "predicted"),
#        col=c("black", "dodgerblue"), lty=1:2, cex= 0.7)


daily_resid <- ggplot()+
  geom_point(data = binded_daily, aes(x = Date, y = resid), color = "black", size = 1)+
  labs(x = "Date",
       y = "Residual (QSU)")+
  #ggtitle("Daily fDOM Concentration")+
  #ylim(c(0,0.17))+
  #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
  #theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  # theme(axis.title.x=element_blank(),
  # axis.text.x=element_blank())+
  #scale_color_manual("Model", limits=c("Observed", "Predicted"), values = c("black","blue")) +
  #guides(colour = guide_legend(override.aes = list(pch = c(20, 25), fill = c("black", "blue"), size = 4)))+
  #theme(legend.position = c(0.85, 0.85))+
  mytheme_AS
daily_resid



##Residual stats 
mean(binded_daily$resid, na.rm = TRUE)  ## mean resid = -8.96 x 10^-16
sd(binded_daily$resid, na.rm = TRUE)  ## sd resid = 0.80 

nov_april_resid <- binded_daily %>% 
  filter(Date > "2018-10-31",
         Date < "2019-05-01")
mean(nov_april_resid$resid, na.rm = TRUE)  ## mean resid = - 0.001
sd(nov_april_resid$resid, na.rm = TRUE)  ## sd resid = 0.56

oct_resid <- binded_daily %>% 
  filter(Date >= "2018-10-01",
         Date < "2018-11-01")
mean(oct_resid$resid, na.rm = TRUE)  ## mean resid = 0.11
sd(oct_resid$resid, na.rm = TRUE)  ## sd resid = 0.86

may_sep_resid <- binded_daily %>% 
  filter(Date >= "2019-05-01")
mean(may_sep_resid$resid, na.rm = TRUE)  ## mean resid = - 0.012
sd(may_sep_resid$resid, na.rm = TRUE)  ## sd resid = 1.01



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
model_fdom_W <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOchla_ugL_ZT + monthly_EXOdo_persat_ZT +
                    monthly_rain_mm_ZT + WRT_days_monthly_ZT,
                  family = gaussian, na.action = 'na.pass', data = data1316_wrt)

glm_1316_W <- dredge(model_fdom_W, rank = "AICc", fixed = "fdom_monthly_ARlag1") #scroll through glm_1316 to get lag only model
select_1316_W <- subset(glm_1316_W, delta<2 )




# build the two individual models selected from dredge, These are the four different rows created in select_1316

##models, preds, and stats for TEMP
{
  mod1_1316_T <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOwtr_temp_ZT,
                     data = data1316_temp, family = gaussian, na.action = 'na.fail')
  
  
  mod2_1316_T <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_EXOwtr_temp_ZT + monthly_EXOdo_persat_ZT + monthly_rain_mm_ZT,
                     data = data1316_temp, family = gaussian, na.action = 'na.fail')
  
  mod3_1316_T <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1,
                     data = data1316_temp, family = gaussian, na.action = 'na.fail')
  
  pred1_1316_T <- predict(mod1_1316_T, newdata = data1316_temp)
  pred2_1316_T <- predict(mod2_1316_T, newdata = data1316_temp)
  pred3_1316_T <- predict(mod3_1316_T, newdata = data1316_temp)
  
  round((rsq(mod1_1316_T, type = 'sse')), digits = 2)
  round((rsq(mod2_1316_T, type = 'sse')), digits = 2)
  round((rsq(mod3_1316_T, type = 'sse')), digits = 2)
  
  
  round(rmse(pred1_1316_T, data1316_temp$monthly_EXOfdom), digits = 1)
  round(rmse(pred2_1316_T, data1316_temp$monthly_EXOfdom), digits = 1)
  round(rmse(pred3_1316_T, data1316_temp$monthly_EXOfdom), digits = 1)
  
  plot(data1316_temp$Date, data1316_temp$monthly_EXOfdom, type = 'l', ylab = "fDOM (QSU)", xlab = "Date")
  points(data1316_temp$Date, pred1_1316_T, col = 'dodgerblue', type = 'l')
  points(data1316_temp$Date, pred2_1316_T, col = 'orange', type = 'l')
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
    #ylim(c(0,0.17))+
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
    #ylim(c(0,0.17))+
    #scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) +
    #theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
    # theme(axis.title.x=element_blank(),
    # axis.text.x=element_blank())+
    #scale_color_manual("Model", limits=c("Observed", "Predicted"), values = c("black","blue")) +
    #guides(colour = guide_legend(override.aes = list(pch = c(20, 25), fill = c("black", "blue"), size = 4)))+
    #theme(legend.position = c(0.85, 0.85))+
    mytheme_AS
  monthly_resid
  
  
}

##models, preds, and stats for WRt
{
  mod1_1316_W <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1,
                     data = data1316_wrt, family = gaussian, na.action = 'na.fail')
  
  
  mod2_1316_W <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + WRT_days_monthly_ZT,
                     data = data1316_wrt, family = gaussian, na.action = 'na.fail')
  
  mod3_1316_W <- glm(monthly_EXOfdom ~ fdom_monthly_ARlag1 + monthly_rain_mm_ZT,
                     data = data1316_wrt, family = gaussian, na.action = 'na.fail')
  
  pred1_1316_W <- predict(mod1_1316_W, newdata = data1316_wrt)
  pred2_1316_W <- predict(mod2_1316_W, newdata = data1316_wrt)
  pred3_1316_W <- predict(mod3_1316_W, newdata = data1316_wrt)
  
  round((rsq(mod1_1316_W, type = 'sse')), digits = 3)
  round((rsq(mod2_1316_W, type = 'sse')), digits = 3)
  round((rsq(mod3_1316_W, type = 'sse')), digits = 3)
  
  
  round(rmse(pred1_1316_W, data1316_wrt$monthly_EXOfdom), digits = 3)
  round(rmse(pred2_1316_W, data1316_wrt$monthly_EXOfdom), digits = 3)
  round(rmse(pred3_1316_W, data1316_wrt$monthly_EXOfdom), digits = 3)
  
  plot(data1316_wrt$Date, data1316_wrt$monthly_EXOfdom, type = 'l', ylab = "fDOM (QSU)", xlab = "Date")
  points(data1316_wrt$Date, pred1_1316_W, col = 'dodgerblue', type = 'l')
  points(data1316_wrt$Date, pred2_1316_W, col = 'orange', type = 'l')
  points(data1316_wrt$Date, pred3_1316_W, col = 'red', type = 'l')
  title("Selected model for monthly fDOM")
  
}







#####  Final model selected and resid plot  #####

# fig5 <- ggarrange(daily_selected, monthly_selected, daily_resid, monthly_resid,
#                      labels = c("a", "b", "c", "d"),
#                      ncol = 2, nrow = 2)

 fig5 <- plot_grid(daily_selected, monthly_selected, daily_resid, monthly_resid,
                        labels = c("a", "b", "c", "d"),
                        ncol = 2, nrow = 2, align="hv")

#drivers <- (DO_ts | SR_ts | temp_ts | chla_ts) / (inf_ts | rain_ts | WRT_ts)

fig5

ggsave(filename = "./Images/ARmodels_and_residuals.tiff", 
       fig5, device = "tiff", width = 230, height = 140, units = "mm")




#### Getting R2 and RMSE for all daily models ####

mod1_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred1_1316 <- predict(mod1_1316, newdata = data1316)

mod2_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred2_1316 <- predict(mod2_1316, newdata = data1316)


mod3_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + daily_EXO_chla_ugL_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred3_1316 <- predict(mod3_1316, newdata = data1316)


mod4_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + daily_EXO_chla_ugL_ZT +
                   daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred4_1316 <- predict(mod4_1316, newdata = data1316)


mod5_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + daily_EXO_do_persat_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred5_1316 <- predict(mod5_1316, newdata = data1316)


mod6_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred6_1316 <- predict(mod6_1316, newdata = data1316)


mod7_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +  
                   daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred7_1316 <- predict(mod7_1316, newdata = data1316)


mod8_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred8_1316 <- predict(mod8_1316, newdata = data1316)


mod9_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred9_1316 <- predict(mod9_1316, newdata = data1316)


mod10_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred10_1316 <- predict(mod10_1316, newdata = data1316)


mod11_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred11_1316 <- predict(mod11_1316, newdata = data1316)


mod12_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred12_1316 <- predict(mod12_1316, newdata = data1316)


mod13_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred13_1316 <- predict(mod13_1316, newdata = data1316)


mod14_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred14_1316 <- predict(mod14_1316, newdata = data1316)


mod15_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred15_1316 <- predict(mod15_1316, newdata = data1316)


mod16_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_wtr_temp_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred16_1316 <- predict(mod16_1316, newdata = data1316)


mod17_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred17_1316 <- predict(mod17_1316, newdata = data1316)


mod18_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred18_1316 <- predict(mod18_1316, newdata = data1316)


mod19_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred19_1316 <- predict(mod19_1316, newdata = data1316)


mod20_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                   daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred20_1316 <- predict(mod20_1316, newdata = data1316)


mod21_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred21_1316 <- predict(mod21_1316, newdata = data1316)


mod22_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred22_1316 <- predict(mod22_1316, newdata = data1316)


mod23_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred23_1316 <- predict(mod23_1316, newdata = data1316)


mod24_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred24_1316 <- predict(mod24_1316, newdata = data1316)


mod25_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + WRT_days_daily_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred25_1316 <- predict(mod25_1316, newdata = data1316)


mod26_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred26_1316 <- predict(mod26_1316, newdata = data1316)


mod27_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred27_1316 <- predict(mod27_1316, newdata = data1316)


mod28_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT,
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred28_1316 <- predict(mod28_1316, newdata = data1316)


mod29_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT  + 
                    daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred29_1316 <- predict(mod29_1316, newdata = data1316)


mod30_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + 
                    daily_EXO_wtr_temp_ZT +  WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred30_1316 <- predict(mod30_1316, newdata = data1316)


mod31_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_EXO_wtr_temp_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred31_1316 <- predict(mod31_1316, newdata = data1316)


mod32_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred32_1316 <- predict(mod32_1316, newdata = data1316)


mod33_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred33_1316 <- predict(mod33_1316, newdata = data1316)

mod34_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + daily_rain_mm_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred34_1316 <- predict(mod34_1316, newdata = data1316)

mod35_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_SRup_mean_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred35_1316 <- predict(mod35_1316, newdata = data1316)

mod36_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred36_1316 <- predict(mod36_1316, newdata = data1316)

mod37_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred37_1316 <- predict(mod37_1316, newdata = data1316)

mod38_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_rain_mm_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred38_1316 <- predict(mod38_1316, newdata = data1316)

mod39_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_SRup_mean_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred39_1316 <- predict(mod39_1316, newdata = data1316)

mod40_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + 
                    daily_EXO_wtr_temp_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred40_1316 <- predict(mod40_1316, newdata = data1316)

mod41_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 +  daily_EXO_do_persat_ZT + 
                    daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred41_1316 <- predict(mod41_1316, newdata = data1316)

mod42_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_rain_mm_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred42_1316 <- predict(mod42_1316, newdata = data1316)

mod43_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                    daily_rain_mm_ZT + daily_SRup_mean_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred43_1316 <- predict(mod43_1316, newdata = data1316)

mod44_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred44_1316 <- predict(mod44_1316, newdata = data1316)

mod45_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred45_1316 <- predict(mod45_1316, newdata = data1316)

mod46_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + daily_rain_mm_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred46_1316 <- predict(mod46_1316, newdata = data1316)

mod47_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                    daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred47_1316 <- predict(mod47_1316, newdata = data1316)

mod48_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred48_1316 <- predict(mod48_1316, newdata = data1316)

mod49_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1  + daily_EXO_do_persat_ZT + 
                    daily_rain_mm_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred49_1316 <- predict(mod49_1316, newdata = data1316)

mod50_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred50_1316 <- predict(mod50_1316, newdata = data1316)

mod51_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT +  
                    daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred51_1316 <- predict(mod51_1316, newdata = data1316)

mod52_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                    daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred52_1316 <- predict(mod52_1316, newdata = data1316)

mod53_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                    daily_EXO_wtr_temp_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred53_1316 <- predict(mod53_1316, newdata = data1316)

mod54_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + 
                     daily_rain_mm_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred54_1316 <- predict(mod54_1316, newdata = data1316)

mod55_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred55_1316 <- predict(mod55_1316, newdata = data1316)

mod56_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_rain_mm_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred56_1316 <- predict(mod56_1316, newdata = data1316)

mod57_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_rain_mm_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred57_1316 <- predict(mod57_1316, newdata = data1316)

mod58_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_rain_mm_ZT + daily_SRup_mean_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred58_1316 <- predict(mod58_1316, newdata = data1316)

mod59_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_do_persat_ZT + 
                    daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred59_1316 <- predict(mod59_1316, newdata = data1316)

mod60_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                    daily_rain_mm_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred60_1316 <- predict(mod60_1316, newdata = data1316)

mod61_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                    daily_rain_mm_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred61_1316 <- predict(mod61_1316, newdata = data1316)

mod62_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_rain_mm_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred62_1316 <- predict(mod62_1316, newdata = data1316)

mod63_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_rain_mm_ZT + daily_SRup_mean_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred63_1316 <- predict(mod63_1316, newdata = data1316)

mod64_1316 <- glm(daily_EXO_fdom ~ fdom_daily_ARlag1 + daily_EXO_chla_ugL_ZT + daily_EXO_do_persat_ZT + 
                    daily_EXO_wtr_temp_ZT + daily_rain_mm_ZT + WRT_days_daily_ZT,
                  data = data1316, family = gaussian, na.action = 'na.fail')
pred64_1316 <- predict(mod64_1316, newdata = data1316)


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
