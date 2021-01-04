library(tidyverse); library(mice); library(zoo)

load(file = "./FitDFCleanSort2020-10-12.Rdata")
Fit.DF2 <- Fit.DF
load(file = "./Objects/FitDFCleanSort2020-08-24.Rdata")
data <- c(Fit.DF, Fit.DF2)
data <- data[-1350] #delete repeat file
data.test <- data[[1373]]$record #have to do this for now because it throws an error if you feed mice a data frame
if (length(which(is.na(data.test$heart_rate))) > 0) { #if there are NAs 
  miceMod <- mice(data.test, method = "rf", print = F) 
  #calculates values to impute using random forest, doesn't print output
  data.test <- complete(miceMod)
  #applies those values to data.test
}
data.test <- data.test %>% #adds column with 15 second altitude difference
  mutate(delta.alt = diff(zoo(data.test$altitude), lag = 15, na.pad=TRUE))
data.test$delta.alt[1:15] <- 0 #set NAs created by diff to 0, mice doesn't work otherwise

library(solitude)
iforest<- isisolationForest$new() #apply iforest algorithms
iforest$fit(data.test)

data.test$pred <- iforest$predict(data.test)
data.test$outlier <- as.factor(ifelse(data.test$pred$anomaly_score >=0.595, "outlier", "normal")) 
  #arbitrarily setting score>0.6 as an outlier

#find outliers after 3 minutes, set them to NA, and then apply MICE to predict them
startime <- data.test$timestamp[1]
outliers <- which(data.test$outlier == "outlier" & data.test$timestamp > (startime+180))
compdata <- data.test[,c(5, 9:11)] #make a copy of HR, timestamp, pred, and outlier for comparison
data.test$heart_rate[outliers] <- NA
data.test <- data.test[, -which(names(data.test) %in% c("pred", "outlier"))]
miceMod <- mice(data.test, method = "rf", print = F) 
  #calculates values to impute using random forest, doesn't print output
data.test <- complete(miceMod)
  #applies those values to data.test

#Compare imputed HR values to original HR values
compdata <- cbind(compdata, data.test$heart_rate)
colnames(compdata)[5] <- "miceHR"
compdata %>% mutate(diff = heart_rate - miceHR)
ggplot(data = compdata[outliers,], aes(timestamp)) + 
  geom_point(aes(y=heart_rate), colour = "red") + #plots original values for outliers in red
  geom_point(aes(y=miceHR), colour = "blue") #and imputed values in blue
#Verifies anomalies can be reasonably corrected


##### Old code #####
data.test <- data.test %>%  tk_tbl() %>% as_tbl_time(index = timestamp)
tk_t
data.test %>%
  filter_time("start" ~ "9:00") %>%
  ggplot(aes(index, heart_rate)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "1749 to 1800 (Zoomed In To Show Cycle)",
    caption = "datasets::sunspot.month"
  )
which(is.na(data.test[,1:9]))
data.test[2174,]
#https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/
