library(tidyverse); library(timetk); library(tibbletime); library(tidyquant)
library(keras)

load(file = "FitDFCleanSort2020-10-13.Rdata")
load(file = "./Objects/FitDFCleanSort2020-08-24.Rdata")
data <- c(Fit.DF, Fit.DF2)

data.test <- data[[1373]]$record #have to do this for now because it throws an error if you feed mice a data frame
if (length(which(is.na(data.test$heart_rate))) > 0) { #if there are NAs 
  miceMod <- mice(data.test, method = "rf", print = F) 
  #calculates values to impute using random forest, doesn't print output
  data.test <- complete(miceMod)
  #applies those values to data.test
}
iforest<- isolationForest$new() #apply iforest algorithms
iforest$fit(data.test)

data.test$pred <- iforest$predict(data.test)
data.test$outlier <- as.factor(ifelse(data.test$pred$anomaly_score >=0.595, "outlier", "normal")) 
  #arbitrarily setting score>0.6 as an outlier

#Figure out where the HR data is accurate and where it's not
which(grepl("08:25:00", data.test$timestamp)) 
which(grepl("08:37:00", data.test$timestamp)) 
which(grepl("09:32:00", data.test$timestamp)) 

#plot the HR data to help decide
ggplot(data.test, aes(x = timestamp, y = heart_rate, color = outlier)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend") #plot highlighting outliers

#Only keep this data because HR monitor was accurate here
data.test <- data.test[931:4100,]

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
