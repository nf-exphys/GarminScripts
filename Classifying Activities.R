library(tidyverse); library(ggplot2)
library(ssc) #https://cran.r-project.org/web/packages/ssc/vignettes/ssc.pdf
library(fastDummies) #need to dummy code
  #https://rdrr.io/cran/ssc/man/setred.html
library(anomalize) #for anomaly detection
library(caret)
library(tibbletime) #for creating tbl_time 

load(file = "FitDFCleanSort2020-10-13.Rdata")
load(file = "./Objects/FitDFCleanSort2020-08-24.Rdata")
data <- c(Fit.DF, Fit.DF2)

#Only for summary data because everything else has a variable number of lines
sumdat <- data.frame(matrix(NA, nrow=length(data), ncol = length(Fit.DF[[1]]$sum_data)))
names(sumdat) <- names(Fit.DF[[1]]$sum_data)
n <- length(data)
for (i in 1:n){
  sumdat[i,] <- add_column(data[[i]]$sum_data)
  }

sumdat$timestamp <- as.POSIXct(sumdat$timestamp.1, origin = "1970-01-01") #format time stamp correctly
sumdat$timestamp.1 <- as.POSIXct(sumdat$timestamp.1, origin = "1970-01-01")
sumdat$time_created <- as.POSIXct(sumdat$time_created, origin = "1970-01-01")

sumdat$act.day <- NA 
#start with blank slate for act.day, which says what type of day it was - easy, workout, long
sumdat$act.type <- NA 
#start with blank slate for act.type, which is specific to workouts 
#and describes workout type: repeats, tempo, warm-up, cool-down, etc. 

keyvar <- list("name", "time_created", "avg_heart_rate", "num_laps", 
               "TRIMP.hour", "TRIMP", "TimeMinutes","total_distance","avg_speed")
  #starting list of which variables might be key for determining activity type manually?

length(which(sumdat$name == "Run"))
length(which(sumdat$name == "Run Indoor"))
(815+357)/1403 #~84% of data is from running

name.dummy <- dummy_cols(sumdat$name) #Creates dummy variables for activity name
colnames(name.dummy) <- c("name", "unknown", 
      "Bike", "Bike Indoor", "Other", "Run", "Run Indoor") #renames dummy vars
sumdat <- cbind(sumdat[,-42],name.dummy) #removes original name column and adds back to the end along with dummy vars

for (i in 1:length(keyvar)){
  print(which(grepl(keyvar[i],colnames(sumdat)))) #find matches for keyvar in sumdat
}

label <- function(rownum,day,type=NA){ #creates function to label activities
  sumdat[rownum,]$act.day <- day
  sumdat[rownum,]$act.type <- type
  return(sumdat)
} #just need to make sure to set output of label to sumdat

#act.day: what type of day it was - easy, workout/race, long, lift, bike, Boele?
#act.type: specific to workouts, describes workout type: repeats, tempo, warm-up, cool-down, other, hills (short), etc.
  #If workout contains wu & cd, set act.type based on workout type
  #If workout contains one of the two plus the workout, set act.type based on workout type
  #If workout is set to lift, leave act.type blank
  
id <- which(grepl("2020-09-01", sumdat$timestamp)) 
  #modify date in quotes to see which activities were on which days
sumdat[id,c(94,65,8,44,84,83,69,76,9)] #returns helpful information for manual classification

sumdat <- label(1085, "")


#double check 2019-07-12 & 2019-07-13, 2019-07-18 (stopped for July 2019 after this)
#come back to 03-12 runs

length(which(is.na(sumdat$act.day)))

#Next steps: label more (~200?)
  #Apply annomalize here as a low level screen for outliers, might help find files with lots of outliers.
  #https://towardsdatascience.com/tidy-anomaly-detection-using-r-82a0c776d523
  #Apply ML methods to files with high number of outliers or high probability of outliers

#trying annomalize
sumdat.t <- cbind(data[[1398]]$record$timestamp, data[[1398]]$record[,1:8])
colnames(sumdat.t)[1] <- "timestamp"
sumdat.t <- sumdat.t %>% rownames_to_column() %>% as_tibble
#sumdat.t <- sumdat.t %>% separate(timestamp, into = c("date", "time"), sep = " ")
#sumdat.t$date <- as.Date(sumdat.t$date)
#sumdat.t$time <- format(as.POSIXct(sumdat.t$time), format = "%H:%M:%S")

sumdat.t <- sumdat.t %>% as_tbl_time(index = timestamp)
sumdat.t <- prep_tbl_time(sumdat.t)

sumdat.t %>%
  time_decompose(heart_rate, method = "STL", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  #time_recompose() %>% 
  plot_anomaly_decomposition()
rlang::last_error()
rlang::last_trace()

#Still stuck on anomalize, which may or may not actually work for my data
#Next time try https://cran.r-project.org/web/packages/timeSeries/timeSeries.pdf
#Or https://rdrr.io/rforge/IsolationForest/man/AnomalyScore.html
  #Some more info here: https://medium.com/@siddharth.suresh92/isolation-forest-for-data-mining-a2c44a26d646

#Maybe the best bet is to find anomalies, remove them, impute using kNN, and then smooth using 3 second rolling average?

#Trying isolation forests based on walk-through here: 
  #https://www.kaggle.com/norealityshows/outlier-detection-with-isolation-forest-in-r

library(solitude)#package for isolation forest
library(mice) #package for imputation
#create isolation forest using isolationForest function from solitude package with default parameters

data.test <- data[[1365]]$record #goldy's 10 mile
head(data.test$timestamp,5)
data.test$timestamp[5] + 120
#data.test[which(is.na(data.test$heart_rate)),] #time stamps where HR is NA, which happens sometimes?
if (length(which(is.na(data.test$heart_rate))) > 0) { #if there are NAs in HR
  miceMod <- mice(data.test, method = "rf", print = F) 
    #calculates values to impute using random forest, doesn't print output
  data.test <- complete(miceMod)
    #applies those values to data.test
  }
iforest<- isolationForest$new() #apply iforest algorithms
iforest$fit(data.test)

#predict outliers within dataset
data.test$pred <- iforest$predict(data.test)
data.test$outlier <- as.factor(ifelse(data.test$pred$anomaly_score >=0.6, "outlier", "normal")) 
  #arbitrarily setting score>0.6 as an outlier

t <- as.numeric(data.test[nrow(data.test),]$timestamp - data.test[1,]$timestamp) 
#sets t as the number of minutes in the activity
if (t > 10){ #sets first 2 minutes to normal because outlier detection can't account for slow component
  slowcomp <- data.test$timestamp[5] + 120 #add 2 minutes to outlier detection to account for slow component
  #The thinking for this is that with a long enough activity, it'll incorrectly flag the slow comp as an outlier
  #With shorter activities, hopefully that won't be a problem
  r <- which(data.test$timestamp == slowcomp)
  data.test$outlier[1:r] <- "normal" 
 }

ggplot(data.test, aes(x = timestamp, y = heart_rate, color = outlier)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend") #plot highlighting outliers

densityplot(data.test$pred$anomaly_score)
table(data.test$pred$anomaly_score) #come back to this - set the cutoff as the value with the top 1-2%, i.e. top 15 values in a 1500 row record
quantile(data.test$pred$anomaly_score)
density(data.test$pred$anomaly_score)
summary(data.test$pred$anomaly_score)
nrow(data.test[(which(data.test$pred$anomaly_score >=0.66)),c(3,5,8,9,11)])
data.test[1,c(3,5)]
class(test)







