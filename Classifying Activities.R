library(tidyverse); library(lubridate); library(ggplot2)
library(ssc) #https://cran.r-project.org/web/packages/ssc/vignettes/ssc.pdf
library(fastDummies) #need to dummy code
  #https://rdrr.io/cran/ssc/man/setred.html

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

label <- function(rownum,day,type="NA"){ #creates function to label activities
  sumdat[rownum,]$act.day <- day
  sumdat[rownum,]$act.type <- type
  return(sumdat)
} #just need to make sure to set output of label to sumdat

#act.day: what type of day it was - easy, workout, long, lift, bike
#act.type: specific to workouts, describes workout type: repeats, tempo, warm-up, cool-down, other, etc.
  #If workout contains wu & cd, set act.type based on workout type
  #If workout contains one of the two plus the workout, set act.type based on workout type
  #If workout is set to lift, leave act.type blank
  
id <- which(grepl("2019-07-12", sumdat$timestamp)) 
  #modify date in quotes to see which activities were on which days
sumdat[id,c(94,65,8,44,84,83,69,76,9)] #returns helpful information for manual classification

sumdat <- label(718, "easy")
#start here with 2019-07-13



