#Calculate TRIMP from One Off File

load("C:/Users/Nick.Foreman/Desktop/Nick Mobile Folders/School/2020-2021/R Files/GarminData/FitDF.Rdata")
library(fitFileR); library(tidyverse)
n <- 1352 #Length is wrong on load file, might need to manually adjust for NULLs in 1352-1355

#Puts all data in one variable
for (i in 1:n) {
  
    Fit.DF[[i]]$activity <- cbind(Fit.DF[[i]]$activity, Fit.DF[[i]]$session$total_distance)
    Fit.DF[[i]]$activity$totaldistance <- Fit.DF[[i]]$activity$`Fit.DF[[i]]$session$total_distance`    
    Fit.DF[[i]]$activity <- Fit.DF[[i]]$activity[-c(12)]  
}

head(Fit.DF[[100]]$activity,5)
ActivityDF <- bind_rows(Fit.DF[[1]]$activity, Fit.DF[[2]]$activity, Fit.DF[[3]]$activity)
ActivityDF <- bind_rows(Fit.DF[[1:3]]$activity)

ActivityList <- vector('list', 5)

for (i in 1:5){
  #ActivityDFTest <- bind_rows(Fit.DF[[i]]$activity)
  ActivityList[[i]] <- select(pluck(Fit.DF, i)$activity, 1:12)names(Fit.DF[[i]]$activity) <- make.unique(names(Fit.DF[[i]]$activity))
  #HERE
  ActivityDFTest <- flatten_dfr(ActivityList)
}
rlang::last_error()
rlang::last_trace()
#Distance >0
#Time > 5 min
