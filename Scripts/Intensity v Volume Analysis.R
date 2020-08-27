#Intensity vs. Volume Analysis

#Criteria for inclusion:
  #HRM connected
  #Running, either indoor or outdoor
  #Activity time > 5 minutes

#Goal is to get one data frame with the time, distance, speed, and TRIMP metrics
  #Still need to pull HR, distance, and speed metrics from session data frame
  #Also add column about run vs run indoor

library(fitFileR); library(tidyverse)

if (exists("Fit.DF") == TRUE) {print("Almost good to go")}
if (exists("FitRecord") == TRUE) {print("Good to go")}

#Create empty vector to add VolvInt data to
VolInt <- vector('list', n)

for (i in 1:n) {

  if (Fit.DF[[i]]$sport$name == "Run") { #If the activity type is run, 
    select(pluck(Fit.DF, i)$activity, 1:11)
    select(pluck(Fit.DF, i)$session, )  #START HERE, see if you can use select to pick columns by name instead of by number
  }
  
  if (Fit.DF[[i]]$sport$name == "Run Indoor") { #If the activity type is run indoor
  }
  
  if (any(grepl("antplus", Fit.DF[[6]]$device_info$source_type)) == TRUE) { 
    #If source type contains any antplus (presumably means HRM is connected?)... 
  }  
  
  if (Fit.DF[[i]]$activity$TimeMinutes > 5) {
    #If activity time is > 5 minutes
  }
}

