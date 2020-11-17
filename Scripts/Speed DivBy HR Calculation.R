library(tidyverse)

load(file = "./FitDFCleanSort2020-10-12.Rdata")
Fit.DF2 <- Fit.DF
load(file = "./Objects/FitDFCleanSort2020-08-24.Rdata")
data <- c(Fit.DF, Fit.DF2)
data <- data[-1350] #delete repeat file
remove(Fit.DF, Fit.DF2)

times <- data[[1]]$lap$timestamp
times[4]
n <- length(times)
for (i in 1:n){
  z <- which(data[[1]]$record$timestamp == times[i])
  #add something about length of z >0, else don't mark it as a meaningful event?
  #might want to use start_time instead of timestamp
  }

