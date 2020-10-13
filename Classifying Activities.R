library(tidyverse); library(lubridate); library(ggplot2)
library(ssc)
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

sumdat$timestamp <- as.POSIXct(sumdat$timestamp.1, origin = "1970-01-01")
sumdat$timestamp.1 <- as.POSIXct(sumdat$timestamp.1, origin = "1970-01-01")

which(grepl("2020-10", sumdat$timestamp))
sumdat$act.type <- NA

