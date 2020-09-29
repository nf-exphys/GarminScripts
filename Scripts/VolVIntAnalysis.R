library(tidyverse); library(lubridate); library(ggplot2)

#First, load most recent copy of FitDF

summarydata <- data.frame(matrix(NA, nrow=1350, ncol = length(Fit.DF[[1]]$sum_data)))
names(summarydata) <- names(Fit.DF[[1]]$sum_data)
for (i in 1:1350){
 summarydata[i,] <- add_column(Fit.DF[[i]]$sum_data)
 }

remove(Fit.DF) #Isn't used from here on out

summarydata$timestamp <- as.POSIXct(summarydata$timestamp.1, origin = "1970-01-01")
summarydata$timestamp.1 <- as.POSIXct(summarydata$timestamp.1, origin = "1970-01-01")
summarydata <- summarydata %>%
  select(c(8,9,33,36,37,42,45,49,56,61,70,71,73:78,80,82,84,85))

summarydata$miles <- summarydata$total_distance/1609.34

dates <- read.delim("clipboard") #Copy pasted dates, days, and VDOT in from Racing VDOT
  #Formats dates
save(dates, file = "//GarminData//Objects//VolIntDates1")
dates$Start.date <- as.Date.character(dates$Start.date, "%m/%d/%Y")
dates$End.date <- as.Date.character(dates$End.date, "%m/%d/%Y")

filtdata <- data.frame(matrix(NA, nrow=13, ncol=12))
names(filtdata) <- c("start", "end", "miles", "timeMin", "trimp", "trimp.hr", "days", "miles30d", "timeMin30d", "trimp30d", "trimp.hr30d", "VDOT")

filtdata$start <- as.POSIXct(filtdata$start, origin = "1970-01-01")
filtdata$end <- as.POSIXct(filtdata$end, origin = "1970-01-01")

for (i in 1:13){
  filtdata[i,] <- summarydata %>% 
   subset(timestamp > dates[i,1] & timestamp < dates[i,2]) %>%
   summarise(start = dates[i,1], end = dates[i,2],
            miles = sum(miles), time = sum(TimeMinutes),
                trimp = sum(TRIMP), trimphour = sum(TRIMP.hour))
 
}

filtdata[,7:12] <- NA #Clear out the for-loop data
filtdata$days <- dates$Days.in.the.block
filtdata$VDOT <- dates$VDOT
filtdata$miles30d <- filtdata$miles/filtdata$days*30
filtdata$trimp30d <- filtdata$trimp/filtdata$days*30
filtdata$trimp.hr30d <- filtdata$trimp.hr/filtdata$days*30
filtdata$timeMin30d <- filtdata$timeMin/filtdata$days*30
