library(tidyverse); library(lubridate); library(ggplot2)

summarydata <- data.frame(matrix(NA, nrow=1350, ncol = length(Fit.DF[[1]]$sum_data)))
names(summarydata) <- names(Fit.DF[[1]]$sum_data)
for (i in 1:1350){
 summarydata[i,] <- add_column(Fit.DF[[i]]$sum_data)
 }

summarydata$timestamp <- as.POSIXct(summarydata$timestamp.1, origin = "1970-01-01")
summarydata$timestamp.1 <- as.POSIXct(summarydata$timestamp.1, origin = "1970-01-01")
summarydata <- summarydata %>%
  select(c(8,9,33,36,37,42,45,49,56,61,70,71,73:78,80,82,84,85))

summarydata$miles <- summarydata$total_distance/1609.34

dates <- read.delim("clipboard") #Copy pasted dates, days, and VDOT in
  #Formats dates
dates$Start.date <- as.Date.character(dates$Start.date, "%m/%d/%Y")
dates$End.date <- as.Date.character(dates$End.date, "%m/%d/%Y")

filtdata <- data.frame(matrix(NA, nrow=13, ncol=6))
names(filtdata) <- c("start", "end", "miles", "timeMin", "trimp", "trimp.hr")

filtdata$start <- as.POSIXct(filtdata$start, origin = "1970-01-01")
filtdata$end <- as.POSIXct(filtdata$end, origin = "1970-01-01")

for (i in 1:13){
  filtdata[i,] <- summarydata %>% 
   subset(timestamp > dates[i,1] & timestamp < dates[i,2]) %>%
   summarise(start = dates[i,1], end = dates[i,2],
            miles = sum(miles), time = sum(TimeMinutes),
                trimp = sum(TRIMP), trimphour = sum(TRIMP.hour))
  #print(dates[i,1])
  #print(dates[i,2])
}




#Plotting

summarydataf = filter(summarydata, summarydata$TimeMinutes > 200 
                      | summarydata$TRIMP > 200 
                      | summarydata$max_heart_rate.1 > 195
                      | summarydata$avg_heart_rate > 180)
nrow(summarydataf)

qplot(x = TimeMinutes, y = TRIMP, 
      data = summarydataf
      , geom = "point")


qplot(x = TimeMinutes, y = TRIMP, 
      data = subset(summarydata, summarydata$TimeMinutes > 200 | summarydata$TRIMP > 175 
                    | summarydata$max_heart_rate.1 > 185
                    | summarydata$TRIMP > 200
      ), geom = "point")

min120 = summarydata[which(summarydata$TimeMinutes > 120),] 
#need to exclude run on 03-19-2019 where avg_heart_rate = 124
#need to manually examine runs with TRIMP > 200, max HR > 190, and avg heart rate > 180

TRIMP200 = summarydata[which(summarydata$TRIMP > 199),]
summarydata$distance_miles <- summarydata$total_distance/1609

TRIMPf = subset(summarydata, summarydata$TimeMinutes < 350)

plot(subset(summarydata, summarydata$TimeMinutes < 350), summarydata$TRIMP)
