library(tidyverse)

load(file = "./FitDFCleanSort2020-10-12.Rdata")
Fit.DF2 <- Fit.DF
load(file = "./Objects/FitDFCleanSort2020-08-24.Rdata")
data <- c(Fit.DF, Fit.DF2)
data <- data[-1350] #delete repeat file
remove(Fit.DF, Fit.DF2)
n <- length(data)

#Approach 1: Put lap start and end times in nested list, find speed/HR based on lap averages
start_times <- data[[1]]$lap$start_time
end_times <- data[[1]]$lap$timestamp
m <- length(start_times)
times <- list()
i=1
j=1
for (i in 1:m){
  y <- which(data[[j]]$record$timestamp == start_times[i])
  z <- which(data[[j]]$record$timestamp == end_times[i])
 if (length(z) < 1){
    z <- tail(data[[j]]$record$timestamp,1)
 }
 if (i==1) {
   times[[i]] <- list("start","end")
   times[[i]][["start"]] <- c(y)
   times[[i]][["end"]] <- c(z)
  }
 if (i>1){
   times[[i]] <- list("start","end")
   times[[i]][["start"]] <- append(times[[i]][["start"]], y)
   times[[i]][["end"]] <- append(times[[i]][["end"]], z)
  }
}

#Approach 2: Just look at speed/HR for each lap across all data points. 
  #Probably not as accurate, might work for proof of concept 
record <- data.frame()

for (i in 1:n){

#Checks for running and outdoors
{run_check <- FALSE; outdoor_check <- FALSE
if (data[[i]]$sum_data$sport == "running"){
  run_check <- TRUE
}
position_at_half_nrow <- data[[i]]$record$position_lat[as.integer(0.5*nrow(data[[i]]$record))+1]
if (position_at_half_nrow > 0 | is.na(position_at_half_nrow) == T){
  outdoor_check <- TRUE
  #if it happens to get an NA, assume it's an outdoor run
}
}
  
if (run_check == T & outdoor_check == T){

  if (i == 1){
    record <- data[[i]]$record
  }
  if (i > 1) {
    record <- rbind(record, data[[i]]$record)
  }
}

else {next} 

}
ggplot(data = record, aes(x = timestamp)) +geom_histogram(bins = 30) #histogram of # of rows roughly by month
speed_cat <- record$speed %>% cut(.,
  breaks = c(0,3.0,3.2512, 3.576, 4.235, 4.733, 5.548, Inf),
  include.lowest = T,
  labels = c("recovery","slow", "easy", "medium","tempo-5K", "5K-mile", "mile&faster")
)
record <- cbind(record, speed_cat)
#recovery is slower than 9:00
#9:00-8:15 is slow
#8:15 - 7:30 is easy
#7:30 - 6:20 is medium
#6:20 - 5:40 is tempo-5K
#5:40 - 4:50 is 5K-mile

ggplot(data = record, aes(x=timestamp, y=heart_rate, colour = speed_cat)) + geom_point() #crashes R
speed_HR_data <- record %>% 
  arrange(timestamp) %>%
  mutate(dmy = as.POSIXct(substr(timestamp, 1,10)),
         speed.HR = (speed*3.6/(heart_rate/100)) #goes up with decreased HR@same speed or increased speed@same HR
         ) %>% #converts speed to km/hr & divides HR by 100 for easier numbers
  group_by(dmy, speed_cat) %>%
  summarise(speed.HR = mean(speed.HR)) 

speed_HR_data %>% 
  filter(dmy > "2018-01-01") %>%
  #subset(speed_HR_data,speed_cat == "5K-mile") %>%
ggplot(data = ., aes(x = dmy, y=speed.HR, colour = speed_cat)) + geom_smooth() + ylim(6,16)
  


table(record$speed_cat)

