#Correct HR data
library(tidyverse); library(outForest)

#read in data
source("./Scripts/DataCleaning.R")
hr_acc_sum_data <- read_csv("./Data/hr_acc_sum_data.csv")

#combine the two
recent_sum <- merge(x=recent_sum, y=hr_acc_sum_data, by = "ID")
remove(hr_acc_sum_data)

#Figure out how much data needs to be imputed
table(recent_sum$hr_all_acc)
table(recent_sum$hr_impute)

#Split record by date
record <- recent_records %>%
  group_split(date)

#Make list of dates in record
dates_in_record <- unique(recent_records$date)

#Dates to impute
ID_to_impute <- recent_sum %>%
  filter(hr_impute == "yes") %>%
  pull(date) %>% unique(.)

#Adds outlier column
recent_records <- recent_records %>%
  add_column(is_outlier = NA) %>%
  mutate(is_outlier = as.character(is_outlier))

#Setup function for plotting HR & pace
summary_plots <- function(my_date){
  
  if(nrow(recent_records %>% filter(date == my_date)) == 0){
    print("No summary data for this date")
    break}
  
  #Condensing record data for the specific day into 3 second intervals to improve plotting
  record_by_threes <- recent_records %>% 
    filter(date == my_date) %>% 
    mutate(threes = (1:nrow(.) %/% 3)+1) %>% #not quite sure what this %/% does, but it seems to work
    #adding 1 so I can use threes in place of time
    group_by(threes) %>% #group data into sets of 3 seconds
    summarise(speed = mean(speed), #average data over the 3 seconds
              heart_rate=as.integer(mean(heart_rate))) %>%
    round(., 2) %>% mutate(threes = threes *3)
  
  p_hr <- record_by_threes %>%
    ggplot(., aes(x=threes, y=heart_rate, group=1)) + geom_path(color = 'red') + xlab("time (3 sec int.)")
  
  p_speed <- record_by_threes %>%
    mutate(speed_min.mi = 60/(speed * 2.23694)) %>% #convert to min/mi
    filter(speed_min.mi < 15) %>% #get rid of outliers
    ggplot(., aes(x=threes, y=speed_min.mi, group=1)) + geom_path(color = 'blue') + 
    scale_y_continuous(trans = "reverse") + xlab("time (3 sec int.)")
  
  p_sum <- recent_sum %>%
    select(-caret::nearZeroVar(.)) %>% #removes near zero variance columns and gives meaningful info
    filter(date == my_date) %>%
    select(avg_heart_rate, avg_speed, total_distance, TimeMinutes) %>%
    mutate(avg_speed = 60/(avg_speed * 2.23694), total_distance = total_distance/1609.34) %>%
    rename(dist = total_distance, avg_HR=avg_heart_rate) %>% 
    round(., 2) %>%
    gridExtra::tableGrob()
  
  p_lap <- recent_laps %>%
    select(-caret::nearZeroVar(.)) %>%
    filter(date == my_date) %>%
    select(avg_heart_rate, avg_speed, total_distance, total_timer_time) %>%
    mutate(avg_speed = 60/(avg_speed * 2.23694),
           lap_time = (total_timer_time/60), #convert to minutes
           total_distance = total_distance/1609.34) %>% #convert to miles
    rename(dist = total_distance, avg_HR=avg_heart_rate) %>% 
    select(-total_timer_time) %>% #get rid of total timer time
    round(., 2)
  
  if(nrow(p_lap) > 8){
    p_lap <- bind_rows(head(p_lap, 6), tail(p_lap, 2))
  } #limiting the size of the lap tables for easier viewing
  
  p_lap <- tableGrob(p_lap) #convert shrunk-down table to plotable object
  
  print(grid.arrange(p_hr,p_sum,p_speed,p_lap,  #return plots in 2x2 format
                     nrow=2, as.table=T, padding=T))
  
} 

#Make plots
my_date <- ID_to_impute[3]
summary_plots(my_date)

key_date <- which(dates_in_record == ID_to_impute[20])

my_outliers <- record[[key_date]] %>%
  #filter(date == ID_to_impute[1]) %>%
  select(heart_rate, altitude, cadence, speed, distance) %>%
  outForest(data = ., heart_rate ~ ., threshold = 2, seed = 123)

summary(my_outliers)
#outliers(my_outliers)
outlier_rows <- my_outliers$outliers$row

record[[key_date]]$is_outlier[outlier_rows] <- T
record[[key_date]]$is_outlier[-outlier_rows] <- F

# recent_laps %>%
#   filter(date == ID_to_impute[1])
#   filter(.,grep(pattern == "2020-07-08", start_time))

x_labels <- record[[key_date]]$time[seq(1, length(record[[key_date]]$time), by = 360)]

record[[key_date]] %>%
  mutate(heart_rate = (heart_rate/10),
         speed_min.mi = 60/(speed * 2.23694)) %>%
ggplot(data = .) +  
       geom_point(aes(x=time, y=heart_rate, color = is_outlier)) +
       geom_point(aes(x=time, y=speed_min.mi)) + 
  scale_x_discrete(breaks = x_labels) +
  ylim(0,20) + ggtitle(unique(record[[key_date]]$date))

#record[[key_date]] %>% filter(time < "04:55:00")
