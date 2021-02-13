
##### Checking accuracy of HR data #####

library(tidyverse, warn.conflicts = F); library(mapview)
library(gridExtra); library(svDialogs)

source("DataCleaning.R")

#Could be good to add background shading to plot to show HR zones 
#https://stackoverflow.com/questions/61161960/how-do-i-add-shaded-backgrounds-to-ggplot

#Take old summary data stored in different format and add it to recent_sum
old_sum <- read_csv(file = "./Data/hr_acc_sum_data.csv")
hr_all_acc <- as.vector(old_sum$hr_all_acc); length(hr_all_acc) <- nrow(recent_sum)
hr_impute <- as.vector(old_sum$hr_impute); length(hr_impute) <- nrow(recent_sum)
recent_sum <- cbind(recent_sum, hr_all_acc, hr_impute)

recent_sum <- recent_sum %>% mutate(data_index = row_number())

#Find out the last spot I left off (the first NA)
recent_sum[min(which(is.na(recent_sum$hr_all_acc))),]$date

select_date <- function(mm,dd,yyyy){
  select_date <- paste0(yyyy, "-", mm, "-", dd)
  return(select_date)
} #needed date in the Global Environment
#so I wrote a separate function for it

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
  
  # p_map <-  recent_records %>% 
  #   filter(date == my_date) %>% 
  #   select(contains("position")) %>%
  #   mutate(n=row_number()) %>%
  #   filter(row_number() %% 20 == 0) %>%
  # mapview(., xcol = "position_long", ycol="position_lat", crs=4269, grid=F)
  
  # lat_long_bounds <- recent_records %>% 
  #   filter(date == my_date) %>%
  #   summarise(max_lat = max(position_lat)+0.1,
  #             max_long = max(position_long)+0.1,
  #             min_lat = min(position_lat)-0.1,
  #             min_long = min(position_long)-0.1) %>% 
  #   as.numeric()
  # 
  # bbox <- c(left = lat_long_bounds[4], right = lat_long_bounds[2],
  #           top = lat_long_bounds[1], bottom = lat_long_bounds[3]
  #           )
  # myMap <- get_stamenmap(bbox, maptype = "terrain", zoom = 12)
  # 
  # p_map <- ggmap(myMap) + geom_point(aes(x=position_long, 
  #                                        y=position_lat), data = map_data)
  
  if(nrow(p_lap) > 8){
    p_lap <- bind_rows(head(p_lap, 6), tail(p_lap, 2))
  } #limiting the size of the lap tables for easier viewing
  
  p_lap <- tableGrob(p_lap) #convert shrunk-down table to plotable object
  
  #print(p_map); Sys.sleep(5)
  
  print(grid.arrange(p_hr,p_sum,p_speed,p_lap,  #return plots in 2x2 format
                     nrow=2, as.table=T, padding=T))
  
  Sys.sleep(5) #wait 5 sec for plots to appear
  
  #Then display pop-up box asking about HR accuracy
  hr_all_acc_value <- dlgInput("Is all HR data accurate? Enter 'yes' or 'no'", default="yes")$res
  
  #if HR data isn't accurate, prompt about imputing
  hr_impute_value <- NA
  if (hr_all_acc_value == "no"){ 
    hr_impute_value <- dlgInput("Should this HR data be imputed? Enter 'yes' or 'no'", default="no")$res
  } 
  
  #Add information to recent_sum
  
  #gives data_index value for row in recent_sum
  add_HR_info_here <- recent_sum %>% filter(date == my_date) %>% pull(data_index) 
  
  #if data hasn't already been entered for rows of recent_sum being plotted
  if(all(is.na(recent_sum[add_HR_info_here,] %>% select(hr_all_acc, hr_impute))) == T){
    #sets input of dialog box to hr_all_acc & hr_impute for specific rows
    recent_sum[add_HR_info_here,]$hr_all_acc <- hr_all_acc_value
    recent_sum[add_HR_info_here,]$hr_impute <- hr_impute_value
  } else{print(
    paste0("Data has already been written for columns with data index of:", 
           add_HR_info_here))}
  
  return(recent_sum)
} 

#Imputation question addresses whether simple imputation would fix HR artifacts
#Anything more than simple HR fix -> select "no"

my_date <- select_date(mm=12, yyyy=2020, dd=31)
recent_sum <- summary_plots(my_date)
recent_sum %>% select(ID, contains("hr_"), data_index) %>% write_csv(file = "./Data/hr_acc_sum_data.csv")
#Merges data from the same date together, isn't always perfect but a decent start