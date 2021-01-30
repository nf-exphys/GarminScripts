library(tidyverse, warn.conflicts = F)
##### Load 2020 Data #####
  #Long term, I think calling this data from SQL is probably the way to go
  #So much of what I'm doing is dependent on relational data

#Read in any new files, save as CSV
source("./Scripts/Fit File Import From Watch.R")
remove(list = ls())

source("./Scripts/HRV During Exercise.R")
remove(list = ls())

#SQL integration is finicky, reading in CSVs for now
csv_file_path <- "./Data/ExportedRunData/Cleaned_CSVs/"

#list all record, lap, and summary files
all_record_files <- list.files(path = csv_file_path, pattern = "* record.csv")
all_lap_files <- list.files(path = csv_file_path, pattern = "* lap.csv")
all_sum_files <- list.files(path = csv_file_path, pattern = "* sumdata.csv")

#finds files from 2020 OR 2021
recent_record_files <- grep("2020|2021", all_record_files, value = T)
recent_lap_files <- grep("2020|2021", all_lap_files, value = T)
recent_sum_files <- grep("2020|2021", all_sum_files, value = T)

#Removes unneeded files
remove(all_record_files, all_lap_files, all_sum_files)

#read in record data
recent_record_files_path <- paste0(csv_file_path, recent_record_files)
recent_records <- map_dfr(recent_record_files_path, read_csv) %>%
  select(-fractional_cadence)

#read in lap data
recent_lap_files_path <- paste0(csv_file_path, recent_lap_files)
recent_laps <- map_dfr(recent_lap_files_path, read_csv, col_types= cols(
  .default = col_double(), ID = col_datetime(format = ""),
    event = "c", event_type = "c", lap_trigger = "c",
    sport = "c", start_time = col_datetime(format = ""),
    sub_sport = "c", timestamp = col_datetime(format = "")
    ))

#read in summary data
recent_sum_files_path <- paste0(csv_file_path, recent_sum_files)
recent_sum <- map_dfr(recent_sum_files_path, read_csv, col_types = cols_only(
  ID = col_datetime(format = ""),
  avg_heart_rate = "d", avg_speed = "d",
  max_heart_rate = "d", max_speed = "d",
  nec_lat = "d", nec_long = "d", num_laps = "d", sport = "c", sub_sport = "c",
  start_position_lat = "d", start_position_long = "d",
  start_time = col_datetime(format = ""),
  swc_lat = "d", swc_long = "d",
  TimeMinutes = "d", timestamp = col_datetime(format = ""),
  total_descent = "d", total_distance = "d",
  total_elapsed_time = "d", total_timer_time = "d"))

rm(list = ls.str(mode = 'character')) #clears out lots of the non-DF objects

#Get rid of useless lap columns by searching for near zero variance
nzv_in_lap <- caret::nearZeroVar(recent_laps)
recent_laps <- recent_laps[-nzv_in_lap] %>%
  select(-total_moving_time) #only had 4 values total

#Fix lat & long
recent_laps <- recent_laps %>% mutate(
  start_position_lat = start_position_lat/(2^32 / 360),
  end_position_lat = end_position_lat/(2^32 / 360),
  start_position_long = start_position_long/(2^32 / 360),
  end_position_long = end_position_long/(2^32 / 360)
  )

recent_records <- recent_records %>% mutate(
  position_lat = position_lat/(2^32 / 360),
  position_long = position_long/(2^32 / 360)
)

#summary data
lat_cols <- grep(colnames(recent_sum), pattern = "lat")
long_cols <- grep(colnames(recent_sum), pattern = "long")

position_cols <- c(lat_cols, long_cols)
recent_sum[,position_cols] <- recent_sum[,position_cols]/(2^32 / 360)

#clears out lots of the non-DF objects
rm(list = ls.str(mode = 'character')) 
rm(list = ls.str(mode = 'integer')) 

#split timestamp into date and time
split_date_time <- function(x){
  x %>%  mutate(
    date = as.Date(timestamp, tz="America/Chicago"),
    time = format(timestamp, "%H:%M:%S"),
    .keep = "unused"
  )
  
}

recent_laps <- split_date_time(recent_laps)
recent_records <- split_date_time(recent_records)
recent_sum <- split_date_time(recent_sum)

#Discard data from before July 2020 due to high frequency of indoor running, possibly inaccurate HR data, etc. 
  #Other data is still worth keeping, just not for modeling, at least not right now
  #Eventually, might be worth going back through a couple of seasons with some racing to strengthen modeling
  #It's also probably easier to work with a smaller dataset initially
recent_sum <- recent_sum %>% filter(date > "2020-07-01")
recent_records <- recent_records %>% filter(date > "2020-07-01")
recent_laps <- recent_laps %>% filter(date> "2020-07-01")

##### Keeping Only Running Data #####

only_running_IDs <- recent_sum %>% 
  filter(sport == "running") %>% #only running
  filter(sub_sport == "generic") %>% #not going to train on treadmill data
  pull(ID)

recent_sum <- subset(recent_sum, ID %in% only_running_IDs)
recent_laps <- subset(recent_laps, ID %in% only_running_IDs)
recent_records <- subset(recent_records, ID %in% only_running_IDs)


##### Checking accuracy of HR data #####

library(gridExtra); library(svDialogs)

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
  filter(date == my_date) %>% #filter by date
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

my_date <- select_date(mm=10, yyyy=2020, dd=09)
recent_sum <- summary_plots(my_date)
recent_sum %>% select(ID, contains("hr_"), data_index) %>% write_csv(file = "./Data/hr_acc_sum_data.csv")
 #Merges data from the same date together, isn't always perfect but a decent start
  

#### Future ideas: ####

#Add elapsed time as a variable in record
  #Would make plotting easier
  #Might also make prediction of HR data easier, since the model won't understand time of day as well as elapsed time
  #Tough part would be syncing it to lap data and other timestamps. Might need some SQL join logic for that

#Merge runs from the same day unless separated by 4 ish hours
  #Not sure what I'd do with the data index here... 
  #Best idea might be to keep first one but then create another variable with all of them stored as a list

#Create lagged elevation (10 seconds?) 
  #Would need to account for crossing the river on Dinkytown greenway lol
  #Long-term this could be a way to create grade-adjusted pace using Minetti equations
  #for now it would just be helpful to track HR response

#Take accurate HR data and divide into train/test for NN to see if I can predict HR given pace/lagged elevation
#Take accurate HR data and try to impute HR data that was marked as worth imputing
#If I'm able to get corrected HR data, I should re-calculate TRIMP
  
#### Compare day-before or 2-day-before TRIMP to morning HRV ####
  #Might get stronger correlations if you classify TRIMP into easy/medium/hard
  #Do the same thing for TRIMP/hr
recent_sum %>% filter(TRIMP <0) 
nrow(recent_sum)
hist(recent_sum$TRIMP, breaks = 30)

#combine recent_sum by date
#long-term it'd be better to do this with record data
#this just looks at Fall 2020 season 

merge_recent_sum <- recent_sum %>% as_tibble() %>%
  filter(start_time > "2020-09-06") %>%
  select(date, TimeMinutes, TRIMP, TRIMP.hour, avg_heart_rate, max_heart_rate.1) %>%
  #group_by(date) %>%
  summarise(date = date,
    minutes = TimeMinutes,
    TRIMP_sum = TRIMP*TimeMinutes/60,
    TRIMP_hr_sum = TRIMP.hour*TimeMinutes/60,
    avg_HR = avg_heart_rate*TimeMinutes/60, 
    max_HR = max_heart_rate.1*TimeMinutes/60) %>%
  group_by(date) %>%
  summarise(
    #date = date,
    across(everything(), sum) #might artificially inflate TRIMP of multi-hour activities? 
  )

#source("Test HRV package.R")
colnames(timeanalysis)

merge_hrv_data <- timeanalysis %>%
  separate(col = datetime, into = c("date", "time"), sep = " ") %>%
  group_by(date) %>%
  select(date, time, rMSSD, lnRMSSD) %>%
  mutate(date = lubridate::as_date(date)) %>%
  filter(date > "2020-09-06")

hrv_recent_sum <- merge(x=merge_recent_sum, y=merge_hrv_data, all.x = T)

hrv_recent_sum %>% janitor::get_dupes(date)

View(hrv_recent_sum)
hist(hrv_recent_sum$max_HR)

hrv_recent_sum <- hrv_recent_sum %>%
  mutate(
    lead1 = lead(rMSSD),
    lead2 = lead(rMSSD, 2),
    lead3 = lead(rMSSD, 3),
    lead4 = lead(rMSSD, 4),
    lead5 = lead(rMSSD, 5)
         )

library(colorspace)
hrv_recent_sum %>%
  #filter(TRIMP_sum < 150) %>% #realistic values 
ggplot(data = ., aes(x=minutes, y=lead2)) + geom_point(aes(alpha = 0.3)) + geom_smooth(method = "loess") + 
  ylab("rMSSD Two Days Later") + xlab("Minutes Run")


#### Speed Div By HR ####
#Note as of 01/29/2021: Questionable accuracy
  #May be more useful after HR correction or identifying steady state data
# ggplot(data = recent_records, aes(x = date)) +geom_histogram(bins = 30) #histogram of # of rows roughly by month
# speed_cat <- recent_records$speed %>% cut(.,
#                                   breaks = c(0,3.0,3.2512, 3.576, 4.235, 4.733, 5.548, Inf),
#                                   include.lowest = T,
#                                   labels = c("recovery","slow", "easy", "medium","tempo-5K", "5K-mile", "mile&faster")
# )
# recent_records <- cbind(recent_records, speed_cat)
# #recovery is slower than 9:00
# #9:00-8:15 is slow
# #8:15 - 7:30 is easy
# #7:30 - 6:20 is medium
# #6:20 - 5:40 is tempo-5K
# #5:40 - 4:50 is 5K-mile
# 
# speed_HR_data <- recent_records %>% 
#   arrange(date) %>%
#   mutate(date = as.POSIXct(substr(date, 1,10)),
#          #goes up with decreased HR@same speed or increased speed@same HR
#         speed.HR = (speed*3.6/(heart_rate/100)) 
#     
#   ) %>% #converts speed to km/hr & divides HR by 100 for easier numbers
#   group_by(date, speed_cat) %>%
#   summarise(speed.HR = mean(speed.HR)) #takes forever but works
# 
# speed_HR_data %>% 
#   filter(date > "2020-09-01") %>%
#   filter(speed_cat == "tempo-5K" | speed_cat == "medium") %>%
#   #subset(.,speed_cat == "tempo-5K") %>%
#   ggplot(data = ., aes(x = date, y=speed.HR, colour = speed_cat)) + geom_smooth() + ylim(6,16)
# #ggplot(data = ., aes(x = dmy, y=speed.HR)) + geom_smooth() + ylim(8.5,10) #removes category, just looks at all speed/HR data
# 
# #Broader speed categories - still need to finish this
# speed_cat2 <- recent_records$speed %>% cut(.,
#                                    breaks = c(0,3.2512, 3.576, 4.235, 5.36, Inf),
#                                    include.lowest = T,
#                                    labels = c("rec/slow", "easy", "medium","tempo-~2mile", "TooFastForHR")
# )#5.36 is 5:00 min/mile
# recent_records <- cbind(recent_records, speed_cat2)
# 
# speed_HR_data <- recent_records %>% 
#   arrange(date) %>%
#   drop_na(heart_rate, speed) %>%
#   mutate( #date = as.POSIXct(substr(date, 1,10)),
#          speed.HR = (speed*3.6/(heart_rate/100))
#   ) %>% 
#   #group_by(date) %>%
#   group_by(date, speed_cat2) %>%
#   summarise(speed.HR = mean(speed.HR)) #takes forever but works
# 
# speed_HR_data %>% 
#   filter(date > "2020-07-01") %>%
#   filter(speed_cat2 != "rec/slow") %>%
#   #subset(.,speed_cat == "tempo-5K") %>%
#   ggplot(data = ., aes(x = date, y=speed.HR, colour = speed_cat2)) + geom_smooth() + ylim(9,11)
# 
# speed_HR_data %>%
#   filter(date > "2020-12-01") %>%
# ggplot(data = ., aes(x = date, y=speed.HR)) + geom_point() + ylim(8.5,10) #removes category


