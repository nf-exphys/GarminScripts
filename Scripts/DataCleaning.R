library(tidyverse, warn.conflicts = F)
##### Load 2020 Data #####
  #Long term, I think calling this data from SQL is probably the way to go
  #So much of what I'm doing is dependent on relational data

#Read in any new files, save as CSV
source("./Scripts/Fit File Import From Watch V2.R")
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
recent_records <- lapply(recent_record_files_path, read_csv)

#read in lap data
recent_lap_files_path <- paste0(csv_file_path, recent_lap_files)
recent_laps <- lapply(recent_lap_files_path, read_csv)

#read in summary data
recent_sum_files_path <- paste0(csv_file_path, recent_sum_files)
recent_sum <- lapply(recent_sum_files_path, read_csv)

#Most of the record files have ID. Most of the lap files do too.
#Almost none of the summary files do.
#Some of the summary files have 88 columns, while some only have 43.
#It might just be easiest to redo all of it with more stringent QC

  map_dfr(recent_sum_files_path, read_csv, col_types = cols_only(
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

#If the ID is missing, make it again
#time_char <- sum_data %>% pull(timestamp); time_char <-  gsub("[[:punct:]]", "_", time_char)

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

#Add a duration variable




