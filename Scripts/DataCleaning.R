library(tidyverse, quietly = T)
##### Load 2020 Data #####
  #Long term, I think calling this data from SQL is probably the way to go
  #So much of what I'm doing is dependent on relational data
load(file = "./Objects/FitDFCleanSort2020-10-12.Rdata") #loads data
Fit.DF2 <- Fit.DF #renames to avoid being overwritten

load(file = "./Objects/FitDFCleanSort2020-08-24.Rdata") #loads data
data <- c(Fit.DF, Fit.DF2) #combines first two data loads
remove(Fit.DF, Fit.DF2) #to avoid confusion

load(file = "./Objects/FitDFCleanSort2020-12-18_Oct11_Oct31.Rdata") #loads data
Fit.DF3 <- Fit.DF #renames to avoid being overwritten

load(file = "./Objects/FitDFCleanSort2020-12-18_Oct31_Dec17.Rdata") #loads data
Fit.DF4 <- Fit.DF #renames for clarity

data <- c(data, Fit.DF3, Fit.DF4)
remove(Fit.DF, Fit.DF3, Fit.DF4)

##### Checking for Duplicates & Sequential Ordering #####
n <- length(data)
repeat_check <- tibble()
for (i in 1:n){
  repeat_check[i,1] <- as.POSIXct(data[[i]]$sum_data$start_time, origin = "1970-01-01")
  repeat_check[i,2] <- as.numeric(i)
}
colnames(repeat_check) <- c("start", "num")

setdiff(which(duplicated(repeat_check$start) == T), which(repeat_check$start=="1969-12-31 17:59:59"))
  #Compares duplicates in repeat_check to lines in repeat_check from manually created activities, which have date of 1969
  #If setdiff returns nothing, we know all of the repeats are due to manually created activities
  #If setdiff returns something, such as 1351, run this to remove it: data[1351] <- NULL

#Checking to make sure every item is uploaded in sequential order
repeat_check$progress <- as.logical(factor(ifelse(repeat_check$start > lag(repeat_check$start),'T','F')))
repeat_check %>% #outputs any files out of order
  filter(progress == F & start > "2020-07-01") #Checked and 1407 is an indoor ride, don't care *shrug*

#Discard data from before July 2020 due to high frequency of indoor running, possibly inaccurate HR data, etc. 
  #Other data is still worth keeping, just not for modeling, at least not right now
  #Eventually, might be worth going back through a couple of seasons with some racing to strengthen modeling
start_date <- repeat_check %>% filter(start > "2020-07-01") %>% head(1) %>% pull(num)
data[1:(start_date-1)] <- NULL
remove(repeat_check)

##### Keeping Only Running Data #####

#Initial screening
  #Printing out data items that don't match or that have HRV data
  #Ultimately removing names of data 
for (i in 1:length(data)){
  goal <- names(pluck(data, 1)) #standard for order of names
  check <- names(pluck(data, i)) #check for each one
  
  if (("hrv" %in% check) == T){ #check for HRV data
    print(paste0("hrv_", i))
  } else {ifelse(all(diff(match(goal, check)) > 0)==F, print(i), next)} 
  #if no HRV data, compare to goal and print i if it doesn't match
}

data[84:85] <- NULL #removed based on printed i above
  #Seems like the problem is Connect-created files

#Use sum_data to identify events that aren't running
#Somehow, the alphabetical sorting of names in sum_data didn't quite work for all of the data 
for (i in 1:length(data)){
  data[[i]]$sum_data <- data %>% 
    pluck(.,i,"sum_data") %>% #selecting sum_data for each item in data
    select(sort(names(.))) #sorting names alphabetically
    #Then re-writes sum data
}

sum_data <- data %>% #combine sum data into one big data frame
  map(., "sum_data") %>%
  bind_rows(.id = "data_index")

unique(sum_data$name) #shows options for name

not_running_in_data <- sum_data %>% #find events where the name isn't Run
  filter(name != "Run") %>%
  pull(data_index) %>%
  as.numeric()

length(data) #length before removing
data[c(not_running_in_data)] <- NULL
length(data) #length after removing

#Creating data frames for each type of data, with data_index as the # of each from data
  #If this is the way I'm going to go, it might be easier to just use all of the CSVs
event <- data %>% map(.,"event") %>%
  bind_rows(.id = "data_index")

lap <- data %>% map(.,"lap") %>%
  bind_rows(.id = "data_index")

device_info <- data %>% 
  map(.,"device_info") %>%
  bind_rows(.id = "data_index")

record <- data %>% map(.,"record") %>%
  bind_rows(.id = "data_index")

#Not ready for HRV data yet - need to figure out how to handle/process it first

##### Checking accuracy of HR data #####
 #split timestamp into date and time
split_date_time <- function(x){
  x %>%  mutate(
    date = as.Date(timestamp),
    time = format(timestamp, "%H:%M:%S")
  )
  
}
#applies function to each data frame
lap <- split_date_time(lap)
event <- split_date_time(event)
record <- split_date_time(record)
sum_data <- split_date_time(sum_data)
device_info <- split_date_time(device_info)

#Next step is going to have to be to make a function to give me a Garmin Connect Like output
  #Input date - might need to merge same date first or create warning if multiple activities on same date
  #Plot of HR, pace, table of key metrics from summary data, and lap-by-lap data
  #Then have some way to say "Yes, this is accurate", "This is worth imputing", or "No this is garbage"

p_hr <- record %>%
  filter(date == "2020-07-01") %>%
  ggplot(., aes(x=time, y=heart_rate, group=1)) + geom_path()

p_speed <- record %>%
  filter(date == "2020-07-01") %>%
  ggplot(., aes(x=time, y=speed, group=1)) + geom_path() + ylim(2,4)

p_sum <- sum_data %>%
  select(-caret::nearZeroVar(.)) %>% #removes near zero variance columns and gives meaningful info
  filter(date == "2020-07-01") %>%
  select(avg_heart_rate, avg_speed, max_heart_rate.1, TimeMinutes, total_distance) %>%
  gridExtra::tableGrob()

p_lap <- lap %>%
  select(-caret::nearZeroVar(.)) %>%
  filter(date == "2020-07-01") %>%
  select(avg_heart_rate, avg_speed, max_speed, total_distance, total_timer_time) %>%
  gridExtra::tableGrob()

gridExtra::grid.arrange(p_hr, p_speed,p_sum,p_lap, nrow=2, as.table=T, padding=T)
#Future ideas:

#Add variable elapsed time in record
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
  
#Compare day-before or 2-day-before TRIMP to morning HRV
    #Might get stronger correlations if you classify TRIMP into easy/medium/hard
    #Do the same thing for TRIMP/hr
