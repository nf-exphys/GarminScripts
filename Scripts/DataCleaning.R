library(tidyverse, warn.conflicts = F)
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
  #If setdiff returns something, such as 1351, run this to remove it: 
data[1351] <- NULL

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
#HRV data in 218 to 245
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

useless_cols <- sum_data %>% 
  lapply(., unique) %>% #finds number of unique values per column
  lengths(., use.names = T) %>% #finds length of each item in the list
  as.data.frame() %>% #converts to d.f.
  rownames_to_column() %>% #adds row name(which is the previous column name)
  rename(num_unique=".") %>% #fixes column name
  filter(num_unique == 1) %>% pull(rowname) #outputs list of columns where # unique = 1

sum_data <- sum_data %>% select(-c(useless_cols))

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
    date = as.Date(timestamp, tz="America/Chicago"),
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
library(gridExtra); library(svDialogs)

#Could be good to add background shading to plot to show HR zones 
  #https://stackoverflow.com/questions/61161960/how-do-i-add-shaded-backgrounds-to-ggplot

#adds accuracy and impute columns, fills with NAs
sum_data <- sum_data %>% add_column(hr_all_acc=NA, hr_impute=NA)

select_date <- function(mm,dd,yyyy){
  select_date <- paste0(yyyy, "-", mm, "-", dd)
  return(select_date)
} #needed date in the Global Environment
  #so I wrote a separate function for it
summary_plots <- function(my_date){
  
#Condensing record data for the specific day into 3 second intervals to improve plotting
record_by_threes <- record %>% 
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
  
p_sum <- sum_data %>%
  select(-caret::nearZeroVar(.)) %>% #removes near zero variance columns and gives meaningful info
  filter(date == my_date) %>%
  select(avg_heart_rate, avg_speed, total_distance, TimeMinutes) %>%
  mutate(avg_speed = 60/(avg_speed * 2.23694), total_distance = total_distance/1609.34) %>%
  rename(dist = total_distance, avg_HR=avg_heart_rate) %>% 
  round(., 2) %>%
  gridExtra::tableGrob()

p_lap <- lap %>%
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

Sys.sleep(5) #wait 10 seconds to give me time to look at the plots

#Then display pop-up box asking about HR accuracy
hr_all_acc_value <- dlgInput("Is all HR data accurate? Enter 'yes' or 'no'", default="yes")$res

Sys.sleep(5) #wait 5 sec to give me time to answer

#if HR data isn't accurate, prompt about imputing
hr_impute_value <- NA
if (hr_all_acc_value == "no"){ 
hr_impute_value <- dlgInput("Should this HR data be imputed? Enter 'yes' or 'no'", default="no")$res
} 

#Add information to sum_data

#gives data_index value for row in sum_data
add_HR_info_here <- sum_data %>% filter(date == d) %>% pull(data_index) 

  #if data hasn't already been entered for rows of sum_data being plotted
if(all(is.na(sum_data[add_HR_info_here,] %>% select(hr_all_acc, hr_impute))) == T){
  #sets input of dialog box to hr_all_acc & hr_impute for specific rows
  sum_data[add_HR_info_here,]$hr_all_acc <- hr_all_acc_value
  sum_data[add_HR_info_here,]$hr_impute <- hr_impute_value
} else{print(
    paste0("Data has already been written for columns with data index of:", 
           add_HR_info_here))}

return(sum_data)
} 

my_date <- select_date(12, 10, 2020)
sum_data <- summary_plots(my_date) 
  #pretty slow, doesn't work great when there are lots of laps
  #Merges data from the same date together, isn't always perfect
  #Doesn't work returning sum_data to Global Environment


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
  
#Compare day-before or 2-day-before TRIMP to morning HRV
  #Might get stronger correlations if you classify TRIMP into easy/medium/hard
  #Do the same thing for TRIMP/hr
sum_data %>% filter(TRIMP <0) 
nrow(sum_data)
hist(sum_data$TRIMP, breaks = 30)

#combine sum_data by date
#long-term it'd be better to do this with record data
#this just looks at Fall 2020 season 

merge_sum_data <- sum_data %>% as_tibble() %>%
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

hrv_sum_data <- merge(x=merge_sum_data, y=merge_hrv_data, all.x = T)

hrv_sum_data %>% janitor::get_dupes(date)

View(hrv_sum_data)
hist(hrv_sum_data$max_HR)

hrv_sum_data <- hrv_sum_data %>%
  mutate(
    lead1 = lead(rMSSD),
    lead2 = lead(rMSSD, 2),
    lead3 = lead(rMSSD, 3),
    lead4 = lead(rMSSD, 4),
    lead5 = lead(rMSSD, 5)
         )

library(colorspace)
hrv_sum_data %>%
  #filter(TRIMP_sum < 150) %>% #realistic values 
ggplot(data = ., aes(x=minutes, y=lead2)) + geom_point(aes(alpha = 0.3)) + geom_smooth(method = "loess") + 
  ylab("rMSSD Two Days Later") + xlab("Minutes Run")
