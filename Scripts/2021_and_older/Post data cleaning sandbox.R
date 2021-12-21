

#Plotting location of a run
library(mapview)
recent_records %>%
  filter(position_lat > 44.5 & position_lat < 45.1) %>%
  filter(position_long > -93.5 & position_long < -93)%>%
  filter(date > "2021-01-01")%>%
  mapview(., xcol = "position_long", ycol="position_lat", crs=4269, grid=F)

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


