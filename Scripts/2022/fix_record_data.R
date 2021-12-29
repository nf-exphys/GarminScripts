library(magrittr)
source("./Scripts/2022/periodic_fit_import.R")

rm(list = ls())

records <- list.files(path = "./Data/processed_fit/record/", pattern = "2021_", full.names = TRUE)
  #anything older than 2021 is either too old or was collected differently

#read in csv then duplicate key column
data <- lapply(FUN = function(x){read_csv(x, show_col_types = FALSE) %>% mutate(key2 = key)}, 
               X = records)

all_data <- bind_rows(data, .id = "key2") %>% 
  rename(id = key2, latitude = position_lat,
         longitude = position_long)

#get rid of fractional cadence and fix time zone
#then make percent incline and rate of change columns
all_data %<>% 
  select(-fractional_cadence) %>% 
  mutate(timestamp = lubridate::with_tz(timestamp, tzone = "America/Chicago")) %>% 
  relocate(longitude, latitude, timestamp) %>% #switch lat/long order
  group_by(key) %>% 
  mutate(elev_grade = round((altitude - lag(altitude,5)) / (distance - lag(distance,5)) * 100, 3),
         elev_rate_of_change = round((elev_grade - lag(elev_grade,5)) / (lag(elev_grade,5) * 100), 3)) 

#Key is the number associated with the Fit file
#Id is the number associated with the index in data as the files were read in

#Examples: 
#451 is lino easy run
#452 is hill repeats
#405 has run near river


#altitude data in fit file doesn't match Garmin Connect  
#need to correct with USGS data

library(elevatr); library(sp); library(sf)
sf_data <- all_data %>% 
  ungroup() %>% 
  drop_na(longitude, latitude) %>% #throws error if NA in coordinates
  relocate(longitude, latitude) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

test <- get_elev_point(sf_data, src = "epqs") #EPQS data matches with Garmin

#This might be helpful
# st_as_sf(points, coords=1:2, crs=4326)

# https://rdrr.io/cran/elevatr/man/get_epqs.html

#before plotting, get only the unique latitude and longitude coordinates 
  #to avoid repeats


# interactive map:
library(mapview)
mapview(sf_data)

test <- get_epqs(all_data, units = "meters", ncpu = future::availableCores() - 3, serial = FALSE)

class(sf_data)

library(slider)

test_smooth <- test %>% 
  select(time_elapsed, distance, elevation, speed, key, id, heart_rate, cadence) %>% #get relevant columns
  mutate(elevation = slide_min(elevation, before = 10, after = 5, step = 1)) %>% #attempt to correct errors in elevation data
  mutate(roll_elev = slide_mean(elevation, before = 10, after = 0, step = 1), #smooth data a bit before using to find grade
         roll_dist = slide_mean(distance, before = 10, after = 0, step = 1)) %>% #smooth data a bit before using to find grade
  mutate(rise = (roll_elev - lag(roll_elev,3)), #needed to find grade
         run = (distance - lag(distance,3))) %>% #needed to find grade
  mutate(elev_grade = round((rise / run) * 100, 3)) %>% #calculates grade
  mutate(elev_grade = slide_mean(elev_grade, before = 5, after = 0, step = 1)) %>%  #smooths grade
  mutate(delta_elev = elevation - lag(elevation, 1)) #simple change in elevation for comparison
  # mutate(run_corr = run / 10) %>% 
  # mutate(elev_grade_corrected = round(((rise / run_corr)/10) * 100, 3)) %>% #calculates grade with constant distance
  # mutate(elev_grade_corrected = slide_mean(elev_grade_corrected, before = 5, after = 0, step = 1)) #smooths grade


ggplot(data = test_smooth, aes(x = time_elapsed)) + 
  geom_line(aes(y = elevation - elevation[1]), color = "blue") +
  # geom_line(aes(y = elev_grade_corrected), color = "purple") + 
  geom_line(aes(y = elev_grade), color = "green") +
  geom_line(aes(y = delta_elev*10), color = "red") +
  ggtitle("Green is Grade, Purple is Corected Grade, Blue is Elevation") + 
  ylim(-20, 20)



  geom_line(aes(y = elevation), color = "green")

library(streamMetabolizer)
all_data$latitude[100]
all_data$longitude[100]

lookup_usgs_elevation(-93.231, 44.984, units = "Meters")

#To do

#Find and correct elevation drop over bridges, maybe based on drops in elevation

#Find and correct speed/distance around tracks
  #First figure out what a track oval looks like with lat/long data
  #Maybe need to add a column that has direction (N/S/E/W) using lag col?
  #Then find some way to determine probability of it being a semi-circle?



#Make record data a rolling 5 sec average average

#Find lags for speed, HR, and elevation
#Lags should be 5, 10, 30 seconds long

#convert change in elevation to % grade

#Add column for stop events (stop == TRUE?) and for time since the last stop

#Add stats about distribution of speed or HR or grade to better characterize 

#Add column for accumulated time in Zone 1/2/3 based on HR and based on GAP


#General data cleaning: ignore non-running data
#Fix treadmill 