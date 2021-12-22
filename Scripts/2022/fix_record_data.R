library(magrittr)
source("./Scripts/2022/periodic_fit_import.R")

#need to check on UTC to CST change in record data

rm(list = ls())

records <- list.files(path = "./Data/processed_fit/record/", pattern = "2021_", full.names = TRUE)
  #anything older than 2021 is either too old or was collected differently

#read in csv then duplicate key column
data <- lapply(FUN = function(x){read_csv(x) %>% mutate(key2 = key)}, 
               X = records)

all_data <- bind_rows(data, .id = "key2") %>% 
  rename(id = key2, latitude = position_lat,
         longitude = position_long)

#get rid of fractional cadence and fix time zone
all_data %<>% 
  select(-fractional_cadence) %>% 
  mutate(timestamp = lubridate::with_tz(timestamp, tzone = "America/Chicago"))

#To do

#Find and correct elevation drop over bridges, maybe based on drops in elevation

library(sf); library(spData) ## For `world`, an sf MULTIPOLYGON object

## Create an sf POINTS object
set.seed(0)
lat <- runif(10, -80, 80)
lon <- runif(10, -180, 180)
points <- expand.grid(lon, lat)  # Note that I reversed OP's ordering of lat/long
pts <- st_as_sf(points, coords=1:2, crs=4326)

## Find which points fall over land
ii <- !is.na(as.numeric(st_intersects(pts, world)))

## Check that it worked
plot(st_geometry(world))
plot(pts, col=1+ii, pch=16, add=TRUE)


#Make record data a rolling 5 sec average average

#Find lags for speed, HR, and elevation
#Lags should be 5, 10, 30 seconds long

#convert change in elevation to % grade

#Add column for stop events (stop == TRUE?) and for time since the last stop

#Add stats about distribution of speed or HR or grade to better characterize 

#Add column for accumulated time in Zone 1/2/3 based on HR and based on GAP


#General data cleaning: ignore non-running data
#Fix treadmill 