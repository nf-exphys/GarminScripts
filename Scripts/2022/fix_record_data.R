
source("./Scripts/2022/periodic_fit_import.R")
#need to check on UTC to CST change in record data

rm(list = ls())

#Script to deal with record cleaning

  #anything older than 2021 is either too old or was collected differently


#To do

#Remove fractional_cadence column

#Find and correct elevation drop over bridges, maybe based on drops in elevation

#Make record data a rolling 5 sec average average

#Find lags for speed, HR, and elevation
#Lags should be 5, 10, 30 seconds long

#convert change in elevation to % grade

#Add column for stop events (stop == TRUE?) and for time since the last stop

#Add stats about distribution of speed or HR or grade to better characterize 

#Add column for accumulated time in Zone 1/2/3 based on HR and based on GAP


#General data cleaning: ignore non-running data
#Fix treadmill 