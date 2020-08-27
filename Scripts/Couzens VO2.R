#Calculate Couzens VO2
#Data will already be screened for outdoor running and HRM

#Formula: VO2 = (210/pace)/((THR-RHR)/(MHR-RHR)) where THR = HR of session, MHR = 195 bpm, rest HR = 48 bpm or RHR from the morning HRV?

#Calculate on lap-by-lap basis assuming lap time > 5 minutes, total activity time > 45 minutes (activity$total_timer_timer > 2700 sec), lap elevation gain/loss <=55 feet
  #If total activity time > 2700, continue. If not, skip activity & fill VO2 column w/NA
  #If lap time > 5 and elevation gain/loss <= 55 feet, calculate VO2 for the lap and record. If not, record NA

library(fitFileR); library(tidyverse);
loadfile <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles\\A7493133.fit"
GarminFile <- readFitFile(loadfile)
