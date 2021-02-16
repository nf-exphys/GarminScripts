library(RHRV); library(tidyverse)

# Import Data ---------------------------------------------------

source("./Scripts/Import HRV from email.R") 
remove(list = ls())

DateChrTxt <- list.files(path = "./HRV Files to be Read/", pattern = ".txt", full.names = FALSE) 
#For use in naming/setting datetime field

DateChrNoTxt <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(DateChrTxt)) 
#removes file extension

FilesToRead <- list.files(path = ".\\HRV Files to be Read", pattern = ".txt", full.names = TRUE)
#Creates a list with the HRV files and their file path (better than pasting in the file path)

#Sets n as the number of files to be read
n <- length(FilesToRead)

#Read in files from FilesToRead; convert read-in file to a data frame and store it in a list; 
for (i in 1:n) {
  HRVdata <- CreateHRVData() #Create data structure to store HRV data
  
  #Converting yyyy-mm-dd to dd/mm/yyyy HH:MM:SS
  splitDC <- unlist(str_split(string = DateChrNoTxt[i], pattern = "[:punct:]|[:blank:]")) 
  #splits DateChr at spaces and punctuation then applies unlist to fix output
  d <- paste0(splitDC[c(3,2,1)], collapse = "/") #date as dd/mm/yyyy
  t <- paste0(splitDC[c(4,5,6)], collapse = ":") #time as HH:MM:SS
  dt <- paste0(c(d,t), collapse = " ")
  
  #loads RR data
  HRVdata <- LoadBeatRR(HRVdata, RecordName = DateChrTxt[i], scale = 0.001,
                        RecordPath = ".\\HRV Files to be Read", datetime = dt)
  
  #estimate reading duration
  max_time <- max(HRVdata$Beat$Time)
  
  HRVdata <- BuildNIHR(HRVdata)
  #interprets RR data into HR
  
  HRVdata <- FilterNIHR(HRVdata, minbpm = 30, maxbpm = 100)
  #filters within physiological ranges
  
  HRVtime <- CreateTimeAnalysis(HRVdata, size = 5) #performs simple time analysis with size set to default
  if (length(HRVdata$Beat$Time) > 400){ #if there are lots of rows (i.e. it's exercise not resting)
    print(i) 
    #next #skips over copying data into dataframe
  }
  if (i == 1){ #the first time through, just make the data frame
    timeanalysis <- cbind(as.data.frame(HRVtime$datetime),
                          as.data.frame(HRVtime$TimeAnalysis), 
                          as.data.frame(max_time))
  }
  if (i>1){ #after the first time, create a temporary storage timeanalysis.new 
    timeanalysis.new <- cbind(as.data.frame(HRVtime$datetime),
                              as.data.frame(HRVtime$TimeAnalysis), 
                              as.data.frame(max_time))
    timeanalysis <- rbind(timeanalysis, timeanalysis.new) #then add that row to the previous data frame
  }
  
}

#Gives a bunch of warnings, but they're all about SDANN and SDNNIDX

#Export to CSV
write.csv(timeanalysis, file = "./Data/current_AM_HRV.csv")
remove(list = ls())
timeanalysis <- read_csv(file = "./Data/current_AM_HRV.csv") %>%
  rename(datetime = "HRVtime$datetime") %>%
  mutate(X1 = NULL)

##### Plotting RMSD #####
library(zoo); library(roll); library(lubridate)

timeanalysis <- timeanalysis %>%
  filter(rMSSD < 150) %>% #filter out readings w/artifacts
  filter(max_time > 75 & max_time < 315) %>% #no snapshots or long measurements
  mutate(day_time = hour(datetime) + minute(datetime)/60, #separate date & time
         date = as.Date(datetime)) %>%
  group_by(date) %>% slice(1) %>% #keep first reading of the day
  ungroup() %>% #need to ungroup for mutate to work
  mutate(rMSSD = as.numeric(rMSSD), lnRMSSD = log(rMSSD)) %>% 
  mutate(RMSSD7d = roll_mean(rMSSD, width = 7), #setup RMSSD bounds
         RMSSD2mon = roll_mean(rMSSD, width = 60),
         RMSSD2monSD = roll_sd(rMSSD, width = 60),
         RMSDD2monHi = (RMSSD2mon + RMSSD2monSD),
         RMSDD2monLo = (RMSSD2mon - RMSSD2monSD)) %>%
  mutate(lnRMSSD7d = roll_mean(lnRMSSD, width = 7), #setup lnRMSSD bounds
         lnRMSSD2mon = roll_mean(lnRMSSD, width = 60),
         lnRMSSD2monSD = roll_sd(lnRMSSD, width = 60),
         lnRMSDD2monHi = (lnRMSSD2mon + lnRMSSD2monSD),
         lnRMSDD2monLo = (lnRMSSD2mon - lnRMSSD2monSD))

graph_start_date <- Sys.Date()-21

#grDevices::colors() run this line for color options

#plot of rMSSD with 7d and 2 month +/- 3SD shading
timeanalysis %>% 
  filter(date > graph_start_date) %>%
  ggplot(data = ., aes(datetime)) + 
  geom_bar(aes(y= rMSSD), colour = NA, fill = "grey", alpha = 0.5, stat = 'identity') + 
  geom_line(aes(y = RMSSD2mon), color = "green", alpha = 0.9) +
  geom_ribbon(aes(ymin=RMSSD2mon-RMSSD2monSD, ymax=RMSSD2mon+RMSSD2monSD), fill="azure2", alpha=0.80) + 
  geom_line(aes(y = RMSSD7d), color = "blue", size = 0.75) 
#No need to add coord_cartesian on this one, doesn't improve view


#Natural log version
timeanalysis %>% 
  filter(date > graph_start_date) %>%
  ggplot(data = ., aes(datetime)) + 
  geom_bar(aes(y= lnRMSSD), colour = NA, fill = "grey", alpha = 0.5, stat = 'identity') + 
  geom_line(aes(y = lnRMSSD2mon), color = "green", alpha = 0.9) +
  geom_ribbon(aes(ymin=lnRMSSD2mon-lnRMSSD2monSD, ymax=lnRMSSD2mon+lnRMSSD2monSD), fill="azure2", alpha=0.80) + 
  geom_line(aes(y = lnRMSSD7d), color = "blue", size = 0.75) +
  coord_cartesian(ylim=c(3.25,4.75))
