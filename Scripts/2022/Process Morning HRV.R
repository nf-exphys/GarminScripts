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
  filter(max_time > 55 & max_time < 315) %>% #no long measurements
  mutate(day_time = hour(datetime) + minute(datetime)/60, #separate date & time
         date = as.Date(datetime)) %>%
  group_by(date) %>% slice(1) %>% #keep first reading of the day
  ungroup() %>% #need to ungroup for mutate to work
  mutate(rMSSD = as.numeric(rMSSD), lnRMSSD = log(rMSSD)) %>% 
  mutate(RMSSD7d = roll_mean(rMSSD, width = 7), #setup RMSSD bounds
         RMSSD1mon = roll_mean(rMSSD, width = 30),
         RMSSD1monSD = roll_sd(rMSSD, width = 30),
         RMSDD1monHi = (RMSSD1mon + RMSSD1monSD),
         RMSDD1monLo = (RMSSD1mon - RMSSD1monSD)) %>%
  mutate(lnRMSSD7d = roll_mean(lnRMSSD, width = 7), #setup lnRMSSD bounds
         lnRMSSD1mon = roll_mean(lnRMSSD, width = 30),
         lnRMSSD1monSD = roll_sd(lnRMSSD, width = 30),
         lnRMSDD1monHi = (lnRMSSD1mon + lnRMSSD1monSD),
         lnRMSDD1monLo = (lnRMSSD1mon - lnRMSSD1monSD))

graph_start_date <- Sys.Date()-450

#grDevices::colors() run this line for color options

#plot of rMSSD with 7d and 1 month +/- SD shading
plot <- timeanalysis %>% 
  filter(date > graph_start_date) %>%
  ggplot(data = ., aes(datetime)) + 
  geom_ribbon(aes(ymin=RMSSD1mon-RMSSD1monSD, ymax=RMSSD1mon+RMSSD1monSD), fill="seagreen1", alpha=0.9, linetype = 2) + #stripe
  geom_bar(aes(y= rMSSD), colour = NA, fill = "grey", alpha = 0.5, stat = 'identity') + 
  #geom_line(aes(y = RMSSD1mon),color = "tomato", alpha = 0.5) + #2 mon line
  geom_line(aes(y = RMSSD7d), color = "blue", size = 0.75, alpha = 0.7) + #7d line
  scale_x_datetime(date_breaks = "10 days") + 
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Daily RMSSD with 1 week and 1 month rolling averages")
#No need to add coord_cartesian on this one, doesn't improve view

plot

#Natural log version
timeanalysis %>% 
  filter(date > graph_start_date) %>%
  ggplot(data = ., aes(datetime)) + 
  geom_bar(aes(y= lnRMSSD), colour = NA, fill = "grey", alpha = 0.5, stat = 'identity') + 
  geom_ribbon(aes(ymin=lnRMSSD2mon-lnRMSSD2monSD, ymax=lnRMSSD2mon+lnRMSSD2monSD), fill="azure2", alpha=0.80) + 
  geom_line(aes(y = lnRMSSD2mon), color = "green", alpha = 0.9) +
  geom_line(aes(y = lnRMSSD7d), color = "blue", size = 0.75) +
  coord_cartesian(ylim=c(3,4.75))
