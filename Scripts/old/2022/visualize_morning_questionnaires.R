library(RHRV); library(tidyverse)

# Import Data ---------------------------------------------------

all_hrv_files <- list.files(path = "./HRV Files to be Read/", pattern = ".txt", full.names = FALSE) 
#For use in naming/setting datetime field

start_idx <- min(which(str_extract(all_hrv_files, pattern = "\\d{4}") %>% as.numeric() > 2020))

hrv_files <- DateChrTxt[start_idx:length(DateChrTxt)] %>% as_tibble()

#Limits HRV files to the first one of each day to avoid confusion with research readings
hrv_files <- hrv_files %>% 
  separate(value, into = c("year", "month", "day_hour", "min", "sec"), sep = "-") %>% 
  separate(day_hour, into = c("day", "hour"), sep = " ") %>% 
  group_by(year, month, day) %>% 
  slice(1) %>% 
  ungroup() %>% 
  unite(1:3, col = "file_name", sep = "-") %>% 
  unite(2:4, col = "half_name", sep = "-") %>% 
  unite(1:2, col = "file_name", sep = " ") %>% 
  pull(file_name)

hrv_files_with_path <- paste0("HRV Files to be Read/", hrv_files)

#Sets n as the number of files to be read
n <- length(hrv_files)

#Read in files from FilesToRead; convert read-in file to a data frame and store it in a list; 
for (i in 1:n) {
  
  HRVdata <- CreateHRVData() #Create data structure to store HRV data
  
  #Converting yyyy-mm-dd to dd/mm/yyyy HH:MM:SS
  list_date <- unlist(str_split(string = hrv_files[i], pattern = "[:punct:]|[:blank:]")) 
  #splits list_date at spaces and punctuation then applies unlist to fix output
  d <- paste0(list_date[c(3,2,1)], collapse = "/") #date as dd/mm/yyyy
  t <- paste0(list_date[c(4,5,6)], collapse = ":") #time as HH:MM:SS
  dt <- paste0(c(d,t), collapse = " ")
  
  #loads RR data
  HRVdata <- LoadBeatRR(HRVdata, RecordName = hrv_files[i], scale = 0.001,
                        RecordPath = "HRV Files to be Read/", datetime = dt)
  
  #estimate reading duration
  max_time <- max(HRVdata$Beat$Time)
  
  HRVdata <- BuildNIHR(HRVdata)
  #interprets RR data into HR
  
  HRVdata <- FilterNIHR(HRVdata, minbpm = 30, maxbpm = 100)
  #filters within physiological ranges
  
  HRVdata <- CreateNonLinearAnalysis(HRVdata)
  
  dfa <- CalculateDFA(HRVdata)
  
  reg_range <- c(4,16)
  
  dfa <- EstimateDFA(dfa, regressionRange = reg_range, doPlot = FALSE)
  
  dfa_val <- dfa[["NonLinearAnalysis"]][[1]][["dfa"]][["statistic"]][[1]][["estimate"]][1]
  
  HRVtime <- CreateTimeAnalysis(HRVdata, size = 5) #performs simple time analysis with size set to default
  
  if (length(HRVdata$Beat$Time) > 400){ #if there are lots of rows (i.e. it's exercise not resting)
    print(i) 
    #next #skips over copying data into dataframe
  }
  if (i == 1){ #the first time through, just make the data frame
    timeanalysis <- cbind(as.data.frame(HRVtime$datetime),
                          as.data.frame(HRVtime$TimeAnalysis), 
                          as.data.frame(max_time), 
                          as.data.frame(dfa_val))
  }
  if (i>1){ #after the first time, create a temporary storage timeanalysis.new 
    timeanalysis.new <- cbind(as.data.frame(HRVtime$datetime),
                              as.data.frame(HRVtime$TimeAnalysis), 
                              as.data.frame(max_time), 
                              as.data.frame(dfa_val))
    
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
  # filter(date > graph_start_date) %>%
  filter(date > "2021-12-01") %>% 
  ggplot(data = ., aes(datetime)) + 
  geom_ribbon(aes(ymin=RMSSD1mon-RMSSD1monSD, ymax=RMSSD1mon+RMSSD1monSD), fill="seagreen1", alpha=0.9, linetype = 2) + #stripe
  geom_bar(aes(y= rMSSD), colour = NA, fill = "grey", alpha = 0.5, stat = 'identity') + 
  geom_line(aes(y = RMSSD1mon),color = "tomato", alpha = 0.5) + #2 mon line
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


#Add morning data

subjective_data <- read_csv(list.files(path = "Data", pattern = "csv", full.names = TRUE)[3]) %>% 
  janitor::remove_empty(., which = "cols")

colnames(subjective_data) <- tolower(colnames(subjective_data)) %>% 
  str_replace_all(., " ", "_")

subjective_data <- subjective_data %>%
  mutate(across(2:9, str_match, "[:digit:]")) %>% 
  mutate(across(2:9, as.numeric)) %>% 
  select(-submission_date) %>% 
  rename(date = enter_the_date, time = enter_the_time)

subj_measures <- colnames(subjective_data)[1:8]

hrv_data <- timeanalysis %>% 
  separate(datetime, into = c("date", "time"), sep = " ") %>% 
  select(-1)

all_data <- left_join(subjective_data, timeanalysis, by = "date")

plot_data <- all_data %>% 
  select(date, lnRMSSD, subj_measures) %>% 
  pivot_longer(cols = 2:10, names_to = "measure") %>% 
  filter(measure == "lnRMSSD" | measure == subj_measures[1])

ggplot(data = plot_data, aes(x = date, y = value, group = measure, color = measure)) + 
  geom_line()

all_data %>% 
  select(lnRMSSD, subj_measures) %>% 
  cor(., method = "pearson")

#### sRPE data ####

rpe_data <- read_csv(list.files(path = "Data", pattern = "csv", full.names = TRUE)[4]) %>% 
  janitor::remove_empty(., which = "cols")

colnames(rpe_data) <- tolower(colnames(rpe_data)) %>% 
  str_replace_all(., " ", "_")

rpe_data <- rpe_data %>%
  mutate(rpe = str_match(`how_hard_was_your_run?`, "[:digit:]"),
         time_mins = str_match(`how_long_did_you_run?`, "[:digit:]"),
         rpe = as.numeric(rpe),
         time_mins = as.numeric(time_mins)) %>% 
  select(submission_date, rpe, time_mins) %>% 
  separate(submission_date, into = c("date", "time"), sep = " ") %>% 
  mutate(date = str_replace_all(date, "/", "-")) %>% 
  mutate(srpe = rpe * time_mins) %>% 
  group_by(date)

ggplot(data = rpe_data, aes(x = date, y = srpe)) + geom_bar(stat = 'identity')

rpe_hrv <- right_join(rpe_data, hrv_data, by = "date")

rpe_hrv[1:70,] %>% 
  # mutate(lag_srpe = lag(srpe)) %>% View()
  # select(lag_srpe, lnRMSSD)
  ggplot(data = ., aes(x = srpe, y = lnRMSSD)) + geom_point()
