library(tidyverse, warn.conflicts = F); library(RHRV)
library(fitFileR)

library(doParallel)
n <- detectCores()
registerDoParallel(n-4)

#Read in CSVs with HRV data
hrv_files <- list.files(path = "./Data/ExportedRunData/Cleaned_CSVs/", 
                        pattern="* hrvdata.csv", full.names = T)

clean_filename <- function(filename){
  filename <- str_remove(string = filename, pattern = "./Data/ExportedRunData/Cleaned_CSVs/")
  filename <- str_remove(string = filename, pattern = " hrvdata.csv")
  filename
  }

hrv_list <- list()
hrv_list[["rr_int"]] <- lapply(hrv_files, read_csv) #read in HRV
hrv_list[["source"]] <- lapply(hrv_files, clean_filename) #read in date/time

first_correction <- 0.025 #trial-and-error
second_correction <- 0.025 #now catch ones way outside

#consider using cubic spline interpolation or linear interpolation
for (i in 1:length(hrv_list$rr_int)){
  hrv_list$rr_int[[i]] <- hrv_list$rr_int[[i]] %>%
    rename(rr = hrv) %>%
    mutate(rr = case_when(rr < 1 ~ rr)) %>% #sets high values to NA
    drop_na() %>% #drops NA values
    mutate( #calculates acceptable range for RR intervals
      lag_rr = lag(rr),
      high_bound = lag(rr)*(1+first_correction),
      low_bound = lag(rr)*(1-first_correction)
     ) %>%
    filter( #removes RR intervals that don't meet those bounds
        rr < high_bound & rr > low_bound
        ) %>%
    mutate( #re-calculate to catch the ones that are way off
        high_bound = lag(rr)*(1+second_correction),
        low_bound = lag(rr)*(1-second_correction)
      ) %>%
      filter(
        rr < high_bound & rr > low_bound
      ) %>%
    mutate(n = row_number()) %>%
    select(n, rr) %>%
    filter(rr < 0.7) #not the ideal way of doing this, but it'll have to work for now
}

for(i in 1:length(hrv_list$rr_int)){
p <- hrv_list$rr_int[[i]] %>%
  #filter(rr < 0.8) %>%
ggplot(data = ., aes(x=n, y=rr)) + geom_line() + ggtitle(hrv_list$source[[i]])
print(p)
}

#Writes all cleaned CSVs to text files to be read in by RHRV
for(i in 1:length(hrv_list$rr_int)){
  hrv_list$rr_int[[i]] %>%
    select(rr) %>%
    write.table(., 
            file = paste0("./Text Files/cleaned_hrv_", hrv_list$source[[i]], ".txt"), 
            row.names = F, col.names = F)
}

hrv_txt_files <- list.files(path = "./Text Files/", pattern = "cleaned_hrv*", full.names = T)

#Change to POSIXct and rearrange to RHRV format
for(i in 1:length(hrv_list$source)){
  hrv_list$source[[i]] <- as.POSIXct(hrv_list$source[[i]], format = "%Y_%m_%d %H_%M_%S")
  hrv_list$source[[i]] <- format(hrv_list$source[[i]], '%d/%m/%Y %H:%M:%S')
} 

#date_source is the hrv_list$source
library(nonlinearTseries)

#goal is to combine source data (hrv_list$source) with RR data (hrv_txt_files)
date_and_txt <- bind_cols(as_tibble_col(unlist(hrv_list$source)), as_tibble_col(unlist(hrv_txt_files)))
names(date_and_txt) <- c("date", "txt")  

#date_source is hrv_list$source[[#]]

rolling_dfa <- function(date_source){
  
  #take the date and find the associated text file
  rr_txt <- date_and_txt %>%
    filter(date == date_source) %>%
    pull(txt)
  
  #create structure to store HRV data
  hrv.data <- CreateHRVData() 
  hrv.data <- SetVerbose(hrv.data, T)
  
  hrv.data <- LoadBeat(fileType = "RR", HRVData = hrv.data, datetime = date_source,
                       Recordname = rr_txt)
  
  if(nrow(hrv.data$Beat) > 500){
    nihr <- BuildNIHR(hrv.data)
    PlotNIHR(nihr) #close but not quite
    
    nihr <- FilterNIHR(nihr, minbpm = 80, maxbpm = 200)
    PlotNIHR(nihr) #better
    
    #Split data and find DFA
    
    step <- 120 #number of seconds per window
    
    nihr$Beat <- nihr$Beat %>%
      mutate(round_time = round(Time, 1),
             step_time = round_time + step
      )
    
    #pulls RR intervals for a given window and calculates DFA a1
    alpha1_values <- list()
    hr_values <- list()
    
    for (i in seq(from=1, to=nrow(nihr$Beat), by=5)){
      
      RRs_of_interest <- nihr$Beat %>%
        filter(Time > round_time[i] & Time < step_time[i])
      
      #doesn't continue if there aren't at least 120 rows 
      if(nrow(RRs_of_interest) < 120){
        break
      }
      
      #find HR
      current_hr <- RRs_of_interest %>%
        summarise(HR = mean(niHR))
      
      #set bounds for DFA
      dfa_analysis <- RRs_of_interest %>%
        pull(RR) %>%
        dfa(time.series = ., npoints = 30, window.size.range = c(4,300))
      
      regression_range <- c(4,20) #based on Kubios
      
      dfa_est <- estimate(dfa_analysis, do.plot=F, regression.range = regression_range)
      #dfa_est[1]
      alpha1_values <- rlist::list.append(alpha1_values, dfa_est[1])
      hr_values <- rlist::list.append(hr_values, current_hr)
      
    }
    
    #kept getting errors with alpha1_values = 0, filling with fake data to fix
    if(length(alpha1_values) == 0){
      alpha1_values <- list(0,0,0)
      hr_values <- list(0,0,0)
      
    }
    
    alpha1_values <- alpha1_values %>%
      unlist() %>%
      as_tibble_col()
    
    hr_values <- hr_values %>%
      unlist() %>%
      as_tibble_col()
    
    dfa_values <- cbind(alpha1_values, hr_values)
    names(dfa_values) <- c("a1", "HR")
    
    # dfa_values %>%
    #   ggplot(data = ., aes(x=HR, y=a1)) + geom_smooth()
    
    lm_object <- lm(a1 ~ HR, data = dfa_values) 
    
    intercept <- as.numeric(lm_object$coefficients[1])
    slope <- as.numeric(lm_object$coefficients[2])
    VT_a1_HR <- (0.75-intercept)/slope
    
    notable_dfa_values <- list(date = nihr$datetime, 
                               lm = lm_object, 
                               HR=VT_a1_HR, 
                               dfa_data = dfa_values)
    
  }
  else{
    notable_dfa_values <- list(date = hrv.data$datetime)
    }
  return(notable_dfa_values)
  
}

#notable_dfa_values <- rolling_dfa(hrv_list$source[[25]])

all_dfa_values <- list()
all_dfa_values <- lapply(hrv_list$source, rolling_dfa)

no_dfa <- which(lapply(all_dfa_values, length) < 4)

exclude_dfa <- list()
for(i in 1:length(no_dfa)){
  exclude_dfa <- rlist::list.append(exclude_dfa, as.character(all_dfa_values[no_dfa][[i]]$date))
 }
write.table(exclude_dfa, file = "DFA_exclusions.txt")

filtered_dfa_values <- list()
filtered_dfa_values <- all_dfa_values[-no_dfa]
for(i in 1:length(filtered_dfa_values)){
  
  my_date <- filtered_dfa_values[[i]]$date
  my_date <- format(my_date, '%Y_%m_%d %H_%M_%S')
  
  filtered_dfa_values[[i]]$dfa_data %>%
    write_csv(., file = paste0("./Data/DFA/rolling_dfa_", my_date, ".csv"))
  
}




for(i in 1:length(filtered_dfa_values)){
p1 <- ggplot() + 
  geom_point(data = filtered_dfa_values[[i]]$dfa_data, aes(x=n, y=a1), color = "blue") + 
  ggtitle(filtered_dfa_values[[i]]$date)
p2 <- ggplot() + 
  geom_point(data = filtered_dfa_values[[i]]$dfa_data, aes(x=n, y=HR), color = "red") + 
  ggtitle(round(filtered_dfa_values[[i]]$HR), 0)
gridExtra::grid.arrange(p1, p2, nrow=2)
}


dfa_hr_date <- data.frame(hr=numeric(), date = numeric())
for(i in 1:length(filtered_dfa_values)){
  temp_df <- data.frame(hr = filtered_dfa_values[[i]]$HR, date = filtered_dfa_values[[i]]$date)
  dfa_hr_date <- rbind(dfa_hr_date, temp_df)
}
dfa_hr_date %>%
  filter(hr < 200 & hr > 100) %>%
  summarise(HR = mean(hr))
ggplot(data = ., aes(x=date, y=hr)) + geom_line()



#One-off graphs of alpha1 and HR
# p1 <- ggplot() + 
#   geom_point(data = notable_dfa_values$dfa_data, aes(x=n, y=a1), color = "blue") + 
#   ggtitle(notable_dfa_values$date)
# p2 <- ggplot() + 
#   geom_point(data = notable_dfa_values$dfa_data, aes(x=n, y=HR), color = "red")
# gridExtra::grid.arrange(p1, p2, nrow=2)

# #Assorted activities from late December & early January 2021
# easy_run <- fitFileR::readFitFile(fileName = "E:\\GARMIN\\ACTIVITY\\ACS81007.FIT") #easy run with Coach K
# prog_bike <- readFitFile(fileName = "E:\\GARMIN\\ACTIVITY\\B12H3747.FIT") #progression bike ride
# long_run <- readFitFile(fileName = "E:\\GARMIN\\ACTIVITY\\B1285039.FIT") #115 min long run w/~10 min progression
# 
# hrv <- prog_bike$hrv$time #set the hrv data as its own object for more succinct code
# hrv <- long_run$hrv$time
# hrv <- easy_run$hrv$time
# 
# #Alternatively, can read in Exercise HRV data from CSV
#   #Example: CSV from 01/09/2021 easy long run with Dylan
# hrv <- read_csv(file = "./Data/ExportedRunData/Cleaned_CSVs/2021_01_09 10_37_24 hrvdata.csv")

# #Pulls together RR intervals from single file if in list
# rr_data <- matrix(ncol=1)
# for (i in 1:length(hrv)){
#   if("list" %in% class(hrv)){
#     if (i==1){
#       rr_data <- tibble(hrv=as.numeric(unlist(hrv[i]))) %>%
#         pivot_longer(cols = hrv, names_to = NULL, values_to = "rr") %>%
#         mutate(rr = case_when(rr < 60 ~ rr)) #drops 'NA' values
#       } else{
#       rr_data <- tibble(hrv=as.numeric(unlist(hrv[i]))) %>%
#         pivot_longer(cols = hrv, names_to = NULL, values_to = "rr") %>%
#         mutate(rr = case_when(rr < 60 ~ rr)) %>% #drops 'NA' values
#         bind_rows(rr_data,.) %>%
#         filter(is.na(rr) == F) %>%
#         select(rr)
#     }
#   } else {
#     stop("HRV data isn't a list. This for loop isn't needed")
#   }
#   
# }
# 
# #Filters RR intervals from single file if in tibble already
# rr_data <- hrv %>% 
#   rename(rr = hrv) %>%
#   mutate(rr = case_when(rr < 60 ~ rr)) %>%
#   drop_na()
# 
# #Plot of RR intervals over time
# ggplot(data = rr_data, aes(y=rr, x=row_number(rr))) + geom_point()