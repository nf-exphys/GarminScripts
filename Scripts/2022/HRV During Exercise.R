library(tidyverse, warn.conflicts = F); library(RHRV)
library(nonlinearTseries)

#### Setup Files ####
#Figure out which CSVs to read in 
hrv_file_path <- "./Data/ExportedRunData/Cleaned_CSVs/"

hrv_files <- list.files(path = "./Data/ExportedRunData/Cleaned_CSVs/", 
                        pattern="* hrvdata.csv", full.names = F)

previous_dfa <- list.files(path = "./Data/DFA/", pattern = "*.csv")

clean_file_for_comp <- function(x){ #x is either hrv_files or previous_dfa
  x <- str_remove_all(x, pattern = "[:alpha:]|\\.")
  x <- str_split(x, pattern = " ", n=3)
  x <- unlist(x)[1:2]
  x <- paste(x[1], x[2], sep = " ")
  x
}

#hrv_files is all of the HRV CSV files
hrv_files <- lapply(hrv_files, clean_file_for_comp)

#previous_dfa is all of the old DFA files
#Currently 12-18 to 01-28
previous_dfa <- lapply(previous_dfa, clean_file_for_comp)

#List of files in hrv_files not in previous dfa
hrv_to_read <- setdiff(hrv_files, previous_dfa)

#Read in previous DFA exclusions & format
exclude_dfa <- read.table("./Data/DFA/DFA_exclusions.txt", 
                          fill = T, col.names = c("date")) %>%
                        pull(date)

exclude_dfa2 <- read.table("./Data/DFA/DFA_exclusion.txt", 
                          fill = T, col.names = c("date")) %>%
  pull(date)

exclude_dfa <- c(exclude_dfa, exclude_dfa2)
remove(exclude_dfa2)

#Compare hrv_to_read with DFA exclusions
#And then overwrites hrv_files from all of the files to just the ones to read
#(I know, not great naming here)

hrv_files <- setdiff(hrv_to_read, exclude_dfa)


#Assume there's something new to read
continue <- T

#Stops code if there aren't new files to read
if (length(hrv_files) < 1){
  print("There are no new HRV files to be read. Stopping.")
  continue <- F
}

#Check to see if I want to read in new files or not
if(continue == T){
  read_in_now <- readline("Do you want to process HRV data right now? Y or N...")
  if(read_in_now == "Y"){
    msg <- paste0("Okay, will continue reading in ", length(hrv_files), " files")
    print(msg)
  } else{
    continue <- F
  }

}

#### Simple RR Interval Processing
if(continue == T){
  
  #Setup hrv_file storage
  hrv_list <- list()
  
  #Adds dates
  hrv_list[["source"]] <- hrv_files
  
  #Add file path back & import CSVs with RR intervals
  hrv_files <- paste0(hrv_file_path, hrv_files, " hrvdata.csv")
  hrv_list[["rr_int"]] <- lapply(hrv_files, read_csv)
  
  first_correction <- 0.025 #trial-and-error
  second_correction <- 0.025 #now catch ones way outside
  
  #consider using cubic spline interpolation or linear interpolation
  for (i in 1:length(hrv_list$rr_int)){
    
    #There were some files where column name got stuck as value, not hrv
    #This fixes that error before reading in everything else
    if(colnames(hrv_list$rr_int[[i]]) != "hrv"){
      colnames(hrv_list$rr_int[[i]]) <- "hrv"
    }
    
    hrv_list$rr_int[[i]] <- hrv_list$rr_int[[i]] %>%
      rename(rr = hrv) %>%
      filter(rr < 700) %>% #cutoff > ~85 bpm
      #keep RRs on milisecond scale or reasonable hundreds scale
      mutate(rr = case_when((rr < 1 & rr > 0) | (rr > 280 & rr < 900) ~ rr)) %>% 
      #classify based on unit scale
      mutate(units = case_when(rr < 1 ~ "ms",
                                   rr > 1 ~ "hundreds")
             ) %>% #sets high values to NA
      drop_na() %>% #drops NA values
      mutate(rr = ifelse(units == "ms", 
                 rr * 1000, #get all RR intervals on same time scale
                 rr)) %>%
      mutate(rr = rr/1000) %>% #convert unit back to seconds from ms
      select(-units) %>% 
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
      filter(rr < 0.7) #cutoff > ~85 bpm for 2nd time just in case
    #filter isn't the ideal way of doing this, but it'll have to work for now
  }
  
  for(i in 1:length(hrv_list$rr_int)){
    p <- hrv_list$rr_int[[i]] %>%
      #filter(rr < 0.8) %>%
      ggplot(data = ., aes(x=n, y=rr)) + geom_line() + ggtitle(paste0(hrv_list$source[[i]], " (", i, ")"))
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
  
  #ADD something here to prune down hrv_txt_files to just the files in hrv_list$source
  hrv_txt_files <- unique(grep(paste(hrv_list$source, collapse = "|"), hrv_txt_files, value = T))
  
  #Change to POSIXct and rearrange to RHRV format
  for(i in 1:length(hrv_list$source)){
    hrv_list$source[[i]] <- as.POSIXct(hrv_list$source[[i]], format = "%Y_%m_%d %H_%M_%S")
    hrv_list$source[[i]] <- format(hrv_list$source[[i]], '%d/%m/%Y %H:%M:%S')
  } 
  
  #Creates tibble with date/time in RHRV format and file path for cleaned HRV data 
  date_and_txt <- bind_cols(as_tibble_col(unlist(hrv_list$source)), 
                            as_tibble_col(unlist(hrv_txt_files)))
  names(date_and_txt) <- c("date", "txt")  
}

#### Calculate DFA ####
# function to find rolling DFA values and return them
  rolling_dfa <- function(date_source){
    
    #take the date and find the associated text file
    rr_txt <- date_and_txt %>%
      filter(date == date_source) %>%
      pull(txt)
    
    #setup boolean in case text file is empty
    no_rr_data <- F
    if(file.size(rr_txt) == 0){ #if rr intervals weren't recorded
      no_rr_data <- T
    }
    
    #If RR data exists, try to process it
    if((no_rr_data) == F){
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
        #if there isn't more than 500 beats left in the file
          notable_dfa_values <- list(date = hrv.data$datetime)
      }
    }
    
    #if there isn't RR data, just put in dummy data
    if(no_rr_data == T){
        alpha1_values <- list(0,0,0)
        hr_values <- list(0,0,0)
        
      alpha1_values <- alpha1_values %>%
        unlist() %>%
        as_tibble_col()
      
      hr_values <- hr_values %>%
        unlist() %>%
        as_tibble_col()
      
      dfa_values <- cbind(alpha1_values, hr_values)
      names(dfa_values) <- c("a1", "HR")
      
      notable_dfa_values <- list(date = date_source)
    }
    
    return(notable_dfa_values)
    
  }

if(continue == T){
  #calculate DFA values for each run
  all_dfa_values <- list()
  all_dfa_values <- lapply(hrv_list$source, rolling_dfa)
}  

#### Post-processing DFA ####
#Add to DFA exclusions, export DFA values, etc. 

if(continue == T){
  
  #find runs without DFA data
  no_dfa <- which(lapply(all_dfa_values, length) < 4)
  
  #create exclude_dfa object if it doesn't already exist
  if(("exclude_dfa" %in% objects()) == FALSE){
    exclude_dfa <- list()
  }
  
  #Takes files that didn't work for DFA and adds them to exclude_dfa list
  if(length(no_dfa) > 0){
    for(i in 1:length(no_dfa)){
      #fix formating
      #all_dfa_values[no_dfa][[i]]$date <- format(all_dfa_values[no_dfa][[i]]$date, "%Y_%m_%d %H_%M_%S")
      #add to exclude_dfa
      exclude_dfa <- rlist::list.append(exclude_dfa, as.character(all_dfa_values[no_dfa][[i]]$date))
    }
  }
  
  exclude_dfa <- unlist(exclude_dfa)
  write.table(exclude_dfa, file = "./Data/DFA/DFA_exclusions.txt")
  
  #keeps reasonable DFA values and doesn't 
  filtered_dfa_values <- list()
  if(length(no_dfa) == 0){
    filtered_dfa_values <- all_dfa_values
  } else{
    filtered_dfa_values <- all_dfa_values[-no_dfa]  
  }
  
  new_dfa <- T
  if(length(filtered_dfa_values) == 0){
    print("No new rolling DFA data to write. Stopping")
    new_dfa <- F
  }
  
  if(new_dfa ==T){
    for(i in 1:length(filtered_dfa_values)){
      
      my_date <- filtered_dfa_values[[i]]$date
      my_date <- format(my_date, '%Y_%m_%d %H_%M_%S')
      
      filtered_dfa_values[[i]]$dfa_data %>%
        write_csv(., file = paste0("./Data/DFA/", my_date, " rolling_dfa.csv"))
      
    }
    
  }
  
}
 
  




#### Extra code for graphing ####
# filtered_dfa_values[[i]]$dfa_data %>%
#   filter(a1 < 1.0) %>%
#   ggplot(data = ., aes(x=HR, y=a1)) + geom_point()
# 
# #Plotting HR and DFA
# for(i in 1:length(filtered_dfa_values)){
# 
# #Add column with n  
# filtered_dfa_values[[i]]$dfa_data <- filtered_dfa_values[[i]]$dfa_data %>%
#   as_tibble() %>%
#   mutate(n = row_number())
# 
# #DFA data
# 
# p1 <- ggplot() + 
#   geom_point(data = filtered_dfa_values[[i]]$dfa_data, aes(x=n, y=a1), color = "blue") + 
#   ggtitle(filtered_dfa_values[[i]]$date)
# 
# #HR data
# p2 <- ggplot() + 
#   geom_point(data = filtered_dfa_values[[i]]$dfa_data, aes(x=n, y=HR), color = "red") + 
#   ggtitle(round(filtered_dfa_values[[i]]$HR), 0)
# 
# #Setup plot layout
# gridExtra::grid.arrange(p1, p2, nrow=2)
# }
# 
# 
# dfa_hr_date <- data.frame(hr=numeric(), date = numeric())
# for(i in 1:length(filtered_dfa_values)){
#   temp_df <- data.frame(hr = filtered_dfa_values[[i]]$HR, date = filtered_dfa_values[[i]]$date)
#   dfa_hr_date <- rbind(dfa_hr_date, temp_df)
# }
# dfa_hr_date %>%
#   filter(hr < 200 & hr > 100) %>%
#   summarise(HR = mean(hr))
# ggplot(data = ., aes(x=date, y=hr)) + geom_line()



#One-off graphs of alpha1 and HR
# p1 <- ggplot() + 
#   geom_point(data = notable_dfa_values$dfa_data, aes(x=n, y=a1), color = "blue") + 
#   ggtitle(notable_dfa_values$date)
# p2 <- ggplot() + 
#   geom_point(data = notable_dfa_values$dfa_data, aes(x=n, y=HR), color = "red")
# gridExtra::grid.arrange(p1, p2, nrow=2)