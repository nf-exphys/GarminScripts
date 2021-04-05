#Automated import of FIT directly files from the watch

library(FITfileR); library(tidyverse); library(openxlsx)

# if(!requireNamespace("remotes")) {
#   install.packages("remotes")
# }
# 
# remotes::install_github("grimbough/FITfileR")

#last run read in was 02-26 at ~8:30 pm
#oldest run on watch is from 03/16
#data is missing from the 7 mile on 03/16 AM (6439863961) to
# the 43 minute run on 02/27 (6345777580)

# Import Data ---------------------------------------------------

#list of possible watch paths
all_watch_path <- paste0(LETTERS, ":", "\\GARMIN\\ACTIVITY\\")

#finds drive where watch is connected
watch_drive <- which(lapply(all_watch_path, file.access, mode=4) == 0)
watch_path <- all_watch_path[watch_drive]

#Check if the watch is plugged in
if(file.access(names = watch_path, mode=4) == 0){
  #update this eventually to say which drive
  print(paste0("Watch detected. Continuing"))
  continue <- T
} else{
  print("GPS Watch Not Detected. Should stop.")
  continue <- F
}

files_to_read <- list()
if(continue == T){
  #Compare files already read in to new files
  previous_files <- list.files(path = "./Data/ExportedRunData/Old_Fit_Files/", full.names=F, pattern = "*.fit", ignore.case=T)
  new_files <- list.files(path = watch_path, full.names=F, pattern = "*.fit", ignore.case=T)
  
  #List of files to be read in, adds file path
  #Compares old files (already read in) to new files (currently on GPS watch)
  files_to_read <- setdiff(new_files, previous_files) 
  
}

if(length(files_to_read) == 0){
  print("No new files to read")
  continue <- F
}

if(continue == T){
  read_in_now <- readline("Do you want to read in new files right now? Y or N...")
  if(read_in_now == "Y"){
    msg <- paste0("Okay, will continue reading in ", length(files_to_read), " files")
    print(msg)
  } else{
    continue <- F
  }
  
}

if(continue == T){
  files_to_read <- paste0(watch_path, files_to_read)
  
  #Sets n as the number of files to be read
  n <- length(files_to_read)
  
  #Might have to add some sort of condition here
  
  all_data <- lapply(files_to_read, readFitFile)
  
  records <- lapply(all_data, records)
  
  #Catch record activity split into two and bind together
  for (i in 1:length(records)){
    if (length(records[[i]]) > 1){
      records[[i]] <- records[[i]] %>% 
        bind_rows() %>%
        arrange(timestamp)
    }  
    
  }
  
  laps <- lapply(all_data, laps)
  
  process_hrv <- function(x) {
    tryCatch({ret <- getMessagesByType(x, "hrv");}, error = function(e) {ret <<- NA});
    ret
  }
  
  hrv_data <- lapply(all_data, process_hrv)
  
  #Clean up HRV data
  #throws warnings about using lists to find NA, which is fine
  for (i in 1:length(hrv_data)){
    if (any(is.na(hrv_data[[i]])) == TRUE){
      next
    } else {
      hrv <- unlist(hrv_data[[i]]$time)
      hrv <- as_tibble(hrv) %>%
        rename(hrv = value)
      hrv_data[[i]] <- hrv
    }
    
  }
  
  sports <- lapply(all_data, getMessagesByType, "sport") %>%
    bind_rows() %>%
    rename_with( ~ paste0("sports_", .x))
  
  sessions <- lapply(all_data, getMessagesByType, "session") %>%
    bind_rows() 
  
  sum_data <- bind_cols(sports, sessions, .name_repair = "unique") %>%
    mutate(timestamp = timestamp - (6*60*60),
           start_time = start_time - (6*60*60)) #temp workaround for times
}

  #### Export ####
if(continue == T){  
  #folder for saving CSVs
  save_here <- ".\\Data\\ExportedRunData\\Cleaned_CSVs\\"
  
  #Creates character with timestamp for name of file
  time_char <- sum_data %>% pull(timestamp)
  time_char <-  gsub("[[:punct:]]", "_", time_char)
  
  #Makes names for each csv to export
  recordfile <- paste0(save_here, time_char, " record", ".csv")
  lapfile <- paste0(save_here, time_char, " lap", ".csv")
  sumdatafile <- paste0(save_here, time_char, " sumdata", ".csv")
  hrvfile <- paste0(save_here, time_char, " hrvdata", ".csv")
  
  #Convert sum_data to a list to match other file types
  sum_data <- sum_data %>%
    group_by(timestamp) %>%
    group_split()
  
  for (i in 1:length(records)){
    records[[i]] %>%
      mutate(ID = time_char[[i]]) %>%
      select(ID, timestamp, sort(names(.))) %>%
      write.csv(., file = recordfile[[i]], row.names = FALSE)
    
    laps[[i]] %>%
      mutate(ID = time_char[[i]]) %>%
      select(ID, timestamp, sort(names(.))) %>%
      write.csv(., file = lapfile[[i]], row.names = FALSE)
    
    #don't add ID to hrv_data
    write.csv(hrv_data[[i]], file = hrvfile[[i]], row.names = FALSE)
    
    sum_data[[i]] %>%
      mutate(ID = time_char[[i]]) %>%
      select(ID, timestamp, sort(names(.))) %>%
      write.csv(., file = sumdatafile[[i]], row.names = FALSE)  
  }
  
  
  # Move Files --------------------------------------------------------------
  
  #Copy files_to_read to Old_Fit_Files
  file.copy(from = files_to_read, to = "./Data/ExportedRunData/Old_Fit_Files/")
  
}



