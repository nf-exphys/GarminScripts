
source("./Scripts/2022/master_fit_func.R")

#### Automate Transfer/Extraction of Zip Files ####
download_location <- "./Data/zip_fit_files/"
storage_location <- "./Data/raw_fit_files"

# storage_location <- "./Data/processed_fit_files" #fit files go here once analyzed

zip_files <- list.files(path = download_location, pattern = "*.zip", full.names = TRUE)

lapply(zip_files, unzip, exdir = storage_location)
  #if zip_files is empty, nothing happens and no error is produced

#Makes CSV of fit file numbers but doesn't write it if there isn't new data

if(nrow(as_tibble(zip_files)) > 0){
  
  #Make a data frame with names of all the fit files that have been extracted
  
  file_names <- as_tibble(zip_files) %>%
    rename(fit_name = value) %>%
    mutate(fit_name = str_remove(fit_name, download_location),
           fit_name = str_remove(fit_name, ".zip"))
  
  date_today <- tolower(as.character(date()))
  date_today <- str_replace_all(date_today, " ", "_")
  date_today <- str_replace_all(date_today, ":", "_")
  
  file_path <- paste0("./Data/", "fit_files_already_extracted_", date_today, ".csv")
  
  #Write this to a file to compare for future imports
  write_csv(file_names, file = file_path)
  
}

#Now that zip files have been extracted, they can be deleted
file.remove(zip_files)

#### Setup for Parallel Processing ####

#list all files ready to be processed
fit_files_list <- list.files(path = storage_location, pattern = ".fit", full.names = TRUE)

library(parallel)
numCores <- detectCores()
cores_to_use <- numCores-2
cl <- makeCluster(cores_to_use)

# #start by making everything into FitFile objects
# all_data <- parallel::parLapply(cl, fit_files_list, FITfileR::readFitFile)

#Set up workers with necessary libraries
clusterEvalQ(cl, {
  library(dplyr);
  library(tidyr);
  library(readr);
  library(stringr);
  library(FITfileR);
  library(elevatr);
  library(sp);
  library(sf)
  })

#make list of functions in global environment
list_of_funcs <- as.vector(lsf.str())

#make sure each worker can access the functions
clusterExport(cl, list_of_funcs)

#### Process Data ####

# tictoc::tic()
parallel::parLapply(cl, 
                    fit_files_list, 
                    fun = function(x) tryCatch(process_fit_data(x), error = function(e) e)
                    )

closeAllConnections()
# tictoc::toc()

# tictoc::tic()
# lapply(fit_files_list[1:10], process_fit_data)
# closeAllConnections()
# tictoc::toc()

#6339266060 is the last one that worked successfully
which(str_detect(fit_files_list, "6339266060") == T)

process_fit_data(fit_files_list[407])

listMessageTypes(readFitFile(fit_files_list[407]))

test <- readFitFile(fit_files_list[407])

getMessagesByType(test, "file_id")


stopCluster(cl)
remove(cl)

for (i in 1:length(all_data)){
  
  fit_id <- str_match_all(string = fit_files[[i]], 
                          pattern = "[0-9]+") %>% 
    unlist() %>% 
    as.numeric()
  
  #get preliminary data
  file_info <- get_data("file_id")
  sport <- get_data("sport") #need to be before record to determine indoor/outdoor
  
  #process record data
  record <- get_record(i)
  
  #get the rest of the data
  lap <- get_data("lap")
  event <- get_data("event")
  hrv <- get_data("hrv")
  device_info <- get_data("device_info")
  
  #get the year that the file was made
  year <- str_extract(string = as.character(file_info$time_created), pattern = "[:digit:]{4}")
  year <- as.numeric(year)
  
  #don't read in data older than 2020
  if(year < 2020){
    print(paste0("file #", i, " was made in ", year, "and isn't newer than 2020. Skipping."))
    next
  }
  
  #merge sport and file_info because they're both always one row
  file_info <- bind_cols(file_info, sport, .name_repair = "unique")

  #determine whether HRM was connected
  file_info$hrm_connect <- hrm_connected(device_info)
  
  time_created <- as.character(file_info$time_created)
  time_created <- str_replace_all(string = time_created, pattern = "[\\s|:|-]", replacement = "_")
    #convert POSIXct to character and replace special characters
  
  sink(nullfile()) #needed to suppress console output
  lapply(data_to_write, write_csv_path)
  sink() #revert back to normal console output
  
  #clear out sport to make sure record data is processed correctly
  sport <- NULL 
}

#### Notes ####
#Possible file types:
#"file_id" device serial number, time_created, product number (not much else useful)
#"device_settings" not helpful
#"user_profile"    not helpful
#"zones_target"    not helpful
#"sport"           name, sport, and sub_sport describe the activity (running, cycling, etc.)
#"session"        #overall session info, could be useful but not overly so since a lot of data is bad. 
#"lap"          #record data by lap
#"record"       #data every second, will be length of two if sensor disconnects, see comment below   
#"event"        #records events but not sure what they mean.    
#"device_info"    #little confusing but might have some info on when HRM disconnects 
#"activity"        #not helpful
#"file_creator"   #software and hardware versions

