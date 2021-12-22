
source("./Scripts/2022/initial_mass_import_helper_funcs.R")

#### Automate Transfer/Extraction of Zip Files ####
download_location <- "./Data/zip_fit_files/"
storage_location <- "./Data/raw_fit_files"

# storage_location <- "./Data/processed_fit_files" #fit files go here once analyzed

zip_files <- list.files(path = download_location, pattern = "*.zip", full.names = TRUE)

lapply(zip_files, unzip, exdir = storage_location)

#Make a data frame with names of all the fit files that have been extracted
file_names <- as_tibble(zip_files) %>%
  rename(fit_name = value) %>%
  mutate(fit_name = str_remove(fit_name, download_location),
         fit_name = str_remove(fit_name, ".zip"))

#Write this to a file to compare for future imports
write_csv(file_names, file = "./Data/fit_files_already_extracted.csv")

#Now that zip files have been extracted, they can be deleted
file.remove(zip_files)

#### Read in Data from Fit Files ####

#list all files ready to be processed
fit_files <- list.files(path = storage_location, pattern = ".fit", full.names = TRUE)

#start by making everything into FitFile objects
all_data <- lapply(fit_files, readFitFile)

data_to_write <- list("file_info", "record", "lap", "event", "hrv", "device_info")

for (i in 1:length(all_data)){
  
  fit_id <- str_match_all(string = fit_files[[i]], 
                          pattern = "[0-9]+") %>% 
    unlist() %>% 
    as.numeric()
  
  #get data
  file_info <- get_data("file_id")
  record <- get_data("record")
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
  
  sink(nullfile())
  lapply(data_to_write, write_csv_path)
  sink()
  
  
  # write_csv_path("file_info")
  # write_csv_path("record")
  # write_csv_path("lap")
  # write_csv_path("event")
  # write_csv_path("hrv")
  
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

