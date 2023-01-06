#This script holds a function to clean fit files that come in from Garmin Connect
#Helper functions are contained in the script below

source("./Scripts/current/fit_helper_funcs.R")

process_fit_data <- function(fit_file){
  # browser()
  
  fit_id <- str_match_all(string = fit_file, 
                          pattern = "[0-9]+") %>% 
    unlist() %>% 
    as.numeric()
  
  my_file <- FITfileR::readFitFile(fit_file)
  
  msg_types <- listMessageTypes(fitFile = my_file)
  
  #get preliminary data
  file_info <- get_data("file_id", fit_file = my_file)
  sport <- get_data("sport", fit_file = my_file) #need to be before record to determine indoor/outdoor
  
  #use is_indoor to determine how record data is processed
  record <- get_record(my_file)
  
  #get the rest of the data
  lap <- get_data("lap", fit_file = my_file)
  event <- get_data("event", fit_file = my_file)
  device_info <- get_data("device_info", fit_file = my_file)
  
  #If HRV data is there, read it in and assume HRM was connected
  if("hrv" %in% msg_types){
    hrv <- get_data("hrv", fit_file = my_file)  
    file_info$hrm_connect <- TRUE
  } else{
    file_info$hrm_connect <- hrm_connected(device_info)
    hrv <- NA
  }
  
  #get the year that the file was made
  year <- str_extract(string = as.character(file_info$time_created), pattern = "[:digit:]{4}")
  year <- as.numeric(year)
  
  #don't read in data older than 2020
  if(year < 2020){
    next
  }
  
  #merge sport and file_info because they're both always one row
  file_info <- bind_cols(file_info, sport, .name_repair = "unique")
  
  #saves the time when the file was created as a variable to use in the file name
  time_created <- as.character(file_info$time_created)
  time_created <- str_replace_all(string = time_created, pattern = "[\\s|:|-]", replacement = "_")
    #convert POSIXct to character and replace special characters
  
  obj_to_write <- list(file_info, record, lap, event, hrv, device_info)
  names(obj_to_write) <- c("file_info", "record", "lap", "event", "hrv", "device_info")
  
  sink(nullfile()) #needed to suppress console output
  
  
  lapply(c("file_info", "record", "lap", "event", "hrv", "device_info"),
         write_csv_path,
         time_char = time_created,
         .data = obj_to_write,
         fit_id = fit_id)
 
  # write_csv_path("file_info", time_created, .data = obj_to_write[[1]], fit_id = fit_id)
  # write_csv_path("record", time_created, .data = obj_to_write[[2]], fit_id = fit_id)
  
  sink() #revert back to normal console output
  
  #clear out sport to make sure record data is processed correctly
  sport <- NULL 
}
