library(FITfileR); library(tidyverse)

data_path <- "./Data/raw_fit_files/"

all_files <- list.files(path = data_path, pattern = ".fit", full.names = TRUE)
some_files <- all_files[1:100]

#one_file <- all_files[[1356]]
#test <- FITfileR::readFitFile(one_file) #reads the file and converts to S4 class
#types <- FITfileR::listMessageTypes(test) #lists file types 

#Ones that matter: file_ID, sport, lap, record, event?

#some tibbles will be split into two because a sensor wasn't connected the whole time.
#bind_rows and then arranging by timestamp should fix this?

#device info might have information on whether HRM was connected or not. 
#Might be when serial number =/= 0?
#look at ~2 mile run on June 3rd 2021

#currently data is divided up by information type (record, lap, etc.)
#might need to be by file with a for loop

catch_missing_field <- function(x, type = "hrv") {
  tryCatch({ret <- getMessagesByType(x, type);}, error = function(e) {ret <<- NA});
  ret
}


write_csv_path <- function(type = "file_info"){
  path <- paste0("./Data/processed_fit/v2_csv/", type,"/", as.character(time_created), "_", type, ".csv")
  
  if(is.data.frame(get(type))){ #only writes CSV if data exists
    if((nrow(get(type)) > 0)){ #and has rows to write
      
      #remove any columns that are lists and writes remaining data to clean_file
      if (get(type) %>% select_if(is.list) %>% ncol() > 0){
        list_col <- get(type) %>% select_if(is.list) %>% names()
        idx_list_col <- which(colnames(get(type)) == list_col)
        clean_file <- get(type)[-idx_list_col]
      } else{
        clean_file <- get(type)
      }
    
      write_csv(clean_file, file = path) #need get() so that R recognizes type as a file and not a character
    }
    
  }
  
  }
  
all_data <- lapply(all_files, readFitFile)
rest_of_data <- all_data[428:length(all_data)]

attempt_record <- function(i){
  tryCatch({
    record <- records(all_data[[i]]) %>%
      bind_rows();},
    error = function(e) {record <<- NA});
  #as.data.frame(record)
}

for (i in 489:length(rest_of_data)){
  file_info <- file_id(all_data[[i]])
  
  record <- attempt_record(i) #catches error seen with developer fields not being read by package
  
  #Error in FUN(X[[i]], ...) : 
  #no slot of name "dev_fields" for this object of class "FitDataMessage"
  # all_data[[i]]
  # getMessagesByType(all_data[[i]], "lap") %>% View()
  #i = 508
  

  # listMessageTypes(all_data[[i]])
  # getMessagesByType(all_data[[i]], "record")
  
  lap <- laps(all_data[[i]])
  event <- events(all_data[[i]])
  sport <- catch_missing_field(all_data[[i]], "sport")
  hrv <- catch_missing_field(all_data[[i]], "hrv")
  
  file_info <- bind_cols(file_info, sport, .name_repair = "unique")
  
  #create primary key
  time_created <- as.character(file_info$time_created)
  record$key <- time_created
  lap$key <- time_created
  event$key <- time_created
  
  #remove columns with list data
  
  
  #convert POSIXct to character and replace special characters
  time_created <- as.character(time_created)
  time_created <- str_replace_all(string = time_created, pattern = "[\\s|:|-]", replacement = "_")
  
  write_csv_path("file_info")
  write_csv_path("record")
  write_csv_path("lap")
  write_csv_path("event")
  write_csv_path("hrv")
  
}

#don't forget to delete Cleaned_CSVs once all of v2_csv has been written

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


#### Writing Data by File Type ####

types
getMessagesByType(test, types[12])
getMessagesByType(test, "record")
fit_files <- map(some_files, readFitFile)
records <- map(fit_files, getMessagesByType("records"))

#code from here on down is copied from V2 script
some_data <- lapply(some_files, readFitFile)

#### File ID ####
file_id <- lapply(some_data, file_id) %>% bind_rows()

#### Record data ####
records <- lapply(some_data, records)

#Catch record activity split into two and bind together
for (i in 1:length(records)){
  if (length(records[[i]]) > 1){
    records[[i]] <- records[[i]] %>% 
      bind_rows() %>%
      arrange(timestamp)
  }  
  
}

#### Lap data ####
laps <- lapply(some_data, laps)

#### Import HRV Data ####

hrv_data <- lapply(some_data, catch_missing_field, type = "hrv")

#Clean up HRV data, not outlier removal
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

sports <- lapply(some_data, catch_missing_field, type = "sport") 

sports <- sports %>%
  map(discard, is.na) %>% #gets rid of list elements that are NA
  compact() %>% #works with above line to remove empty elements
  bind_rows() %>%
  rename_with( ~ paste0("sports_", .x))

#session_start_time = session_start_time - (6*60*60)) #temp workaround for times

events <- lapply(some_data, events) %>% #returns NULL without error if no events 
  map(discard, is.null) %>% #gets rid of list elements that are NA
  compact() %>% #works with above line to remove empty elements
  bind_rows()

