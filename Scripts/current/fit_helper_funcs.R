if(!requireNamespace("FITfileR")) {
  remotes::install_github("grimbough/FITfileR")
}

library(FITfileR); library(tidyverse)
library(elevatr); library(sp); library(sf)

#function to catch errors thrown when data type doesn't exist
catch_missing_field <- function(x, type = "hrv") {
  tryCatch({ret <- getMessagesByType(x, type);}, error = function(e) {ret <<- NA});
  ret
}

#function to catch the error when there's multiple rows in record
attempt_record <- function(fit_file){
  
  #before attempting anything, clear out previous record data
  record <- NULL
  
  record <- suppressMessages(records(fit_file)) %>% 
    bind_rows()
    
  return(record)
}

#function to tell if something is POSIXct
is.POSIXct <- function(x) inherits(x, "POSIXct")

#function to convert all POSIX columns to CST instead of UTC
#written using .data to allow for piping
convert_time_cols <- function(.data){
  
  col_types <- lapply(.data, class)
  
  where_time <- grep("POSIX", col_types)
  
  time_cols <- names(col_types[where_time])
  
  .data[time_cols] <- lubridate::with_tz(.data[time_cols], tzone = "America/Chicago")
  
  .data
}

#function to determine whether HRM was connected or not
hrm_connected <- function(x){
  
  # >0 is probably HRM
  serial_num <- length(which(x$serial_number == 645613190))
  
  # >0 is probably HRM
  device_maker <- length(grep("polar", x$manufacturer))
  
  # >0 is probably HRM
  ant_connect <- length(which((x$ant_network == "antplus") == TRUE))
  
  #make list of criteria
  hrm_connection_criteria <- c(serial_num, device_maker, ant_connect)
  
  #if at least 1 of the criteria are met, assume the HRM was connected
  hrm_connected <- length(which(hrm_connection_criteria > 0)) > 1
  
  return(hrm_connected)
}

#adds fit_id as a column called "key" to the data frame
add_key <- function(.data, fit_id){
  
  .data["key"] <- fit_id
  
  .data
  
}

#function to initially read in data for relevant fields
get_data <- function(type_to_get = "file_id", fit_file){
  
  if(type_to_get == "file_id"){
    file_info <- file_id(fit_file) %>% 
      convert_time_cols()
    return(file_info)
  }
  
  if(type_to_get == "event"){
    event <- events(fit_file)  %>% 
      convert_time_cols()
    return(event)
  }
  
  if(type_to_get == "lap"){
    lap <- laps(fit_file) %>% 
      convert_time_cols()
    return(lap)
  }
  
  if(type_to_get == "sport"){
    sport <- catch_missing_field(fit_file, "sport")  %>% 
      convert_time_cols()
    return(sport)
  }
  
  if(type_to_get == "hrv"){
    
    hrv <- catch_missing_field(fit_file, "hrv") 
    #won't have any POSIX columns
    
    #if hrv data exists, do some simple cleaning
    hrv_data_na <- pull(as.data.frame(unique(is.na(hrv))))
    
    if(hrv_data_na == FALSE){
      
      hrv <- hrv %>% 
        mutate(rr = ifelse(time > 20, NA, time)) %>% 
        drop_na() %>% 
        mutate(time_sec = cumsum(rr))
      
      #non-RR interval values get filled with 65, so this removes them
    }
    
    
    return(hrv)
  }
  
  if(type_to_get == "device_info"){
    device_info <- catch_missing_field(fit_file, "device_info")  %>% 
      convert_time_cols()
    return(device_info)
  }
  
}

#Moved record data processing into its own function
get_record <- function(fit_file){
  #browser()

  #catches error seen with developer fields not being read by package
  record <- attempt_record(fit_file) %>% 
    convert_time_cols()
  
  if(nrow(record) > 0){
    record <- record %>% 
      mutate(timestamp = lubridate::with_tz(timestamp, tzone = "America/Chicago")) %>% 
      arrange(timestamp)
  }
  
  #read sport data in here to help with data processing
  sport <- get_data("sport", fit_file)
  
  is_indoor <- NULL
  is_run <- NULL
  
  #If sport returns a data frame and there aren't any NAs
    #Then determine the type of activity
  if(all(is.data.frame(sport), any(is.na(sport)))){
    is_indoor <- str_detect(tolower(sport$name), "indoor")
    is_run <- str_detect(tolower(sport$name), "run")
  }
  
  #if record data exists, process it
  if(all((is.data.frame(record)), (nrow(record) > 0))){
    #Create variable with t=0 so elapsed time can be calculated
    init_time <- record$timestamp[1]
    
    #Are there position columns in the record data?
    is_position <- any(str_detect(colnames(record), "position"))
    
    #There are some files where record data exists but sport data doesn't
      #When sport data doesn't exist, is_indoor and is_run get set to NULL
      #For these, set is_indoor to TRUE and is_run to TRUE
    if(is.null(is_indoor)){
      is_indoor <- FALSE
    }
    
    if(is.null(is_run)){
      is_run <- TRUE
    }
    
    #Indoor data doesn't have position or altitude columns
    #Some other data (usually "other" workout files) also don't have the right columns
    #Need to create these columns to make sure the next block of code still runs
    
    if(is_indoor == TRUE | is_position == FALSE){
      record <- record %>% 
        arrange(timestamp) %>% 
        mutate(position_lat = replicate(n = nrow(record), NA),
               position_long = replicate(n = nrow(record), NA),
               altitude = replicate(n = nrow(record), 0),
               fractional_cadence = replicate(n = nrow(record), 0)) %>% 
        mutate(across(where(is.logical), as.numeric)) #position cols need to be a double for case_when to work
      
    }
    
    #Non-running data has different formatting than running data
    if(is_run == FALSE){
      #format and arrange data
      record <- record %>% 
        arrange(timestamp) %>% 
        mutate(time_elapsed = timestamp - init_time,
               time_elapsed = as.numeric(time_elapsed)) %>% 
        rename(latitude = position_lat, #rename to work better with GIS libraries 
               longitude = position_long) %>% 
        mutate(timestamp = lubridate::with_tz(timestamp, tzone = "America/Chicago")) %>% #make sure time zone is always CST
        relocate(longitude, latitude, timestamp) %>% #move lat/long to beginning for GIS
        arrange(time_elapsed)
    } else{
      #similar to above except also removes fractional cadence
      record <- record %>% 
        arrange(timestamp) %>% 
        mutate(time_elapsed = timestamp - init_time,
               time_elapsed = as.numeric(time_elapsed)) %>% 
        rename(latitude = position_lat, #rename to work better with GIS libraries 
               longitude = position_long) %>% 
        select(-fractional_cadence) %>% #no useful information in this column
        mutate(timestamp = lubridate::with_tz(timestamp, tzone = "America/Chicago")) %>% #make sure time zone is always CST
        relocate(longitude, latitude, timestamp) %>% #move lat/long to beginning for GIS
        arrange(time_elapsed)
    }
    
    #find time_elapsed for rows missing lat/long data
    missing_geo_data <- record %>% 
      filter(is.na(latitude) | is.na(longitude)) %>% 
      pull(time_elapsed)
    
    #gets rows where lat/long is missing
    missing_geo_idx <- which(record$time_elapsed %in% missing_geo_data)
    
    if(length(missing_geo_idx) > 0){
      #confirm missing_geo_idx selection was correct
      long_na <- all(is.na(record[missing_geo_idx,]$longitude))
      lat_na <- all(is.na(record[missing_geo_idx,]$latitude))
      
      if(long_na == FALSE | lat_na == FALSE){
        message("Missing Geo IDX didn't match all the right rows")
        break
      }  
    }
    
    # #time_elapsed starts at zero but row numbers start at 1. Add 1 to time_elapsed to correct this
    # missing_geo_data <- missing_geo_data + 1
    
    #If workout was outdoors AND has position data, find elevation
    #Otherwise, make up an elevation column and fill with zeros
    #This condition is important, because "other" activities aren't technically indoors but don't have position data
    if(is_indoor == FALSE & is_position == TRUE){
      
      
      
      record[missing_geo_idx,]$latitude <- median(record$latitude, na.rm = TRUE)
      record[missing_geo_idx,]$longitude <- median(record$longitude, na.rm = TRUE)
      
      
      
      #Convert to sf  
      record <- record %>% st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)
      
      #catch points that are unrealistically far apart from each other
      record <- record %>% 
        mutate(lat_long_dist = map2_dbl(
        geometry,
        lead(geometry), 
        ~ sf::st_distance(.x, .y) # Calculate distance
      ))
      
      #filter out elevation points that are incorrect
        #typically just one second at a time where the GPS defaults to (180,180)
        #this caused problems, because then elevation had to be downloaded 
          #for an unreasonably large area
        #could come back and impute the lat/long data, but for now this will work
      record <- record %>% 
        filter(lat_long_dist < 50) %>% 
        select(-lat_long_dist) #no longer needed
      
      #get elevation data
      tryCatch({
        #if no error, use AWS data at zoom=14 to pull elevation data
        record$elevation <- suppressMessages(get_elev_point(record, 
                                            src = "aws", 
                                            z = 14, 
                                            units = "meters")$elevation)
      }, error = function(e) {
        
        #if there's an error, try zooming out to use less data
        warning("Running elevation correction with less accuracy")
        record$elevation <- suppressMessages(get_elev_point(record, 
                                            src = "aws", 
                                            z = 5, 
                                            units = "meters")$elevation)
        
      })
      
      #convert record back to tibble and drop unneeded columns
      record <- record %>% 
        mutate(latitude = unlist(purrr::map(geometry,2)), #split geometry list back into lat/long
               longitude = unlist(purrr::map(geometry,1))) %>% 
        select(-altitude) %>%  #get rid of old altitude data
        as_tibble() %>% 
        arrange(time_elapsed)
      
      record$geometry <- NULL #get rid of geometry now that lat/long are split up again
      
    } else{ #indoor data
      record <- record %>% 
        select(-altitude) %>%  #get rid of old altitude data
        as_tibble() %>%  #make sure it's a tibble
        arrange(time_elapsed) %>% 
        mutate(elevation = 0) #empty elevation column
    }
    
    #add NAs back to lat/long data where it was originally missing
    record[missing_geo_idx,]$latitude <- NA
    record[missing_geo_idx,]$longitude <- NA
    
  }
  
  #clear out missing_geo_idx and missing_geo_data just in case
  missing_geo_idx <- NULL 
  missing_geo_data <- NULL 
  
  return(record)
  
}

#function to write each data frame to a CSV
#includes logic to make sure the data exists and has enough rows
#also adds primary keys using fit_id
write_csv_path <- function(type = "file_info", time_char, .data, fit_id){
  
  #time_char will represent time_created
  
  # browser()
  #Find where type is in .data
  idx_type <- which(names(.data) %in% type)
  
  #Label the data associated with type as the item to be written to file
  item_to_write <- .data[[idx_type]]
  
  path <- paste0("./Data/processed_fit/", type,"/", as.character(time_char), "_", type, ".csv")
  #type is in the path twice, once in the folder structure and once in the file name
  
  #need to use get() so that R recognizes type as a file and not a character
  
  # all((is.data.frame(data.frame(1,1,1))), (ncol(data.frame(1,1,1)) > 1))
  
  #only writes CSV if data exists & has rows to write
  if(all((is.data.frame(item_to_write)), (nrow(item_to_write) > 0))){
     
    # browser()
    
    #remove any columns that are lists and designates remaining data as clean
    if (item_to_write %>% select_if(is.list) %>% ncol() > 0){
      list_col <- item_to_write %>% select_if(is.list) %>% names()
      idx_list_col <- which(colnames(item_to_write) == list_col)
      item_to_write <- item_to_write[-idx_list_col]
    } 
    
    #now writes the data to a CSV
    #don't add unnecessary columns to record data b/c it has lots of rows
    if(type == "record"){
      
      item_to_write %>% 
        add_key(fit_id) %>% #just add primary key
        readr::write_csv(., file = path)
      
    } else { #but it's fine to add them to all other data types
      
      item_to_write %>% 
        add_key(fit_id) %>% #add primary key
        mutate(time_created = time_char) %>%  #add time created 
        readr::write_csv(., file = path)   
    }   
   
  }
  
  rm(item_to_write, idx_type, path)
}
