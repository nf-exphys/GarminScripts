library(FITfileR); library(tidyverse)

#function to catch errors thrown when data type doesn't exist
catch_missing_field <- function(x, type = "hrv") {
  tryCatch({ret <- getMessagesByType(x, type);}, error = function(e) {ret <<- NA});
  ret
}

#function to catch the error when there's multiple rows in record
attempt_record <- function(i){
  tryCatch({
    record <- records(all_data[[i]]) %>%
      bind_rows();},
    error = function(e) {record <<- NA});
  #as.data.frame(record)
  if(is.null(record)){record <<- NA}
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
  
  #if at least 2 of the criteria are met, assume the HRM was connected
  hrm_connected <- length(which(hrm_connection_criteria > 0)) > 2
  
  return(hrm_connected)
}

#adds fit_id as a column called "key" to the data frame
add_key <- function(.data){
  
  .data["key"] <- fit_id
  
  .data
  
}

#function to initially read in data for relevant fields
#assumes FitFileR data is stored as all_data[[i]]
get_data <- function(type_to_get = "file_id"){
  
  if(type_to_get == "file_id"){
    file_info <- file_id(all_data[[i]]) %>% 
      convert_time_cols()
    return(file_info)
  }
  
  if(type_to_get == "record"){
    record <- attempt_record(i) %>% #catches error seen with developer fields not being read by package
      convert_time_cols() 
    
    #if record data did exist, create time_elapsed var
    if(is.data.frame(record) & nrow(record) > 0){
      
      init_time <- record$timestamp[1]
      
      record <- record %>% 
        mutate(time_elapsed = timestamp - init_time,
               time_elapsed = as.numeric(time_elapsed)) 
      
    }
    
    return(record)
    
  }
  
  if(type_to_get == "event"){
    event <- events(all_data[[i]])  %>% 
      convert_time_cols()
    return(event)
  }
  
  if(type_to_get == "lap"){
    lap <- laps(all_data[[i]]) %>% 
      convert_time_cols()
    return(lap)
  }
  
  if(type_to_get == "sport"){
    sport <- catch_missing_field(all_data[[i]], "sport")  %>% 
      convert_time_cols()
    return(sport)
  }
  
  if(type_to_get == "hrv"){
    
    hrv <- catch_missing_field(all_data[[i]], "hrv") 
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
    device_info <- catch_missing_field(all_data[[i]], "device_info")  %>% 
      convert_time_cols()
    return(device_info)
  }
}

#function to write each data frame to a CSV
#includes logic to make sure the data exists and has enough rows
#also adds primary keys 
write_csv_path <- function(type = "file_info"){
  
  path <- paste0("./Data/processed_fit/", type,"/", as.character(time_created), "_", type, ".csv")
  #type is in the path twice, once in the folder structure and once in the file name
  
  #need to use get() so that R recognizes type as a file and not a character
  
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
      
      #clean_file only exists once the above if/else chunk has been run
      #this part writes the data to a CSV
      if(exists("clean_file")){
        
        #don't add unnecessary columns to record data b/c it has lots of rows
        if(type == "record"){
          
          clean_file %>% 
            add_key() %>% #just add primary key
            write_csv(., file = path)
          
        } else { #but it's fine to add them to all other data types
          
          clean_file %>% 
            add_key() %>% #add primary key
            mutate(time_created = time_created) %>%  #add time created 
            write_csv(., file = path)   
        }   
      }
      
    }
    
  }
  
}