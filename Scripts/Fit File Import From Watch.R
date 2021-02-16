#Automated import of FIT directly files from the watch

library(FITfileR); library(tidyverse); library(openxlsx)

#Creates functions to merge one-row data into sum_data data frame
CondenseDataFromWatch <- function(data, i){
  data[[i]]$sum_data <- cbind(data[[i]]$file_id, data[[i]]$file_creator, data[[i]]$device_settings, data[[i]]$zones_target, data[[i]]$user_profile, data[[i]]$sport, data[[i]]$session, data[[i]]$activity)
  return(data[[i]]$sum_data)
}

CondenseDataFromConnect <- function(data, i){
  data[[i]]$sum_data <- cbind(data[[i]]$file_id, data[[i]]$file_creator, data[[i]]$activity)
  return(data[[i]]$sum_data)
}

#Function to calculate TRIMP off of record data

TRIMP.func <- function(data, i) {
  HRfreq <- data[[i]]$record %>% #data frame with frequency of each heart rate measure
    count(heart_rate) 
  names(HRfreq)[1] <- "HR" #Set name of first column to HR
  names(HRfreq)[2] <- "number" #Set name of second column to number
  HRfreqHRR <- HRfreq %>%
    mutate(HRR = ((HRfreq$HR-48)/(195-48))) #Create HRR column
  rm(HRfreq) #Remove data frame with HR freq since it's not needed
  HRfreqTRIMP <- HRfreqHRR %>%
    mutate(TRIMP = (HRfreqHRR$number / 60) * HRfreqHRR$HRR * (0.64 * exp(1.92*HRfreqHRR$HRR))) #Now apply TRIMP formula
  TotalTRIMP <- sum(HRfreqTRIMP$TRIMP, na.rm = TRUE) #TRIMP value for the activity
  TimeMinutes <- data[[i]]$activity$total_timer_time/60 #Makes character with total activity time
  TotalTRIMP.hour <- TotalTRIMP/TimeMinutes #Scales TRIMP to per hour metric
  rm(HRfreqHRR) #Removes HRR data frame
  
  #Writes TRIMP data to activity file
  data[[i]]$activity["TRIMP"] <- TotalTRIMP 
  data[[i]]$activity["TRIMP.hour"] <- TotalTRIMP.hour
  data[[i]]$activity["TimeMinutes"] <- TimeMinutes
  
  return(data[[i]]$activity) #Returns activity data frame so it can be written into Global Environment by the for loop below
  
}

# Import Data ---------------------------------------------------
watch_path <- "E:\\GARMIN\\ACTIVITY\\"
alt_watch_path <- "L:\\GARMIN\\ACTIVITY\\"

#Check if the watch is plugged in
if(file.access(names = watch_path, mode=4) == 0){
  print("Watch detected at Drive E. Continuing")
  continue <- T
} else if(file.access(names = alt_watch_path, mode=4) == 0){
  print("Watch detected at Drive L. Continuing")
  watch_path <- alt_watch_path
  continue <- T
} else{
  print("GPS Watch Not Detected at Drive E or Drive L. Should stop.")
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
  
  #Setup parallel processing
  library(doParallel); c <- detectCores()
  c <- round((c/3),0)
  c <- c+4
  c1 <- makeCluster(c)
  
  #Creates an empty list to be filled with data frames
  Fit.DF <- vector('list', n)
  
  #Read in files from files_to_read; convert read-in file to a data frame and store it in a list; 
  #start <- proc.time()
  
  Fit.DF <- foreach(i=1:n) %dopar% {
    #Reads in each fit file then puts fit files into a list as data frames
    IndivFitFile <- FITfileR::readFitFile(files_to_read[i])
    Fit.DF[[i]] <- lapply(IndivFitFile, data.frame, stringsAsFactors = FALSE)
    
    test_record <- FITfileR::records(files_to_read[1])
    list.files(system.file("extdata", "Activities", package = "FITfileR"))
    fenix6_file <- system.file("extdata", "Activities", "garmin-fenix6-swim.fit", 
                               package = "FITfileR")
    fenix6 <- readFitFile(fenix6_file)
    test_file <- "L:/Garmin/ACTIVITY/ACO81542.FIT"
    fenix6_records <- records(fenix6)
    listMessageTypes(fenix6)
    print("yes")
    getMessagesByType(fenix6, message_type = "sport")
    
  }
  
  #Sometimes parallel processing doesn't work. If so, just run a normal for loop
  if(is.null(Fit.DF[[1]]) == T){
    print("Parallel processing didn't work. Running normal for loop instead")
    Fit.DF <- for(i in 1:n){
      IndivFitFile <- fitFileR::readFitFile(files_to_read[i]) 
      Fit.DF[[i]] <- lapply(IndivFitFile, data.frame, stringsAsFactors = FALSE) 
    }
    
    ?fitFileR::readFitFile()
  }
  
  #do_par_loop <- proc.time()-start
  
  n <- length(Fit.DF)
  
  # Organize Data -----------------------------------------------------------
  
  for(i in 1:n) {
    
    # Created by the watch ----------------------------------------------------
    
    #If the file was created by the watch, as most are
    if (any(head(Fit.DF[[i]]$device_info$product, 1) == "fr235") == TRUE) {
      
      #If HR data exists, calculate TRIMP
      #Plan on further differentiating this later on
      if (any(names(Fit.DF[[i]]$record) == "heart_rate") == TRUE){
        Fit.DF[[i]]$activity <- TRIMP.func(Fit.DF, i) #Calculate TRIMP and write to activity in the Global Environment
      }
      
      if (any(names(Fit.DF[[i]]$record) == "heart_rate") == FALSE){
        cat(i, " no heart rate column ")
        #head(Fit.DF[[200]]$record, 1)
      }
      
      #Prevents error when for loop is run more than once over the same set of data
      if (any(names(Fit.DF[[i]]) == "sum_data") == FALSE) {
        
        #If device settings doesn't exist, let me know and then delete the file
        #Maybe circle back to this and do something to deal with it
        if (any(names(Fit.DF[[i]]) == "device_settings") == FALSE) {cat(i, "no device settings ")
          Fit.DF[[i]] <- NULL
        }
        
        #If device settings exists
        if (any(names(Fit.DF[[i]]) == "device_settings") == TRUE) {
          #Makes a list of data to summarize & confirms the data is all 1 row
          sum_list <- list(Fit.DF[[i]]$file_id, Fit.DF[[i]]$file_creator, Fit.DF[[i]]$device_settings, 
                           Fit.DF[[i]]$zones_target, Fit.DF[[i]]$user_profile, Fit.DF[[i]]$sport, 
                           Fit.DF[[i]]$activity, Fit.DF[[i]]$session)
          sum_rownum2 <- mapply(nrow,sum_list) #Creates integer with number of rows for each data frame in sum_list
          
          checkrowsum <- sum(sum_rownum2) #Sum of the rows, should equal 7
          checkrowprod <- prod(sum_rownum2) #Product of the rows, should equal 1
          
          #If sum and product are good, pass Fit.DF[[i]] to Condense Data, otherwise print error
          ifelse(checkrowsum == 8 & checkrowprod == 1,CondenseDataFromWatch(Fit.DF, i),cat(i, "CondenseDataWatch error"))
          
          #Write sum_data to the Global Environment
          Fit.DF[[i]]$sum_data <- CondenseDataFromWatch(Fit.DF, i)
          
          #Clear out data already summarized in sum_data
          Fit.DF[[i]]$file_id <- NULL #Set it to NULL, which removes it 
          Fit.DF[[i]]$file_creator <- NULL
          Fit.DF[[i]]$device_settings <- NULL
          Fit.DF[[i]]$zones_target <- NULL 
          Fit.DF[[i]]$user_profile <- NULL 
          Fit.DF[[i]]$sport <- NULL 
          Fit.DF[[i]]$activity <- NULL
          Fit.DF[[i]]$session <- NULL
          
        }
      }
      
      #Clean up developer fields
      if (length(pluck(Fit.DF, i)$developer_data_id) > 0){ #If developer_data_id exists...
        Fit.DF[[i]]$developer_data_id <- NULL #Set it to NULL, which removes it
      }
      if (length(pluck(Fit.DF, i)$field_description) > 0){ #If field_description exists...
        Fit.DF[[i]]$field_description <- NULL #Set it to NULL, which removes it
      }
      if (length(pluck(Fit.DF, i)$training_file) > 0){ #If training_file exists...
        Fit.DF[[i]]$training_file <- NULL #Set it to NULL, which removes it
      }
      #ifelse(length(pluck(Fit.DF, i)$sum_data) > 0, next, cat(i, "something wrong with sum_data"))
      #Print error and i value if sum_data hasn't been made
    }
    
    
    # Created by Connect ------------------------------------------------------
    
    #If the file was created by connect manually
    if (any(head(Fit.DF[[i]]$device_info$product, 1) == "connect") == TRUE) {
      
      #If summary data doesn't exist, make it
      if (any(names(Fit.DF[[i]]) == "sum_data") == FALSE) {
        
        #missing zones_target, device_settings, user_profile, sport, record, event
        #Makes a list of data to summarize & confirms the data is all 1 row
        sum_list <- list(Fit.DF[[i]]$file_id, Fit.DF[[i]]$file_creator, Fit.DF[[i]]$activity, Fit.DF[[i]]$session)
        sum_rownum2 <- mapply(nrow,sum_list) #Creates integer with number of rows for each data frame in sum_list
        
        checkrowsum <- sum(sum_rownum2) #Sum of the rows, should equal 2
        checkrowprod <- prod(sum_rownum2) #Product of the rows, should equal 1
        
        #If sum and product are good, pass Fit.DF[[i]] to Condense Data, otherwise print error
        ifelse(checkrowsum == 4 & checkrowprod == 1,CondenseDataFromConnect(Fit.DF, i), 
               cat(i, "CondenseDataConnect didn't work here"))
        
        #Write sum_data to the Global Environment
        Fit.DF[[i]]$sum_data <- CondenseDataFromConnect(Fit.DF, i)
        
        #Fixes mistake in creation time year, though it's still usually off by a day or so
        Fit.DF[[i]]$sum_data$time_created <- Fit.DF[[i]]$session$start_time
        
      }
      
      #If summary data is already made, remove extra data
      if (any(names(Fit.DF[[i]]) == "sum_data") == TRUE) {
        #Clear out data already summarized in sum_data
        Fit.DF[[i]]$file_id <- NULL #Set it to NULL, which removes it
        Fit.DF[[i]]$file_creator <- NULL #Set it to NULL, which removes it
        Fit.DF[[i]]$session <- NULL
      }
      
      #Remove dive_settings if it's there
      if (any(names(Fit.DF[[i]]) == "dive_settings") == TRUE) {
        Fit.DF[[i]]$dive_settings <- NULL
      }
      
      
      #Make filler record df if needed
      if (any(names(Fit.DF[[i]]) == "record") == FALSE) {
        
        Fit.DF[[i]]$record <- head(Fit.DF[[i-1]]$record, 1) #Copy data from the previous 
        Fit.DF[[i]]$activity$timestamp <- Fit.DF[[i]]$record$timestamp #Replace with correct time stamp
        Fit.DF[[i]]$record[,2:ncol(Fit.DF[[i]]$record)] <- -1 #Clear out all other data
      }
      
      #Make filler event df if needed
      if (any(names(Fit.DF[[i]]) == "event") == FALSE) {
        Fit.DF[[i]]$event <- head(Fit.DF[[i-1]]$event, 1) #Copy data from the previous 
        Fit.DF[[i]]$activity$timestamp <- Fit.DF[[i]]$event$timestamp #Replace with correct time stamp
        Fit.DF[[i]]$event[,2:ncol(Fit.DF[[i]]$event)] <- -1 #Clear out all other data
      }
      
      #Clear out activity
      if (any(names(Fit.DF[[i]]) == "activity") == TRUE) {
        Fit.DF[[i]]$activity <- NULL
      }
    }
    
    
  }
  
  # Fill In Columns --------------------------------------------------
  
  for (i in 1:n) {
    
    # SumData -----------------------------------------------------------------
    load(".\\Objects\\sumdataNames.Rdata")
    
    if (all(names(sumdataNames) %in% names(Fit.DF[[i]]$sum_data)) == FALSE) {
      #Returns false if names in sumdata don't match 
      
      sumdataNamesToAdd <- c(setdiff(names(sumdataNames), names(Fit.DF[[i]]$sum_data))) #Identify names to add
      z <-  length(sumdataNamesToAdd) #Set z as the # of columns to be added
      df <- setNames(data.frame(matrix(ncol = z, nrow = 1)), sumdataNamesToAdd) #Make data frame df with missing names
      df[,1:z] <- -1 #Set all values in df to -1
      Fit.DF[[i]]$sum_data <- add_column(Fit.DF[[i]]$sum_data, df) #Add columns of -1 to the data frame
      remove(z, df, sumdataNamesToAdd, sumdataNames)
      
    }
    # Device Info --------------------------------------------------------------
    load(".\\Objects\\deviceinfoNames.Rdata")
    if (all(names(device_infoNames) %in% names(Fit.DF[[i]]$device_info)) == FALSE) {
      #Returns false if names in device_info don't match 
      
      device_infoNamesToAdd <- c(setdiff(names(device_infoNames), names(Fit.DF[[i]]$device_info))) #Identify names to add
      z <-  length(device_infoNamesToAdd) #Set z as the # of columns to be added
      df <- setNames(data.frame(matrix(ncol = z, nrow = 1)), device_infoNamesToAdd) #Make data frame df with missing names
      df[,1:z] <- -1 #Set all values in df to -1
      Fit.DF[[i]]$device_info <- add_column(Fit.DF[[i]]$device_info, df) #Add columns of -1 to the data frame
      remove(z, df, device_infoNamesToAdd,device_infoNames)
    }
    # Event -------------------------------------------------------------------
    load(".\\Objects\\eventNames.Rdata")
    if (all(names(eventNames) %in% names(Fit.DF[[i]]$event)) == FALSE) {
      #Returns false if names in lap don't match 
      
      eventNamesToAdd <- c(setdiff(names(eventNames), names(Fit.DF[[i]]$event))) #Identify names to add
      z <-  length(eventNamesToAdd) #Set z as the # of columns to be added
      df <- setNames(data.frame(matrix(ncol = z, nrow = 1)), eventNamesToAdd) #Make data frame df with missing names
      df[,1:z] <- -1 #Set all values in df to -1
      Fit.DF[[i]]$event <- add_column(Fit.DF[[i]]$event, df) #Add columns of -1 to the data frame
      remove(z, df, eventNamestoAdd,eventNames)
    }
    # Lap ---------------------------------------------------------------------
    load(".\\Objects\\LapNames.Rdata")
    if (all(names(LapNames) %in% names(Fit.DF[[i]]$lap)) == FALSE) {
      #Returns false if names in lap don't match 
      
      lapNamesToAdd <- c(setdiff(names(LapNames), names(Fit.DF[[i]]$lap))) #Identify names to add
      z <-  length(lapNamesToAdd) #Set z as the # of columns to be added
      df <- setNames(data.frame(matrix(ncol = z, nrow = 1)), lapNamesToAdd) #Make data frame df with missing names
      df[,1:z] <- -1 #Set all values in df to -1
      Fit.DF[[i]]$lap <- add_column(Fit.DF[[i]]$lap, df) #Add columns of -1 to the data frame
      remove(z,df,lapNamesToAdd,LapNames)
    }
    # Record ------------------------------------------------------------------
    load(".\\Objects\\RecordNames.Rdata")
    
    if (all(names(RecordNames) %in% names(Fit.DF[[i]]$record)) == FALSE) {
      #Returns false if names in record don't match 
      
      RecordNamesToAdd <- c(setdiff(names(RecordNames), names(Fit.DF[[i]]$record))) #Identify names to add
      z <-  length(RecordNamesToAdd) #Set z as the # of columns to be added
      df <- setNames(data.frame(matrix(ncol = z, nrow = 1)), RecordNamesToAdd) #Make data frame df with missing names
      df[,1:z] <- -1 #Set all values in df to -1
      Fit.DF[[i]]$record <- add_column(Fit.DF[[i]]$record, df) #Add columns of -1 to the data frame
      remove(z, df, RecordNamesToAdd, RecordNames)
    }
    
  }
  
  # Alphabetize Columns -----------------------------------------------------
  
  for (i in 1:n) {
    Fit.DF[[i]]$record <- Fit.DF[[i]]$record[ , order(names(Fit.DF[[i]]$record))] 
    Fit.DF[[i]]$lap <- Fit.DF[[i]]$lap[ , order(names(Fit.DF[[i]]$lap))] 
    Fit.DF[[i]]$event <- Fit.DF[[i]]$event[ , order(names(Fit.DF[[i]]$event))]
    Fit.DF[[i]]$device_info <- Fit.DF[[i]]$device_info[ , order(names(Fit.DF[[i]]$device_info))] 
    Fit.DF[[i]]$sum_data <- Fit.DF[[i]]$sum_data[ , order(names(Fit.DF[[i]]$sum_data))]
  }
  
  # Add Timestamp ID --------------------------------------------------------
  
  #clear out duplicate timestamp from sum_data merge
  for (i in 1:length(Fit.DF)){
    Fit.DF[[i]]$sum_data$timestamp.1 <- NULL
    Fit.DF[[i]]$sum_data$event_group.1 <- NULL
    Fit.DF[[i]]$sum_data$event_type.1 <- NULL
    Fit.DF[[i]]$sum_data$event_type.1 <- NULL
    Fit.DF[[i]]$sum_data$sport.1 <- NULL
    Fit.DF[[i]]$sum_data$sub_sport.1 <- NULL
    Fit.DF[[i]]$sum_data$total_timer_time.1 <- NULL
  }
  
  #Add IDs as long as sum_data$timestamp isn't -1
  for(i in 1:length(Fit.DF)){
    if (any(Fit.DF[[i]]$sum_data$timestamp == -1) == TRUE) {print(i)}
    else{
      Fit.DF[[i]]$record <- cbind(ID = Fit.DF[[i]]$sum_data$timestamp, Fit.DF[[i]]$record)
      Fit.DF[[i]]$lap <- cbind(ID = Fit.DF[[i]]$sum_data$timestamp, Fit.DF[[i]]$lap)
      Fit.DF[[i]]$sum_data <- cbind(ID = Fit.DF[[i]]$sum_data$timestamp, Fit.DF[[i]]$sum_data)
      Fit.DF[[i]]$event <- cbind(ID = Fit.DF[[i]]$sum_data$timestamp, Fit.DF[[i]]$event)
      Fit.DF[[i]]$device_info <- cbind(ID = Fit.DF[[i]]$sum_data$timestamp, Fit.DF[[i]]$device_info)
    }
    
  }
  
  
  
  # Process HRV Data --------------------------------------------------------
  
  for (i in 1:length(Fit.DF)){
    hrv <- unlist(Fit.DF[[i]]$hrv$time)
    hrv <- as.data.frame(hrv)
    Fit.DF[[i]]$hrv <- hrv
  }
  
  # Export to Excel --------------------------------------------------------
  
  #folder for saving CSVs
  save_here <- ".\\Data\\ExportedRunData\\Cleaned_CSVs\\"
  
  foreach(i=1:length(Fit.DF)) %dopar% {
    
    #Creates character with timestamp for name of file
    time_char <- as.character(Fit.DF[[i]]$sum_data$timestamp)
    time_char <-  gsub("[[:punct:]]", "_", time_char)
    
    #Makes names for each csv to export
    recordfile <- paste0(save_here, time_char, " record", ".csv")
    eventfile <- paste0(save_here, time_char, " event", ".csv")
    lapfile <- paste0(save_here, time_char, " lap", ".csv")
    devinfofile <- paste0(save_here, time_char, " devinfo", ".csv")
    sumdatafile <- paste0(save_here, time_char, " sumdata", ".csv")
    hrvfile <- paste0(save_here, time_char, " hrvdata", ".csv")
    
    write.csv(Fit.DF[[i]]$record, file = recordfile, row.names = FALSE)
    write.csv(Fit.DF[[i]]$event, file = eventfile, row.names = FALSE)
    write.csv(Fit.DF[[i]]$lap, file = lapfile, row.names = FALSE)
    write.csv(Fit.DF[[i]]$device_info, file = devinfofile, row.names = FALSE)
    write.csv(Fit.DF[[i]]$sum_data, file = sumdatafile, row.names = FALSE)
    write.csv(Fit.DF[[i]]$hrv, file = hrvfile, row.names = FALSE)
    
    #Consider rearranging this code to reduce size of Fit.DF as data is copied
  }
  
  
  # Move Files --------------------------------------------------------------
  
  #Copy files_to_read to Old_Fit_Files
  file.copy(from = files_to_read, to = "./Data/ExportedRunData/Old_Fit_Files/")
  
  stopImplicitCluster()
    
}


