#Import all old Fit Files and Analyze

#All files from 2018 to 08/20/2020

library(fitFileR); library(tidyverse); library(openxlsx)

#Creates functions to merge one-row data into sum_data data frame
CondenseDataFromWatch <- function(data, i){
  data[[i]]$sum_data <- cbind(data[[i]]$file_id, data[[i]]$file_creator, data[[i]]$device_settings, data[[i]]$zones_target, data[[i]]$user_profile, data[[i]]$sport)
  return(data[[i]]$sum_data)
}

CondenseDataFromConnect <- function(data, i){
  data[[i]]$sum_data <- cbind(data[[i]]$file_id, data[[i]]$file_creator)
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

#Function to clean developer fields, still a work in progress
  #Need to think through how to best return modified data to Global Environment
CleanDevField <- function(data, i) {
#if (length(pluck(data, i)$developer_data_id) > 0){ #If developer_data_id exists...
 # data[[i]]$developer_data_id <- NULL #Set it to NULL, which removes it
 #return(data[[i]]$developer_data_id)
#}
#if (length(pluck(data, i)$field_description) > 0){ #If field_description exists...
#  data[[i]]$field_description <- NULL #Set it to NULL, which removes it
#}
#if (length(pluck(data, i)$training_file) > 0){ #If training_file exists...
#  data[[i]]$training_file <- NULL #Set it to NULL, which removes it
#}
}

# Import Data ---------------------------------------------------

FolderWithFitFiles <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\OldFitFiles"

FilesToRead <- list.files(path = FolderWithFitFiles, full.names = TRUE)
#^Creates a list with the fit files in the OldFitFiles folder and their file path (better than pasting in the file path)

#Sets n as the number of files to be read
n <- length(FilesToRead)

#Creates an empty list to be filled with data frames
Fit.DF <- vector('list', n)

  #Read in files from FilesToRead; convert read-in file to a data frame and store it in a list; 
for (i in 1:n) {
  IndivFitFile = readFitFile(FilesToRead[i]) #Reads in each fit file and gives it the name IndivFitFile
  Fit.DF[[i]] <- lapply(IndivFitFile, data.frame, stringsAsFactors = FALSE) #Puts fit files into a list as data frames
}




# For Loop for Troubleshooting --------------------------------------------

#Creates an empty list to be filled with data frames
Timing <- vector('list', n)


for (i in 1:50) {
  ptm <- proc.time()  
  print(i+1)
  proc.time() - ptm  
}


# Organize Data -----------------------------------------------------------

for (i in 1:n) {
  #If the file was created by the watch, as most are
  if (any(head(Fit.DF[[i]]$device_info$product, 1) == "fr235") == TRUE) {
  
    #If HR data exists, calculate TRIMP
      #Plan on further differentiating this later on
    if (any(names(Fit.DF[[i]]$record) == "heart_rate") == TRUE){
    Fit.DF[[i]]$activity <- TRIMP.func(Fit.DF, i) #Calculate TRIMP and write to activity in the Global Environment
    }
    
   #Prevents error when for loop is run more than once over the same set of data
   if (any(names(Fit.DF[[i]]) == "sum_data") == FALSE) {
      
    #If device settings doesn't exist, just go ahead
      #Maybe circle back to this and do something to deal with it
    if (any(names(Fit.DF[[i]]) == "device_settings") == FALSE) {cat(i, "no device settings ")}
    
    #If device settings exists
    if (any(names(Fit.DF[[i]]) == "device_settings") == TRUE) {
      #Makes a list of data to summarize & confirms the data is all 1 row
      sum_list <- list(Fit.DF[[i]]$file_id, Fit.DF[[i]]$file_creator, Fit.DF[[i]]$device_settings, 
                       Fit.DF[[i]]$zones_target, Fit.DF[[i]]$user_profile, Fit.DF[[i]]$sport)
      sum_rownum2 <- mapply(nrow,sum_list) #Creates integer with number of rows for each data frame in sum_list
      
      checkrowsum <- sum(sum_rownum2) #Sum of the rows, should equal 6
      checkrowprod <- prod(sum_rownum2) #Product of the rows, should equal 1
      
      #If sum and product are good, pass Fit.DF[[i]] to Condense Data, otherwise print error
      ifelse(checkrowsum == 6 & checkrowprod == 1,CondenseDataFromWatch(Fit.DF, i),cat(i, "CondenseDataWatch error"))
      
      #Write sum_data to the Global Environment
      Fit.DF[[i]]$sum_data <- CondenseDataFromWatch(Fit.DF, i)
      
      #Clear out data already summarized in sum_data
      Fit.DF[[i]]$file_id <- NULL #Set it to NULL, which removes it
      Fit.DF[[i]]$file_creator <- NULL #Set it to NULL, which removes it
      Fit.DF[[i]]$device_settings <- NULL #Set it to NULL, which removes it
      Fit.DF[[i]]$zones_target <- NULL #Set it to NULL, which removes it
      Fit.DF[[i]]$user_profile <- NULL #Set it to NULL, which removes it
      Fit.DF[[i]]$sport <- NULL #Set it to NULL, which removes it
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
  
  #If the file was created by connect manually
  if (any(head(Fit.DF[[i]]$device_info$product, 1) == "connect") == TRUE) {
    
    #If summary data doesn't exist, make it
    if (any(names(Fit.DF[[i]]) == "sum_data") == FALSE) {
      
    #missing zones_target, device_settings, user_profile, sport, record, event
    #Makes a list of data to summarize & confirms the data is all 1 row
    sum_list <- list(Fit.DF[[i]]$file_id, Fit.DF[[i]]$file_creator)
    sum_rownum2 <- mapply(nrow,sum_list) #Creates integer with number of rows for each data frame in sum_list
    
    checkrowsum <- sum(sum_rownum2) #Sum of the rows, should equal 2
    checkrowprod <- prod(sum_rownum2) #Product of the rows, should equal 1
    
    #If sum and product are good, pass Fit.DF[[i]] to Condense Data, otherwise print error
    ifelse(checkrowsum == 2 & checkrowprod == 1,CondenseDataFromConnect(Fit.DF, i), 
           cat(i, "CondenseDataConnect didn't work here"))
    
    #Write sum_data to the Global Environment
    Fit.DF[[i]]$sum_data <- CondenseDataFromConnect(Fit.DF, i)
    
    #Fixes mistake in creation time year, though it's still usually off by a day or so
    Fit.DF[[i]]$sum_data$time_created <- Fit.DF[[i]]$activity$timestamp
    
    #Clear out data already summarized in sum_data
    Fit.DF[[i]]$file_id <- NULL #Set it to NULL, which removes it
    Fit.DF[[i]]$file_creator <- NULL #Set it to NULL, which removes it
    }
    
    #Remove dive_settings if it's there
    if (any(names(Fit.DF[[i]]) == "dive_settings") == TRUE) {
      Fit.DF[[i]]$dive_settings <- NULL
    }
    
    #Make filler record df if needed
    if (any(names(Fit.DF[[i]]) == "record") == FALSE) {
    
    Fit.DF[[i]]$record <- head(Fit.DF[[i-1]]$record, 1) #Copy data from the previous 
    Fit.DF[[i]]$activity$timestamp <- Fit.DF[[i]]$record$timestamp #Replace with correct time stamp
    Fit.DF[[i]]$record[,2:ncol(Fit.DF[[i]]$record)] <- 1 #Clear out all other data
    }
    
    if (any(names(Fit.DF[[i]]) == "event") == FALSE) {
    Fit.DF[[i]]$event <- head(Fit.DF[[i-1]]$event, 1) #Copy data from the previous 
    Fit.DF[[i]]$activity$timestamp <- Fit.DF[[i]]$event$timestamp #Replace with correct time stamp
    Fit.DF[[i]]$event[,2:ncol(Fit.DF[[i]]$event)] <- 1 #Clear out all other data
    }
  }
  
  
}



# Screen for Export -------------------------------------------------------


#Clean up unneeded data and values
remove(IndivFitFile, sum_list, sum_rownum2, checkrowprod, checkrowsum)


#EXPORT TO EXCEL FILE FOR IMPORT INTO DATABASE

#Preliminary screen to make sure all of the files have the same names
FitNames <-  as.list(names(Fit.DF[[1]]))
for(i in 1:n) {
  
  if (all(FitNames %in% as.list(names(Fit.DF[[i]]))) == FALSE)  {print(i)}
  
}

# Export to Excel --------------------------------------------------------

ptm <- proc.time() #Start timing

for (i in 501:1350) {
  
  wb <- createWorkbook() #Creates blank workbook
  
  if (any(names(Fit.DF[[i]]) == "sum_data") == TRUE){
    addWorksheet(wb, "SumData") #Need to try this one-off first
    a <- Fit.DF[[i]]$sum_data
    writeData(wb, "SumData", a)
  }
  
  if (any(names(Fit.DF[[i]]) == "event") == TRUE){
    addWorksheet(wb, "Event")
    b <- Fit.DF[[i]]$event
    writeData(wb, "Event", b)
  }
  
  if (any(names(Fit.DF[[i]]) == "device_info") == TRUE){
    addWorksheet(wb, "DevInfo")
    c <- Fit.DF[[i]]$device_info
    writeData(wb, "DevInfo", c)
  }
  
  if (any(names(Fit.DF[[i]]) == "record") == TRUE){
    addWorksheet(wb, "Record")
    d <- Fit.DF[[i]]$record
    writeData(wb, "Record", d)
  }
  
  if (any(names(Fit.DF[[i]]) == "lap") == TRUE){
    addWorksheet(wb, "Lap")
    e <- Fit.DF[[i]]$lap
    writeData(wb, "Lap", e)
  }
  
  if (any(names(Fit.DF[[i]]) == "session") == TRUE){
    f <- Fit.DF[[i]]$session
    addWorksheet(wb, "Session")
    writeData(wb, "Session", f)
  }
  
  if (any(names(Fit.DF[[i]]) == "activity") == TRUE){
    g <- Fit.DF[[i]]$activity
    addWorksheet(wb, "Activity")
    writeData(wb, "Activity", g)
  }
  
  #Names the file as the date created with underscores
  time_char <- as.character(Fit.DF[[i]]$sum_data$time_created)
  time_char <-  gsub("[[:punct:]]", "_", time_char)
  fullname <- paste0(".\\ExportedExcelSheets\\", time_char, ".xlsx")
  
  #Saves the workbook
  saveWorkbook(wb, file = fullname, overwrite = FALSE)
  
  remove(a,b,c,d,e,f,g,time_char)

  ifelse(file.exists(fullname) == TRUE, next, cat(i, " export didn't work "))
  
#Consider rearranging this code to reduce size of Fit.DF as data is copied

}

totaltime <- proc.time() - ptm #Stop timing and assign total time

# Other -------------------------------------------------------------------

partname <- paste0(time_char, ".xlsx")
file.exists(fullname)
#Name the files in Fit.DF by the date created rather by the order they were read in 
#Maybe use setNames or a for loop? Might be easier to create a function for this? 

#Consider for repeated import
#checkduplicates <- duplicated(FilesToRead)
#listduplicates <- as.list(checkduplicates)
#isTRUE(checkduplicates)


#ifelse(length(pluck(Fit.DF, i)$sum_data) > 0, next, cat(i, "something wrong with sum_data"))
#Print error and i value if sum_data hasn't been made
