#Import all old Fit Files and Analyze

library(fitFileR); library(tidyverse); library(rlist)

FolderWithFitFiles <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles"

FilesToRead <- list.files(path = FolderWithFitFiles, full.names = TRUE)
#^Creates a list with the fit files in the RandomFitFiles folder and their file path (better than pasting in the file path)

#Sets n as the number of files to be read
n <- length(FilesToRead)
#Creates an empty list to be filled with data frames
Fit.DF <- vector('list', n)

#Read in files from FilesToRead; convert read-in file to a data frame and store it in a list; remove specified developer fields
for (i in 1:n) {
  IndivFitFile = readFitFile(FilesToRead[i]) #Reads in each fit file and gives it the name IndivFitFile
  Fit.DF[[i]] <- lapply(IndivFitFile, data.frame, stringsAsFactors = FALSE) #Puts fit files into a list
  if (length(pluck(Fit.DF, i)$developer_data_id) > 0){ #If developer_data_id exists...
    Fit.DF[[i]]$developer_data_id <- NULL #Set it to NULL, which removes it
  }
  if (length(pluck(Fit.DF, i)$field_description) > 0){ #If field_description exists...
    Fit.DF[[i]]$field_description <- NULL #Set it to NULL, which removes it
  }
  if (length(pluck(Fit.DF, i)$training_file) > 0){ #If training_file exists...
    Fit.DF[[i]]$training_file <- NULL #Set it to NULL, which removes it
  }
}

#Calculate TRIMP off of record data and add to activity data

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

#Adds TRIMP calculations to the activity data frame
for (i in 1:n) {Fit.DF[[i]]$activity <- TRIMP.func(Fit.DF, i)}


#Creates a list with all of the record data as a data frame
FitRecord <- vector('list', n)
ActivityType <- c('Run', 'Run Indoor')

#Copy line-by-line record data and place in new list of data frames
for (i in 1:n) {
  
  if (Fit.DF[[i]]$sport$name == "Run") { #If the activity type is run, pull out the record and put it in FitRecord
    FitRecord[[i]] <- select(pluck(Fit.DF, i)$record, 1:7)
    }
  
  if (Fit.DF[[i]]$sport$name == "Run Indoor") { #If the activity type is run indoor...
    j <- ncol(Fit.DF[[i]]$record) #Create a value that corresponds to the number of columns, sometimes there are 6 columns and not 7
    FitRecord[[i]] <- select(pluck(Fit.DF, i)$record, all_of(1:j)) #Pulls out the record and puts it in FitRecord
    }
  
}

#Name the files in Fit.DF by the date created rather by the order they were read in 
#Maybe use setNames or a for loop? Might be easier to create a function for this? 
