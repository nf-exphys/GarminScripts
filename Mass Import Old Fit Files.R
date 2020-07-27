#Import all old Fit Files and Analyze

library(fitFileR); library(tidyverse)

FolderWithFitFiles <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles"
FilesToRead <- list.files(path = FolderWithFitFiles) 
#^Creates a list with the fit files in the RandomFitFiles folder
  #Note: Need to add double backslashes to make file path work, just copying the file path from Windows Explorer won't work

FilePath <- paste(FolderWithFitFiles, FilesToRead, sep = "//") #Adds the file name to each file in the list of Fit files

ReadData <- lapply(FilePath, readFitFile) #Extracts data from fit files using readFitFile from the list FilePath

#Removing developer fields from the files
exists("ReadData") #Shows that ReadData exists
DevFieldsLocation <- grep("developer_data_id|field_description", ReadData) #Creates integer with number of files in ReadData list that have developer fields
DevFieldsChar <- as.character(DevFieldsLocation)
ReadDataNoDev <- lapply(DevFieldsChar, remove(ReadData)) #The remove(ReadData) part of this doesn't work, maybe piping is the answer?
#HERE'S WHERE TO PICK UP WHEN YOU GET THE CHANCE

#idea: RemoveDevFields <- function()

#idea: ReadDataNoDev <- lapply()

ReadDataDF <- as.data.frame(do.call(rbind, ReadData)) #Converts data in ReadData (a list) to a large data frame
rm(ReadData) #Removes list of files

#Didn't work perfectly, something wrong with the number of columns when converted to DF. Seems like it's the developer fields in some but not all files

