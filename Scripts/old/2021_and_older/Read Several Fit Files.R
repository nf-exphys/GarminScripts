#Reading Multiple Fit Files
library(fitFileR); library(tidyverse)

FolderWithFitFiles <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles"
FilesToRead <- list.files(path = FolderWithFitFiles) 
#^Creates a list with the fit files in the RandomFitFiles folder
#Note: Need to add double backslashes to make file path work, just copying the file path from Windows Explorer won't work

#Still need a way to compare against files that have already been read
#Look at this: https://stackoverflow.com/questions/17598134/compare-two-character-vectors-in-r

FilePath <- paste(FolderWithFitFiles, FilesToRead, sep = "//") #Adds the file name to each file in the list of Fit files

ReadData <- lapply(FilePath, readFitFile) 
ReadDataDF <- as.data.frame(do.call(rbind, ReadData))
rm(ReadData)

warnings()
rm() #Add variable name in parentheses to remove it from the environment

getwd()