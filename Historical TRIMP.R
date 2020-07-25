#Historical TRIMP analysis for Volume v. Intensity

#First, make sure I can load just one file
library(fitFileR); library(tidyverse);
loadfile <- system.file("extdata/FitFiles/", "3220831446.fit", package = "fitFileR")
GarminFile <- readFitFile(loadfile)

#Calculate TRIMP
HRfreq <- GarminFile$record %>%
  count(heart_rate) #makes data frame of frequencies of each HR value
names(HRfreq)[1] <- "HR" #Set name of first column to HR
names(HRfreq)[2] <- "number" #Set name of second column to number

HRfreqHRR <- HRfreq %>%
  mutate(HRR = ((HRfreq$HR-48)/(192-48))) #Create HRR column

HRfreqTRIMP <- HRfreqHRR %>%
  mutate(TRIMP = (HRfreqHRR$number / 60) * HRfreqHRR$HRR * (0.64 * exp(1.92*HRfreqHRR$HRR))) #Now apply TRIMP formula
TotalTRIMP <- sum(HRfreqTRIMP$TRIMP, na.rm = TRUE) #TRIMP value for the activity

#Identifying date and time
FileDate <- as.character(GarminFile$record$timestamp[1]) #Makes a character with the file date
filename <- str_replace(loadfile, "\\\\ES19/user-documents/Nick.Foreman/Documents/R/win-library/4.0/fitFileR/extdata/FitFiles//","") 
#^makes a character of the file name, still need to remove the slashes and ".fit" part
TimeMinutes <- GarminFile$activity$total_timer_time #Makes character with total activity time

SumData <- data.frame(FileDate, filename, TotalTRIMP, TimeMinutes) #Just need to do this once

SumDataNew <- data.frame(FileDate, filename, TotalTRIMP, TimeMinutes) #Data frame with new data

SumData <- rbind(SumData, SumDataNew) #Add new data to SumData dataframe

write_csv(SumData, "GarminData\\GarminData.csv") #Writes csv file with new and old data

#Attempt at looping

FolderWithFitFiles <- system.file("extdata/FitFiles/", package = "fitFileR"); FilesToRead <- list.files(path = FolderWithFitFiles) 
#^Creates a character with the fit files in the extdata/FitFiles folder

#Still need a way to compare against files that have already been read
#Look at this: https://stackoverflow.com/questions/17598134/compare-two-character-vectors-in-r

FilePath <- paste(FolderWithFitFiles, FilesToRead, sep = "//") #character list (?) with file path for each fit file in the folder

ReadData <- lapply(FilePath, readFitFile) 
ReadDataDF <- as.data.frame(do.call(rbind, ReadData))


warnings()

getwd()