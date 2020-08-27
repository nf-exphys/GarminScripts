#Calculating Summary Data from Line-by-Line data

#Identifying date and time
FileDate <- as.character(GarminFile$record$timestamp[1]) #Makes a character with the file date
filename <- str_replace(loadfile, "\\\\ES19/user-documents/Nick.Foreman/Documents/R/win-library/4.0/fitFileR/extdata/FitFiles//","") 
str_remove()
#^makes a character of the file name, still need to remove the slashes and ".fit" part
TimeMinutes <- GarminFile$activity$total_timer_time #Makes character with total activity time

SumData <- data.frame(FileDate, filename, TotalTRIMP, TimeMinutes) #Just need to do this once

SumDataNew <- data.frame(FileDate, filename, TotalTRIMP, TimeMinutes) #Data frame with new data

SumData <- rbind(SumData, SumDataNew) #Add new data to SumData dataframe

write_csv(SumData, "GarminData\\GarminData.csv") #Writes csv file with new and old data