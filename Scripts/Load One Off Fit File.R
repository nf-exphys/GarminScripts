#Load one file

library(fitFileR); library(data.table); library(tidyverse)
loadfile <- "E:\\GARMIN\\ACTIVITY\\AC8F0553.FIT" #load file straight from the watch
GarminFile <- readFitFile(loadfile)

loadfile3 <- "./Data/ExportedRunData/Old_Fit_Files/B2FJ1553.FIT"
GarminFile3 <- readFitFile(loadfile3)
FITfileR::file_id(GarminFile3)
FITfileR::laps(GarminFile3)
#GarminFile.lapp <- lapply(GarminFile2, data.frame, stringsAsFactors = FALSE)
#GarminFile <- lapply(GarminFile, data.frame, stringsAsFactors = FALSE)

library(purrr)
as.numeric(map(GarminFile2$hrv$time[1],1)) #pulls out RR interval from hrv
length(GarminFile2$record$heart_rate) == length(GarminFile2$hrv$time) #length of the files is the same!
rr <- matrix(data = NA, nrow = 13, ncol = 1) #creates matrix to store data
for (i in 1:13){
  rr[i,] <- as.numeric(map(GarminFile2$hrv$time[i],1))
  
} #pulls RR data out and adds to matrix
colnames(rr)[1] <- "RR" #sets column name to RR
write.csv(rr, file = "QuickHRVTest.csv", row.names = F) #writes to csv, doesn't seem to help with reading in though
write.table(rr, file = "QuickHRVTest.txt", row.names = F, col.names = F) #writes to txt
library(RHRV) #load HRV library 
hrv.data <- CreateHRVData() #create structure to store HRV data
hrv.data <- SetVerbose(hrv.data, T)
#hrv.data <- LoadBeatRR(hrv.data, RecordName = "QuickHRVTest.csv", RecordPath = ".", scale = 0.001)
  #Doesn't work, not sure why
hrv.data <- LoadBeat(fileType = "RR", HRVData = hrv.data, Recordname = "QuickHRVTest.txt")
nihr <- BuildNIHR(hrv.data)
nihr$Beat$RR; rr #comparing RR intervals
nihr$Beat$niHR; GarminFile2$record$heart_rate
PlotNIHR(nihr)

##### Looking at HRV in workout #####
library(purrr)
as.numeric(map(GarminFile$hrv$time[1],1)) #pulls out RR interval from hrv
length(GarminFile$record$heart_rate) == length(GarminFile$hrv$time) #length of the files is not the same, sadly
record <- GarminFile$record
plot(record$timestamp, record$heart_rate) #shows HR vs. time
#record$timestamp[which(record$heart_rate < 90)] #gives timestamps when HRM disconnected
  #Looks like right around 15:25 and 15:35
#head(record$timestamp)
rr <- matrix(data = NA, nrow = length(GarminFile$hrv$time), ncol = 1) #creates matrix to store data
for (i in 1:length(GarminFile$hrv$time)){
  rr[i,] <- as.numeric(map(GarminFile$hrv$time[i],1))
  
} #pulls RR data out and adds to matrix
write.table(rr, file = "LongRun.HRVTest.txt", row.names = F, col.names = F) #writes to txt

library(RHRV) #load HRV library 
hrv.data <- CreateHRVData() #create structure to store HRV data
hrv.data <- SetVerbose(hrv.data, T)
hrv.data <- LoadBeat(fileType = "RR", HRVData = hrv.data, Recordname = "Workout4x7T.HRVTest.txt")
nihr <- BuildNIHR(hrv.data)
class(rr)
rrcomp <- data.frame(nihr$Beat$RR[2:length(nihr$Beat$RR)], rr) #comparing RR intervals
colnames(rrcomp)[1] <- "nihr.RR"
rrcomp$nihr.RR.scaled <- rrcomp$nihr.RR/1000
rrcomp$difference <- rrcomp$nihr.RR.scaled - rrcomp$rr
plot(record$heart_rate)
length(nihr$Beat$niHR)
plot(record$heart_rate[1:3087], nihr$Beat$niHR[1:3087])

#no difference between nihr RR and measured RR even without filtering
rrcomp %>% arrange(desc(abs(difference)))  %>% head(n=5) 

nihr.filt <- RHRV::FilterNIHR(nihr, minbpm = 120, maxbpm = 180)
plot(record$heart_rate[1:2204], nihr.filt$Beat$niHR)
plot(nihr.filt$Beat$Time, nihr.filt$Beat$niHR) #plots filtered HR vs time, appears similar to Garmin HR graph

