#Load one file

library(fitFileR); library(data.table); library(tidyverse)
loadfile <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\Personal Running\\enable_hrv_settings_file.fit"
GarminFile <- readFitFile(loadfile)

loadfile2 <- "HRVtest.FIT"
GarminFile2 <- readFitFile(loadfile2)

GarminFile.lapp <- lapply(GarminFile2, data.frame, stringsAsFactors = FALSE)
GarminFile <- lapply(GarminFile, data.frame, stringsAsFactors = FALSE)

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