#Load one file

library(fitFileR); library(data.table); library(tidyverse)
loadfile <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles\\A6NI5029.fit"
GarminFile <- readFitFile(loadfile)

loadfile2 <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles\\A6JB2928.fit"
GarminFile2 <- readFitFile(loadfile2)

GarminFile.asDF <- as.data.frame(GarminFile2) #DOESN'T WORK

GarminFile.DF <- data.frame(GarminFile2) #DOESN'T WORK

GarminFile.rbind <- rbindlist(GarminFile2, fill = TRUE) #DOESN'T WORK

GarminFile.lapp <- lapply(GarminFile2, data.frame, stringsAsFactors = FALSE) #WORKS

class(GarminFile.asDF)
class(GarminFile.DF)
class(GarminFile.rbind)
class(GarminFile.lapp)
