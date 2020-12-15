library(reticulate); library(tidyverse)

#Sys.which("python")
#use_python("\\Desktop\\python-3.9.1-amd64.exe")

#reticulate::source_python("C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\Fall 2020 classes\\R Files\\GarminData\\HRV Thresholds\\hrv_thresholds.py")


library(fitFileR)
hrvfit <- fitFileR::readFitFile("enable_hrv_settings_file.fit")

files <- list.files(path = "./HRV Thresholds/", pattern = "*.FIT", full.names = T)
garminfile <- fitFileR::readFitFile(files[20])

head(garminfile$hrv)