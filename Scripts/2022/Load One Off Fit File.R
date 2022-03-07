#Load one file

library(FITfileR); 
# load_file <- list.files("./Data/raw_fit_files/6038649122_ACTIVITY.FIT", full.names = TRUE)
load_file <- "./Data/processed_fit_files/6323276835_ACTIVITY.fit" #load file straight from the watch
GarminFile <- readFitFile(load_file)
getMessagesByType(GarminFile, "file_id")