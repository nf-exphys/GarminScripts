
source("./Scripts/2022/master_fit_func.R")

download_location <- "./Data/zip_fit_files/" #location for zip download
storage_location <- "./Data/raw_fit_files" #location for storage of fit files

#Give warning if zip files haven't been extracted/copied
if (length(list.files(download_location) != 0)){
  
  warning("Not all zip files have been extracted")

}

#### Find New Fit Files ####

#list all files ready to be processed
fit_files <- list.files(path = storage_location, pattern = ".fit", full.names = TRUE)

#gets just the file # from the file name of the fit files
fit_to_read <- as_tibble(fit_files) %>%
  rename(fit_name = value) %>%
  mutate(fit_name = str_remove(fit_name, storage_location),
         fit_name = str_remove(fit_name, "/"),
         fit_name = str_remove(fit_name, "_ACTIVITY.fit"),
         fit_name = as.numeric(fit_name))

fit_already_done <- read_csv("./Data/fit_files_already_extracted.csv") 

#list of file numbers that could be read in but haven't been yet
fit_to_read <- setdiff(fit_to_read$fit_name, fit_already_done$fit_name)

#Add file path for matching to fit_files
fit_to_read <- paste0(storage_location, "/", fit_to_read, "_ACTIVITY.fit")

#Find the location in the list of fit files that haven't been read
idx_to_read <- which(fit_files %in% fit_to_read == TRUE)

#subset to just those that haven't been read
fit_files <- fit_files[idx_to_read]

#stop if there isn't anything new to read in
if(length(fit_files) < 1){
  stop("There isn't any data to read in")
}

#### Process Data ####

all_data <- lapply(fit_files, readFitFile)

data_to_write <- list("file_info", "record", "lap", "event", "hrv", "device_info")

#this seems to work, not sure why I wasn't using it before?
lapply(fit_files, process_fit_data)

#now check the fit files in file_info folder 
fit_files <- list.files("./Data/processed_fit/file_info", full.names = TRUE)
for (i in 1:length(fit_files)){
  
  csv <- read_csv(fit_files[i], col_select = "key", show_col_types = FALSE)
  if (unique(csv$key) %in% fit_already_done$fit_name){
    next
  } else{
    key = as_tibble(unique(csv$key))
    names(key)[1] = "fit_name"
    fit_already_done <- bind_rows(fit_already_done, key)
  }
  
  if (i == length(fit_files)){
    write_csv(fit_already_done, "./Data/fit_files_already_extracted.csv")    
  }
  
}

#Old for loop to process data

# for (i in 1:length(all_data)){

# fit_id <- str_match_all(string = fit_files[[i]], 
#                         pattern = "[0-9]+") %>% 
#   unlist() %>% 
#   as.numeric()
# 
# #get data
# file_info <- get_data("file_id")
# record <- get_data("record")
# lap <- get_data("lap")
# event <- get_data("event")
# hrv <- get_data("hrv")
# device_info <- get_data("device_info")
# 
# #get the year that the file was made
# year <- str_extract(string = as.character(file_info$time_created), pattern = "[:digit:]{4}")
# year <- as.numeric(year)
# 
# #don't read in data older than 2020
# if(year < 2020){
#   print(paste0("file #", i, " was made in ", year, "and isn't newer than 2020. Skipping."))
#   next
# }
# 
# #merge sport and file_info because they're both always one row
# file_info <- bind_cols(file_info, sport, .name_repair = "unique")
# 
# #determine whether HRM was connected
# file_info$hrm_connect <- hrm_connected(device_info)
# 
# time_created <- as.character(file_info$time_created)
# time_created <- str_replace_all(string = time_created, pattern = "[\\s|:|-]", replacement = "_")
# #convert POSIXct to character and replace special characters
# 
# sink(nullfile())
# lapply(data_to_write, write_csv_path)
# sink()
# 
# }
