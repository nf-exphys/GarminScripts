library(tidyverse); library(missForest) 
library(slider) #note: this masks filter from dplyr

records <- list.files(path = "./Data/processed_fit/record/", pattern = "*.csv", full.names = TRUE)

#keep only 2021 or newer

records_idx <- which(str_detect(records, "2021_") == TRUE)

records <- records[records_idx]

record_data <- lapply(FUN = function(x){read_csv(x, show_col_types = FALSE) %>% mutate(key2 = key)}, 
                      X = records)

record_df <- bind_rows(record_data, .id = "key2") %>% 
  select(-fractional_cadence) %>% 
  mutate(timestamp = lubridate::with_tz(timestamp, tzone = "America/Chicago")) %>% #make sure time zone is always CST
  rename(id = key2) %>% 
  drop_na(latitude, longitude)

record_list <- record_df %>% 
#function to extract the key variable from a dataframe or list of data frames
get_key <- function(x){return(unique(x$key))}

#Now, smooth all data and set elevation to NA where diff_smooth > 1.8
#Then, impute using MissForrest and re-smooth the data
#Then, find errors in elevation data due to big drops

#Smooths elevation and calculates difference between smoothed and original values
smooth_elevation <- function(df, plot = FALSE, impute_diffs = TRUE){
  
  #baseline smoothing
  df <- df %>%  
    mutate(
      smooth_elev = slide_dbl(elevation, .f = median, before = 5, after = 5, step = 1),   #smooths out using the median
      smooth_elev = slide_mean(smooth_elev, before = 3, after = 3, step = 1),  #smooth data a bit more with rolling mean
      smooth_elev = slide_mean(smooth_elev, before = 15, after = 15, step = 1)) %>%  #now take a wider pass
    mutate(diff_smooth = smooth_elev - elevation) #find difference between smoothed and original elevation
  
  #Set original elevation data aside
  og_elev <- df$elevation
  
  if(impute_diffs == TRUE){
    
    df <- df %>% 
      mutate(elevation = 
               case_when(diff_smooth > 1.8 ~ NA_real_, 
                         diff_smooth < -1.8 ~ NA_real_, 
                         TRUE ~ elevation))
    #remove columns that cause errors with RF and convert to data frame as required by missForest
    df_imp <- df %>% 
      select(-timestamp, -id) %>% 
      as.data.frame()
    
    #Impute data using default parameters 
    #but without printing information about each iteration
    invisible(capture.output(df_imp <- missForest::missForest(df_imp, verbose = FALSE)))
    
    #Convert back to tibble, make sure data is in the correct order, add back columns, and re-smooth data
    df <- df_imp$ximp %>% 
      as_tibble() %>% 
      arrange(time_elapsed) %>% 
      mutate(timestamp = df$timestamp,
             id = df$id) %>% 
      mutate( #smooth again after imputing elevation
        smooth_elev = slide_dbl(elevation, .f = median, before = 5, after = 5, step = 1),   
        smooth_elev = slide_mean(smooth_elev, before = 3, after = 3, step = 1),  
        smooth_elev = slide_mean(smooth_elev, before = 15, after = 15, step = 1))
    
  }
  
  if(plot == TRUE){
    
    df %>% 
      mutate(
        og_elevation= og_elev,
        diff_smooth = case_when(diff_smooth > 1.8 ~ diff_smooth, diff_smooth < -1.8 ~ diff_smooth, TRUE ~ 0)
      ) %>% 
      pivot_longer(cols = contains("elev"), names_to = "elev_type", values_to = "elev") %>% 
      mutate(elev_type = as.factor(elev_type)) %>% 
      ggplot(data = ., aes(x = distance, y = elev - elev[1], group = elev_type, colour = elev_type)) +
      geom_bar(aes(x = distance, y = diff_smooth), alpha = 0.1, stat = 'identity', color = "pink") + 
      geom_line() +
      labs(title = "Smoothed vs. Original vs. Corrected Elevation Data",
           subtitle = "Pink bars highlight notable differences between original and smoothed data") + 
      ylab("Change in Elevation")
  } else{
    return(df)
  }
  
}

#Example use
test <- record_list[[1]]
smooth_elevation(test, plot = TRUE, impute_diffs = TRUE)

library(parallel)
numCores <- detectCores()
cores_to_use <- numCores-1
cl <- makeCluster(cores_to_use)

#Set up workers with necessary libraries
clusterEvalQ(cl, {
  library(dplyr);
  library(tidyr);
  library(readr);
  library(stringr);
  library(utils);
  library(slider);
  library(ggplot2);
  library(missForest)
})

smooth_record <- parallel::parLapply(cl, 
                                     record_list, 
                                     #fun = function(x) tryCatch(process_fit_data(x), error = function(e) e)
                                     fun = smooth_elevation #use defaults of impute = TRUE and plot = FALSE
)

stopCluster(cl)
closeAllConnections()
remove(cl)

#Now write each element in smooth_record to a CSV
keys_in_record_data <- lapply(record_data, FUN = get_key) #gets the key variable from the initial data that was read in
keys_in_smooth_data <- lapply(smooth_record, FUN = get_key) #gets the key variable from the smoothed dat that needs to be written

idx_to_write <- which(keys_in_record_data %in% keys_in_smooth_data) #idx of record data that matches smooth data

records_to_write <- records[idx_to_write]

records_to_write <- str_replace(string = records_to_write, pattern = "_record", replacement = "_smooth_elev_record")

for(i in 1:length(smooth_record)){
  
  smooth_record[[i]] %>% 
    select(-id) %>% 
    write_csv(x = ., file = records_to_write[[i]])
  
}