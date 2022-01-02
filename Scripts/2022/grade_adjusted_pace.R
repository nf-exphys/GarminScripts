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
  group_by(key) %>% 
  group_split(.keep = TRUE)

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
test <- record_list[[375]]
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




#Now calculate


# #Developing smoothed data through trial and error

#375 is Mississippi river data
#406 is Missouri run with big drop/rise
#408 is hill repeats
#250 is a bike home from the lab with some elevation errors


# 
# test <- record_list[[375]]
# 
# ggplot(data = test, aes(x = distance, y = elevation)) + 
#   geom_line() + 
#   ggtitle("Original Elevation Data")
# 
# init_elev <- test$elevation[1]
# 
# #Set up smoothed elevation with overlay of elevation difference
# test %>% 
#   mutate(smooth_elev = slide_dbl(elevation, .f = median, before = 5, after = 5, step = 1)) %>% #smooths out using the median
#   mutate(smooth_elev = slide_mean(smooth_elev, before = 3, after = 3, step = 1)) %>%  #smooth data a bit more with rolling mean
#   mutate(smooth_elev = slide_mean(smooth_elev, before = 15, after = 15, step = 1)) %>% #now take a wider pass
#   mutate(diff_smooth = smooth_elev - elevation) %>% 
#   mutate(diff_smooth = case_when(diff_smooth > 1.8 ~ diff_smooth, diff_smooth < -1.8 ~ diff_smooth, TRUE ~ 0)) %>% 
#   pivot_longer(cols = contains("elev"), names_to = "elev_type", values_to = "elev") %>% 
#   mutate(elev_type = as.factor(elev_type)) %>% 
#   ggplot(data = ., aes(x = distance, y = elev - init_elev, group = elev_type, colour = elev_type)) +
#   geom_bar(aes(x = distance, y = diff_smooth), alpha = 0.1, stat = 'identity', color = "grey") + 
#   geom_line() +
#   # ylim(265, 285) + 
#   ggtitle("All Elevation Data")
# 
# 
# #375 is Mississippi river data
# #406 is Missouri run with big drop/rise
# #408 is hill repeats
# #250 is a bike home from the lab with some elevation errors
# #356 is flapjack friday
# 
# smooth_record[[356]]
# 
# 
# test %>% 
#   mutate(rise = (smooth_elev - lag(smooth_elev,3)), #needed to find grade
#          run = (distance - lag(distance,3))) %>% #needed to find grade
#   mutate(elev_grade = round((rise / run) * 100, 3)) %>% #calculates grade
#   mutate(elev_grade = slide_mean(elev_grade, before = 5, after = 0, step = 1)) %>%  #smooths grade
#   ggplot(data = ., aes(x = distance, y = smooth_elev), color = "green") + 
#     geom_line() + 
#     geom_line(aes(x = distance, y = elev_grade), color = "blue")
# 
# 
# #Once the errors from bridges or rivers are corrected, go back and re-smooth
# #Can just use smooth_elevation with plot and impute set to false
