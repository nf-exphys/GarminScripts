library(tidyverse)

#Read in smoothed elevation data

smoothed_records <- list.files(path = "./Data/processed_fit/record/", pattern = "smooth", full.names = TRUE)

smoothed_data <- lapply(smoothed_records, read_csv, show_col_types = FALSE)

# #375 is Mississippi river data
# #406 is Missouri run with big drop/rise
# #408 is hill repeats
# #250 is a bike home from the lab with some elevation errors
# #356 is flapjack friday


#Now calculate grade

test <- smoothed_data[[408]] 
 
test %>%
  mutate(rise = (smooth_elev - lag(smooth_elev,3)), #needed to find grade
         run = (distance - lag(distance,3))) %>% #needed to find grade
  mutate(elev_grade = round((rise / run) * 100, 3)) %>% #calculates grade
  mutate(elev_grade = slide_mean(elev_grade, before = 1, after = 1, step = 1)) %>%  #smooths grade
  mutate(elev_grade = slide_mean(elev_grade, before = 3, after = 0, step = 1)) %>%  #smooths grade again
  mutate(elev_grade = slide_mean(elev_grade, before = 5, after = 5, step = 1)) %>%  #smooths grade again x2
  ggplot(data = .) +
    geom_line(aes(x = distance, y = smooth_elev - smooth_elev[1]), color = "green") +
    geom_line(aes(x = distance, y = elev_grade), color = "blue")

# #Once the errors from bridges or rivers are corrected, go back and re-smooth
# #Can just use smooth_elevation with plot and impute set to false
