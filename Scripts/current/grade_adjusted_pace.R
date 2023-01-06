library(tidyverse); library(slider)

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
  mutate(elev_grade = slide_dbl(elev_grade, .f = median, before = 5, after = 5, step = 1),   #smooth grade using the median
    elev_grade = slide_mean(elev_grade, before = 1, after = 1, step = 1), #smooth grade using mean
    elev_grade = slide_mean(elev_grade, before = 3, after = 0, step = 1),  #smooths grade again
    elev_grade = slide_mean(elev_grade, before = 5, after = 5, step = 1)) %>%  #smooths grade again x2
  ggplot(data = .) +
    geom_line(aes(x = distance, y = smooth_elev - smooth_elev[1]), color = "green") +
    geom_line(aes(x = distance, y = elev_grade), color = "blue")

#Create a function to get grade
calc_grade <- function(.data){
  df <- .data %>%
    mutate(rise = (smooth_elev - lag(smooth_elev,3)), #needed to find grade
           run = (distance - lag(distance,3))) %>% #needed to find grade
    mutate(elev_grade = round((rise / run) * 100, 3)) %>% #calculates grade
    mutate(elev_grade = slide_dbl(elev_grade, .f = median, before = 5, after = 5, step = 1),   #smooth grade using the median
           elev_grade = slide_mean(elev_grade, before = 1, after = 1, step = 1), #smooth grade using mean
           elev_grade = slide_mean(elev_grade, before = 3, after = 0, step = 1),  #smooths grade again
           elev_grade = slide_mean(elev_grade, before = 5, after = 5, step = 1))
  
  return(df)
}

#test to make sure calc_grade works
calc_grade(smoothed_data[[120]]) %>% 
  ggplot(data = .) +
  geom_line(aes(x = distance, y = smooth_elev - smooth_elev[1]), color = "green") +
  geom_line(aes(x = distance, y = elev_grade), color = "blue")

#Now get grade information for everything
all_data <- lapply(smoothed_data, calc_grade)

#Make a function to find stop events?

#Combine list of dataframes into one large dataframe
all_data <- bind_rows(all_data)

#Once the errors from bridges or rivers are corrected, go back and re-smooth
#For now, just use mean +/- 3 SD of speed, grade, and HR

#Create function to find mean and 95% CI
var_confint <- function(.data, .var){
  m <- mean(.data[[.var]], na.rm = TRUE)
  s <- sd(.data[[.var]], na.rm = TRUE)
  
  res <- c((m - 3*s), m, (m + 3*s))
  return(res)
}

filtered_data <- all_data %>% 
  filter(time_elapsed > 180) %>%  #ignore first 3 minutes of data
  mutate(elev_grade = ifelse(is.infinite(elev_grade), NA, elev_grade)) %>%  #fix infinite values in grade
  mutate(speed = speed * (3600/1609)) #get speed in mph

#Visualize data
ggplot(data = filtered_data) + geom_density(aes(x = heart_rate))
ggplot(data = filtered_data) + geom_density(aes(x = speed))
ggplot(data = filtered_data) + geom_density(aes(x = elev_grade))
ggplot(data = filtered_data) + geom_density(aes(x = cadence))

#Get 95% CIs
hr_bounds <- var_confint(filtered_data, "heart_rate")
speed_bounds <- var_confint(filtered_data, "speed")
grade_bounds <- var_confint(filtered_data, "elev_grade")
cadence_bounds <- var_confint(filtered_data, "cadence")

#Filter data within 95% CI
filtered_data <- filtered_data %>% 
  filter(heart_rate > hr_bounds[1] & heart_rate < hr_bounds[3]) %>% 
  filter(speed > speed_bounds[1] & speed < speed_bounds[3]) %>% 
  filter(elev_grade > grade_bounds[1] & elev_grade < grade_bounds[3]) %>% 
  filter(cadence > cadence_bounds[1] & cadence < cadence_bounds[3])

#Consider adding lagged speed and grade for 10 and 30 seconds back
#Also find way to flag rows after I stopped running

easy_data <- filtered_data %>% 
  filter(speed > 5 & speed < 8.25) %>% #easy running speeds
  filter(heart_rate > 120) %>%
  mutate(elev_grade = round(elev_grade, 1)) %>% #round grade to the nearest tenth
  filter(elev_grade > -5 & elev_grade < 5)
  
#Plot for easy running
easy_data %>% 
  # filter(heart_rate < 155 & heart_rate > 140) %>% #easy running heart rates
  # filter(elev_grade > -2 & elev_grade < 10) %>% 
  group_by(speed, elev_grade) %>% 
  summarise(across(.fns = mean)) %>% 
  ggplot(data = ., aes(x = elev_grade, y = heart_rate)) + 
  geom_point() + 
  geom_smooth()

my_aov <- aov(heart_rate ~ elev_grade * speed * cadence * time_elapsed * distance, data = easy_data)
summary(my_aov)

set.seed(123)

training_data <- easy_data %>% 
  select(heart_rate, elev_grade, speed, cadence, time_elapsed, distance, timestamp, key)

test_nnet <- nnet::nnet(heart_rate ~ elev_grade + speed + cadence + time_elapsed + distance, data = training_data, size = 3)

rand_rows <- sample(nrow(training_data), size = 10)

sample_data <- easy_data[rand_rows,]

predict(test_nnet, sample_data)

#crashes R, probably due to too much data in training_data
test_rf <- caret::train(heart_rate ~ ., data = training_data, method = "rf")

predict(test_rf, sample_data) 

#Attempt to look at faster running
filtered_data %>% 
  filter(speed > 9 & speed < 14) %>% #workout speeds but not a sprint
  filter(heart_rate > 140) %>% #ignore heart rates that are unreasonably low
  mutate(elev_grade = round(elev_grade, 1)) %>% #round grade to the nearest tenth
  filter(elev_grade > -3 & elev_grade < 3) %>%
  group_by(speed, elev_grade) %>% 
  summarise(across(.fns = median)) %>% 
  ggplot(data = ., aes(x = elev_grade, y = heart_rate)) + 
  geom_point() + 
  geom_smooth()








