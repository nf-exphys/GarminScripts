
library(tidyverse)
library(neuralnet)

sum_data <- read_csv("./Data/cleaned_summary_data.csv")
lap_data <- read_csv("./Data/cleaned_lap_data.csv")
record_data <- read_csv("./Data/cleaned_record_data.csv")
  #record data is only 01-29 to 04-02, no idea where all of 2020 went
  #same thing for lap data
hrv_data <- read_csv("./Data/current_AM_HRV.csv")

#quick TL metrics
#ideally recalculate summary data from record or lap data
#this will work for now
sum_data <- sum_data %>%
  mutate(time_7d = roll::roll_mean(TimeMinutes, 7),
         dayb4_time = lag(TimeMinutes, 1),
         dayb4_distance = lag(total_distance),
         dayb4_num_laps = lag(num_laps),
         distance_7d = roll::roll_mean(total_distance, 7))



hrv_data <- hrv_data %>% 
  filter(`HRVtime$datetime` > "2020-07-01") %>% #first consistent data
  rename(datetime = `HRVtime$datetime`)

hrv_data$date <- as.Date(hrv_data$datetime)
hrv_data$time <- format(hrv_data$datetime, "%H:%M:%S")

hrv_data <- hrv_data %>%
  select(-datetime) %>%
  mutate(dayb4_HRV = lag(rMSSD),
         twodayb4_HRV = lag(rMSSD, 2))

#Switch to lap data here if necessary

sum_data <- sum_data %>%
  filter(date > "2020-07-01") %>%
  mutate(EF = (avg_speed / avg_heart_rate) * 100)

all_data <- merge(sum_data, hrv_data, "date", all.y = TRUE)

colnames(all_data)

all_data %>%
  filter(current_fitness > 1.5 & current_fitness < 3) %>% #filtering based on data
  ggplot(data = ., aes(x = date, y = current_fitness)) + geom_point() + 
  geom_smooth()

#select interesting variables for now
some_data <- all_data %>%
  select(avg_heart_rate, avg_speed, max_heart_rate, 
         max_speed, num_laps, total_distance, total_timer_time, 
         current_fitness, rMSSD, day_before_HRV, contains("dayb4"), contains("_7d")) %>%
  drop_na() #drop NA values for now

set.seed(123)
sample_size <- nrow(some_data)*0.8
train_data <- sample(1:nrow(some_data),sample_size)

test_data <- some_data[-train_data,]
train_data <- some_data[train_data,]

nn <- neuralnet::neuralnet(current_fitness ~ ., data = train_data, 
                           startweights = NULL, hidden = c(3))

plot(nn)

temp_test <- test_data %>%
  select(-current_fitness)

nn.results <- compute(nn, temp_test)
results <- data.frame(actual = test_data$current_fitness, prediction = nn.results$net.result)

caret::RMSE(pred = results$prediction, obs = results$actual)

my_lm <- lm(current_fitness ~ ., data = train_data)
lm.results <- predict(my_lm, temp_test)

results$lm.pred <- lm.results

caret::RMSE(pred = results$lm.pred, obs = results$actual)

#Still need to center and scale data
#Also need to extract features from record and lap to improve sum data quality
#Also should see if I can feed record data into a NN to improve prediction


#First shot at record data

colnames(record_data)

#remove ID which is completely empty...
record_data <- janitor::remove_empty(record_data, which = "cols")

some_record <- record_data %>%
  drop_na() %>%
  mutate(EF = heart_rate/speed,
         yr = as.integer(lubridate::year(date)),
         month = lubridate::month(date),
         day = lubridate::day(date),
         hour = lubridate::hour(time),
         min = lubridate::minute(time),
         sec = lubridate::second(time)) %>%
  select(-time, -date)

condense_record <- record_data %>%
  group_split(date) %>%
  map(., .f = zoo::rollapply(some_record, 5, sum, by = 5))

#summarize every 5 sec, takes a bit oof
some_record <- zoo::rollapply(some_record, 5, sum, by = 5)

unique(tibble(as.data.frame(some_record))$month)

some_record <- tibble(some_record)

sample_size <- nrow(some_record)*0.8
train_data <- sample(1:nrow(some_record),sample_size)

dtest_data <- some_record[-train_data,]
train_data <- some_record[train_data,]

nn <- neuralnet::neuralnet(EF ~ ., data = train_data, 
                             startweights = NULL, hidden = c(3),
                           learningrate = 0.000000000000000001)
str(train_data)


#### Try NN with Lap Data #### 

most_data <- merge(lap_data, hrv_data, "date", all.x = TRUE) %>%
  filter(date > "2020-07-10") %>% #rough good date
  select(-total_moving_time) #bad variable, breaks df

colnames(sum_data) <- paste0(colnames(sum_data), "_sum")

sum_data <- sum_data %>%
  filter(date_sum > "2020-07-10")

colnames(sum_data)
colnames(most_data)

#find columns that NN won't like
sum_data %>%
  select(!where(is.numeric))

sum_data <- sum_data %>% select(-ID_sum, -sport_sum, 
                    -start_time_sum, -sub_sport_sum, -time_sum)

#add in summary data
all_data <- merge(most_data, sum_data, by.x = "date", by.y = "date_sum", all.x = TRUE)

#trims down variables a bit
nzv <- caret::nearZeroVar(all_data)
all_data <- all_data[,-nzv] %>%
  drop_na() %>%
  mutate(EF = avg_speed/avg_heart_rate) #get EF from lap data

#double check non-numeric
all_data %>%
  select(!where(is.numeric)) %>%
  colnames()

all_data <- all_data %>% #should probably have some of these but fix later
  select(-date, -lap_trigger, -start_time, -time.x, -time.y)

#should also select some more relevant variables

sample_size <- nrow(all_data)*0.8
train_data <- sample(1:nrow(all_data),sample_size)

test_data <- all_data[-train_data,]
train_data <- all_data[train_data,]

str(all_data)

nn <- neuralnet(EF ~ ., data = all_data, 
                           startweights = NULL, hidden = c(3))
plot(nn)

temp_test <- test_data %>%
  select(-EF)

nn.results <- compute(nn, temp_test)
results <- data.frame(actual = test_data$EF, prediction = nn.results$net.result)

caret::RMSE(pred = results$prediction, obs = results$actual)

my_lm <- lm(EF ~ ., data = train_data)
lm.results <- predict(my_lm, temp_test)

results$lm.pred <- lm.results

caret::RMSE(pred = results$lm.pred, obs = results$actual)

colnames(all_data)

all_data %>%
  mutate(date = as.Date(paste0(yr, "_", month, "_", day), format = ""),
         EF = 100*EF) %>%
  filter(EF > 0) %>%
  #filter(EF < 5) %>%
  ggplot(data = ., aes(x = date, y = EF)) + 
    geom_point(stat = 'identity') + 
  scale_x_date(date_labels = "%Y %m %d")
