#Correct HR data
library(tidyverse, warn.conflicts = FALSE)
library(chron)

#read in data
source("./Scripts/DataCleaning.R")
hr_acc_sum_data <- read_csv("./Data/hr_acc_sum_data.csv")

#combine the two
recent_sum <- merge(x=recent_sum, y=hr_acc_sum_data, by = "ID")
remove(hr_acc_sum_data)

#Group by ID and create change in altitude in feet by 10-60 seconds
recent_records <- recent_records %>%
  group_by(ID) %>%
  mutate(
    alt_lag10 = lag(altitude,10) - altitude,
    alt_lag20 = lag(altitude,20) - altitude,
    alt_lag30 = lag(altitude,30) - altitude,
    alt_lag40 = lag(altitude,40) - altitude,
    alt_lag50 = lag(altitude,50) - altitude,
    alt_lag60 = lag(altitude,60) - altitude
    ) 

condense_records <- recent_records %>%
  group_by(ID) %>%
  add_count() %>% #adds col with n of each group
  mutate(threes = (1:unique(n) %/% 3)+1) %>% #adding 1 to use threes in place of time
  group_by(ID, threes) %>% #group data into sets of 3 seconds
  summarise(time = max(time), across(-time,mean))
    #max of time, mean of everything else

#to preserve original scale
condense_records <- condense_records %>% mutate(threes = threes *3) 

#round all numeric to 2 decimals except position
condense_records <- condense_records %>%
  mutate(position_lat = as.character(position_lat),
         position_long = as.character(position_long),
  across(where(~ is_numeric(.x)), round, 2), #throws warnings, still works
  position_lat = as.numeric(position_lat),
  position_long = as.numeric(position_long))

acc_HR_IDs <- recent_sum %>%
  filter(hr_all_acc == "yes") %>%
  pull(ID)

sample_ID <- sample(acc_HR_IDs, 1)

condense_records %>%
  filter(ID == acc_HR_IDs[93]) %>%
  ggplot(.) +
  geom_line(aes(x=threes, y=altitude_lag30)) + #black
  #geom_line(aes(x=threes, y=heart_rate/5), colour = "#FF0000") + #red
  #geom_line(aes(x=threes, y=speed*20), colour = "#0000ff") +  #blue
  geom_line(aes(x=threes, y=altitude-700), colour = "green") #green


#We'll go with lag20 for now, 
#b/c that's about how long it would take HR to adjust
#won't be perfect but we'll go with it

acc_HR_rows <- which(condense_records$ID %in% acc_HR_IDs == T)
acc_HR_data <- condense_records[acc_HR_rows,]

set.seed(123)
data_split <- caret::createDataPartition(y=acc_HR_data$heart_rate, p=0.7)

ggplot(data = acc_HR_data) + 
  geom_point(aes(x=speed, y=heart_rate))

acc_HR_data <- acc_HR_data %>% mutate(s.div.hr = speed*10/heart_rate)

acc_hr_by_alt10.30.60 <- acc_HR_data %>%
  group_by(ID, alt_lag10) %>%
  summarise(time = max(time), across(-time,mean)) %>%
  group_by(ID, alt_lag30) %>%
  summarise(time = max(time), across(-time,mean)) %>%
  group_by(ID, alt_lag60) %>%
  summarise(time = max(time), across(-time,mean)) %>%
  mutate(category = cut(alt_lag30, 
                        breaks = c(-Inf, -20, -10, -3, 3, 10, 20, Inf),
                        labels = c("outlier","big_down", "down", 
                                   "flat", "up", "big_up", 
                                   "outlier")
                        ), 
         category = as.factor(category))

ggplot(acc_hr_by_alt10.30.60, aes(x = alt_lag30)) + 
  geom_histogram(bins = 30) + 
  xlim(-25,25)

acc_hr_by_alt10.30.60 %>%
  filter(category == "flat") %>%
  filter(speed > 2 & speed < 5.4) %>% #5.4 ~ 5:00 min/mi ~ max aerobic speed
  filter(heart_rate > 130) %>%
ggplot(data = ., aes(x=speed, y=heart_rate)) + geom_point()

# model1 <- lm(s.div.hr ~ speed * heart_rate * alt_lag10 * alt_lag30 * alt_lag60 * cadence, data = acc_hr_by_alt10.30.60)
# model1 <- step(model1, direction = "backward", trace=FALSE) 
# summary(model1)
# car::Anova(model1)

#LM for minimal incline
lm_flat <- acc_hr_by_alt10.30.60 %>%
  filter(category == "flat") %>%
  filter(speed > 2 & speed < 5.4) %>% 
  filter(heart_rate > 130) %>%
  lm(heart_rate ~ speed * alt_lag60, data = .)

summary(lm_flat)
lm_flat$coefficients

lm_up <- acc_hr_by_alt10.30.60 %>%
  filter(category == "up" | category == "big_up") %>%
  filter(speed > 2 & speed < 5.4) %>% 
  filter(heart_rate > 130) %>%
  lm(heart_rate ~ speed * alt_lag60, data = .)

summary(lm_up)
lm_up$coefficients

lm_down <- acc_hr_by_alt10.30.60 %>%
  filter(category == "down" | category == "big_down") %>%
  filter(speed > 2 & speed < 5.4) %>% 
  filter(heart_rate > 130) %>%
  lm(heart_rate ~ speed * alt_lag60, data = .)

summary(lm_down)
lm_down$coefficients

lm_flat$coefficients
lm_up$coefficients
lm_down$coefficients
#HR = 9.06 * speed - 0.44 * alt_lag60 + 0.07*speed*alt_lag60 + 116 for flat
#HR = 9.62 * speed + 0.53 * alt_lag60 - 0.23*speed*alt_lag60 + 115 for up
#HR = 9.06 * speed - 1.74 * alt_lag60 + 0.51*speed*alt_lag60 + 117 for down

9.62*4.5 + 0.53*10 - 0.23*4.5*10 + 116
#Need to pull in better elevation data with get_elev_point() from elevatr
#Could also find gradient by creating lag alt then dividing by distance covered
  #Would need to fix units on distance and *100 for percent too
  #Would make GAP much easier to interpret

#Also need to group by ID and speed instead of ID and whatever else
#Holding speed constant lets me see how increase/decrease grade affects HR

#After that, could be worthwhile to try NN or something like that. 
#GAM from library(gam) might be better at first, more interpretable 