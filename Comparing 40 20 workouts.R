#Compare 40:20s with and without 1:30 hard beforehand

library(fitFileR); library(data.table); library(tidyverse); 
library(groupdata2) #for grouping into 20 sec windows
library(zoo) #for rollmean

nohardb4 <- "./AAUH0410.FIT"; nohardb4 <- readFitFile(nohardb4)
hardb4 <- "./AB5G2216.FIT"; hardb4 <- readFitFile(hardb4)

hardb4 <- lapply(hardb4, data.frame, stringsAsFactors = FALSE) 
nohardb4 <- lapply(nohardb4, data.frame, stringsAsFactors = FALSE)
#hardb4 <- hardb4$record; nohardb4 <- nohardb4$record

#Workout with 90 sec hard ("hardb4")
#Start of first set of 40:20s: 2020-11-05 16:24:17
#Start time of 180 sec lap: 2020-11-05 16:34:17
  #Because this workout had 9 sets instead of 10, subtract 1 minute from 180 start time for comparison 
start <- which(grepl("2020-11-05 16:24:17",hardb4$record$timestamp))
end <- which(grepl("2020-11-05 16:33:17",hardb4$record$timestamp))
firstset.hardb4 <- hardb4$record[start:end,]

#Workout without 90 sec hard ("nohardb4")
#Start of first set of 40:20s nohardb4: 2020-10-30 17:04:10
#Start time of 180 sec lap: 2020-10-30 17:13:12
start <- which(grepl("2020-10-30 17:04:10",nohardb4$record$timestamp))
end <- which(grepl("2020-10-30 17:13:12",nohardb4$record$timestamp))
firstset.nohardb4 <- nohardb4$record[start:end,]

#Remove 1 row from nohardb4 to make sure they have the same # of rows, allows for merging of data frames
end <- nrow(firstset.nohardb4)-1
firstset.nohardb4 <- firstset.nohardb4[1:end,]
firstset <- cbind(firstset.nohardb4, firstset.hardb4)
firstset <- firstset[-c(2,3,11,12)] #get rid of latitude and longitude

#Group data by 20 row intervals (should be 20 sec each?) and summarize key metrics for each
data1 <- firstset %>% #no hard before
  group(n=20, method = "greedy") %>%
  summarise(HR = mean(heart_rate), s = mean(speed), d = mean(distance), 
            cadence = mean(cadence), alt = mean(altitude))
data2 <- firstset %>% #hard before
  group(n=20, method = "greedy") %>%
  summarise(HR = mean(heart_rate.1), s = mean(speed.1), d = mean(distance.1), 
            cadence = mean(cadence.1), alt = mean(altitude.1))
remove(firstset, firstset.hardb4, firstset.nohardb4, hardb4, nohardb4)

#Merge data together, assign hard vs. no hard
#data <- cbind(data1, data2) #easier to read & graph, harder for ANOVA
  #data <- data[,-7] #remove second group variable
data <- rbind(data1, data2)
type <- data.frame(type = c("hard")); data <- cbind(data, type); data$type[1:28] <- "nohard"
data$type <- as.factor(data$type)
colnames(data)[1] <- "interval"
colnames(data)

#Plot of HR by workout type
ggplot(data = data, aes(x=interval, y=HR, colour=type, group=type)) + geom_point() + 
geom_line() +
geom_smooth(method="gam", formula = y~s(x))

#Normality assumptions for ANOVA
data %>%
  group_by(type) %>%
  shapiro_test(HR, s, d, alt) #p > 0.05 for HR, can't assume normality

#correction for HR
ggplot(data, aes(x = HR1)) + geom_histogram()
ggplot(data, aes(x = sqrt(HR1))) + geom_histogram()
ggplot(data, aes(x = log10(HR1))) + geom_histogram()
ggplot(data, aes(x = (HR1^1/3))) + geom_histogram()
ggplot(data, aes(x = (HR1^13))) + geom_histogram()
ggplot(data, aes(x = scale(HR1))) + geom_histogram()


#Giving it a shot without HR correction (sample size might be enough?)
anova1 <- aov(HR ~ type, data = data)
anova2 <- aov(HR ~ interval, data = data)
anova3 <- aov(HR ~ type + interval, data = data)
anova4 <- aov(HR ~ type * interval, data = data)
summary(anova3)

#Still need to do post-hoc testing. Tukey HSD might not be right for this...
posthoc1 <- TukeyHSD(x=anova3, 'type',conf.level=0.95)
tukey.plot.aov<-aov(HR ~ type:interval, data=data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

test1 <- (TukeyHSD(anova3, "interval"))
plot(test1)
test2 <- test1$interval
test2 <- as.data.frame(test2)
sig <- which(test2$`p adj` < 0.05)
test2[sig,]

#Old rolling average code
plot(rollmean(firstset.hardb4$heart_rate, 5))

#Filling in NAs
# if (length(which(is.na(hardb4$heart_rate))) > 0) { #if there are NAs 
#  miceMod <- mice(hardb4, method = "rf", print = F) 
#  #calculates values to impute using random forest, doesn't print output
#  hardb4 <- complete(miceMod)
#  #applies those values to data.test
# }
# iforest<- isolationForest$new() #apply iforest algorithms
# iforest$fit(data.test)

#data.test$pred <- iforest$predict(data.test)
#data.test$outlier <- as.factor(ifelse(data.test$pred$anomaly_score >=0.595, "outlier", "normal")) 
#arbitrarily setting score>0.6 as an outlier
