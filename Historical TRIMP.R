#Calculate TRIMP from One Off File

library(fitFileR); library(tidyverse);
loadfile <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles\\3220831446.fit"
GarminFile <- readFitFile(loadfile)

#Calculate TRIMP
HRfreq <- GarminFile$record %>%
  count(heart_rate) #makes data frame of frequencies of each HR value
names(HRfreq)[1] <- "HR" #Set name of first column to HR
names(HRfreq)[2] <- "number" #Set name of second column to number
#Consider removing HR values under 100 or 110, ideally find physiological rationale for this cutoff

HRfreqHRR <- HRfreq %>%
  mutate(HRR = ((HRfreq$HR-48)/(195-48))) #Create HRR column
rm(HRfreq) #Remove data frame with HR freq since it's not needed
HRfreqTRIMP <- HRfreqHRR %>%
  mutate(TRIMP = (HRfreqHRR$number / 60) * HRfreqHRR$HRR * (0.64 * exp(1.92*HRfreqHRR$HRR))) #Now apply TRIMP formula
TotalTRIMP <- sum(HRfreqTRIMP$TRIMP, na.rm = TRUE) #TRIMP value for the activity
TimeMinutes <- GarminFile$activity$total_timer_time/60 #Makes character with total activity time
TotalTRIMP.hour <- TotalTRIMP/TimeMinutes

rm(HRfreqHRR) #Remove data from with HR freq scaled to HRR since it's not needed

#The above code works but might not be best for long-term, repetitive use for each file. Using a function seems like a better plan

#Calculating TRIMP using a formula



