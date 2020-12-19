library(reticulate); library(tidyverse)

#Sys.which("python")
#use_python("\\Desktop\\python-3.9.1-amd64.exe")

#reticulate::source_python("C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\Fall 2020 classes\\R Files\\GarminData\\HRV Thresholds\\hrv_thresholds.py")


library(fitFileR); library(purrr)

files <- list.files(path = "./HRV Thresholds/", pattern = "*.FIT", full.names = T)
files[16]
garminfile <- fitFileR::readFitFile(files[16])

head(garminfile$hrv) #quick confirmation that HRV data is there

#plot HR and speed to get a better idea of the type of run
nrow(garminfile$record)
ggplot(data = garminfile$record[400:3239,], aes(x=timestamp, y=heart_rate)) + geom_point()
ggplot(data = garminfile$record, aes(x=timestamp, y=speed)) + geom_point()

hrv <- garminfile$hrv$time #set the hrv data as its own object for more succinct code
length(hrv)

rr_data <- matrix(ncol=1)
for (i in 1:length(hrv)){
  if (i==1){
  rr_data <- tibble(hrv=as.numeric(unlist(hrv[i]))) %>%
    pivot_longer(cols = hrv, names_to = NULL, values_to = "rr") %>%
    mutate(rr = case_when(rr < 1.2 ~ rr)) #drops values greater than 1.2
  } else{
  rr_data <- tibble(hrv=as.numeric(unlist(hrv[i]))) %>%
    pivot_longer(cols = hrv, names_to = NULL, values_to = "rr") %>%
    mutate(rr = case_when(rr < 1.2 ~ rr)) %>% #drops values greater than 1.2
    bind_rows(rr_data,.) %>%
    filter(is.na(rr) == F) %>%
    select(rr)
  }
}
nrow(rr_data)

ggplot(data = rr_data, aes(y=rr, x=row_number(rr))) + geom_point()

threshold <- 0.10 #apply Marco's recommended threshold detection
rr_data2 <- rr_data %>%
  mutate(
    rr2 = case_when(rr>(lag(rr,1)*(1-threshold)) ~ rr), #keep if current rr is above low bound
    rr2 = case_when(rr<(lag(rr,1)*(1+threshold)) ~ rr)  #keep if current rr is below upper bound
  ) %>%
  filter(is.na(rr2) ==F) %>%
  select(rr2)

#calculate RMSSD
library(varian)
library(mltools)

n <- length(rr_data2$rr2)/200 #number of bins to make if we want 20 observations in each
rr_data2$bin <- bin_data(rr_data2$rr2, bins = n, binType = "quantile")

rr_data3 <- rr_data2 %>% 
  group_by(bin) %>%
  summarise(
    rmssd = rmssd(rr2)
    ) 

ggplot(data = rr_data3, aes(x=row_number(rmssd), y=rmssd)) + geom_point()
ggplot(data = garminfile$record, aes(x=timestamp, y=heart_rate)) + geom_point()

rr_data <- rr_data %>%
  mutate(
    hr = 60/rr
  )

p1 <- ggplot(data = rr_data2, aes(y=rr2, x=row_number(rr2))) + geom_point()
print(p1)
p2 <- ggplot(data = garminfile$record, aes(x=timestamp, y=heart_rate)) + geom_point()
gridExtra::grid.arrange(p1, p2, ncol=2)
nrow(rr_data)
nrow(garminfile$record)

#write.table(rr, file = "LongRun.HRVTest.txt", row.names = F, col.names = F) #writes to txt

library(RHRV) #load HRV library 
hrv.data <- CreateHRVData() #create structure to store HRV data
hrv.data <-  SetVerbose(hrv.data, TRUE )
hrv.data <- RHRV::LoadBeatVector(hrv.data, beatPositions = rr_data)
hrv.data <- BuildNIHR(hrv.data)
hrv.data <- FilterNIHR(hrv.data, minbpm = 30, maxbpm = 200)
PlotNIHR(hrv.data)

length(garminfile$record$heart_rate)


nihr <- BuildNIHR(hrv.data)
class(rr)
rrcomp <- data.frame(nihr$Beat$RR[2:length(nihr$Beat$RR)], rr) #comparing RR intervals
colnames(rrcomp)[1] <- "nihr.RR"
rrcomp$nihr.RR.scaled <- rrcomp$nihr.RR/1000
rrcomp$difference <- rrcomp$nihr.RR.scaled - rrcomp$rr
plot(record$heart_rate)
length(nihr$Beat$niHR)
plot(record$heart_rate[1:3087], nihr$Beat$niHR[1:3087])

#no difference between nihr RR and measured RR even without filtering
rrcomp %>% arrange(desc(abs(difference)))  %>% head(n=5) 

nihr.filt <- RHRV::FilterNIHR(nihr, minbpm = 120, maxbpm = 180)
plot(record$heart_rate[1:2204], nihr.filt$Beat$niHR)
plot(nihr.filt$Beat$Time, nihr.filt$Beat$niHR) #plots filtered HR vs time, appears similar to Garmin HR graph

