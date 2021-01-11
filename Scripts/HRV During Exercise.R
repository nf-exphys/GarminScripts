library(tidyverse, warn.conflicts = F); library(RHRV)
library(fitFileR)

#Assorted activities from late December & early January 2021
easy_run <- fitFileR::readFitFile(fileName = "E:\\GARMIN\\ACTIVITY\\ACS81007.FIT") #easy run with Coach K
prog_bike <- readFitFile(fileName = "E:\\GARMIN\\ACTIVITY\\B12H3747.FIT") #progression bike ride
long_run <- readFitFile(fileName = "E:\\GARMIN\\ACTIVITY\\B1285039.FIT") #115 min long run w/~10 min progression

hrv <- prog_bike$hrv$time #set the hrv data as its own object for more succinct code
hrv <- long_run$hrv$time
hrv <- easy_run$hrv$time

#Alternatively, can read in Exercise HRV data from CSV
  #Example: CSV from 01/09/2021 easy long run with Dylan
hrv <- read_csv(file = "./Data/ExportedRunData/Cleaned_CSVs/2021_01_09 10_37_24 hrvdata.csv")

#Read in multiple CSVs
hrv_files <- list.files(path = "./Data/ExportedRunData/Cleaned_CSVs/", 
                        pattern="* hrvdata.csv", full.names = T)

read_csv_filename <- function(filename){
  ret <- read_csv(filename)
  filename <- str_remove(string = filename, pattern = "./Data/ExportedRunData/Cleaned_CSVs/")
  filename <- str_remove(string = filename, pattern = " hrvdata.csv")
  ret$Source <- filename #EDIT
  ret
}

hrv_list <- lapply(hrv_files, read_csv_filename)

library(doParallel)
n <- detectCores()
registerDoParallel(n-3)

all_rr_data <- list()

all_rr_data <- foreach(i=1:length(hrv_list), .packages = "tidyverse") %dopar% {
  all_rr_data[[i]] <- hrv_list[[i]] %>% rename(rr = hrv) %>%
    mutate(rr = case_when(rr < 1 ~ rr)) %>%
    drop_na()
}

#Now need to do artifact correction
artifact_correction_threshold <- 0.05
#filtered_rr_data <- tibble()

#Something isn't right with Source, need to figure that out
for(j in 1:length(all_rr_data)){
  
  current_rr <- all_rr_data[[j]]
  
  filtered_rr_data <- foreach(i=2:nrow(current_rr), .packages = "tidyverse") %dopar% { #start at 2 because of i-1
    
    #Set high and low bounds
    low_bound <- current_rr$rr[i-1]*(1-artifact_correction_threshold)
    high_bound <- current_rr$rr[i-1]*(1+artifact_correction_threshold)
    
    if ((low_bound < current_rr$rr[i]) & (current_rr$rr[i] < high_bound)){
      
      #filtered_rr_data <- rlist::list.append(filtered_rr_data, current_rr$rr[i])
      filtered_rr_data <- filtered_rr_data %>% 
        bind_rows(.,current_rr[i,])
        
      
    } 
  }
  
  all_rr_data[[j]] <- filtered_rr_data
  
}

for(i in 1:length(all_rr_data)){plot(unlist(all_rr_data[[i]]), 
                                     main = all_rr_data[[i]][[1]])}
?plot()
unique(all_rr_data[[j+20]]$Source)

#Adjust filter based on plot
 all_rr_data[[1]] %>% 
   unlist %>% as_tibble_col() %>%
   filter(value < 0.8) 

for(i in 1:length(all_rr_data)){
  all_rr_data[[i]] <- all_rr_data[[i]] %>%
    unlist() %>%
    as_tibble_col()
  }

#writes to txt
write.table(filtered_rr_data, file = "./Text Files/01_09_2021_easy_long_run.txt", row.names = F, col.names = F) 

hrv.data <- CreateHRVData() #create structure to store HRV data
hrv.data <- SetVerbose(hrv.data, T)
hrv.data <- LoadBeat(fileType = "RR", HRVData = hrv.data, 
                     Recordname = "./Text Files/01_09_2021_easy_long_run.txt")
nihr <- BuildNIHR(hrv.data)
PlotNIHR(nihr) #close but not quite

nihr <- FilterNIHR(nihr, minbpm = 100, maxbpm = 165)
PlotNIHR(nihr) #better

#Split data and find DFA
library(nonlinearTseries)
step <- 120 #number of seconds per window

#Number of minutes divided by window size
n<- max(nihr$Beat$Time)/step

nihr$Beat$split <- cut_interval(1:nrow(nihr$Beat), floor(n), boundary = 0)

# one <- which(round(nihr$Beat$Time,0) == 300)
# two <- which(round(nihr$Beat$Time,0) == 600)
# three <- which(round(nihr$Beat$Time,0) == 900)
# four <- which(round(nihr$Beat$Time,0) == 1200)
# five <- which(round(nihr$Beat$Time,0) == 1500)
# 
# nihr$Beat$split <- as.character(nihr$Beat$split)
# nihr$Beat$split[1:one[1]] <- 1
# nihr$Beat$split[one[2]:two[1]] <- 2
# nihr$Beat$split[two[2]:three[1]] <- 3
# nihr$Beat$split[three[2]:four[1]] <- 4
# nihr$Beat$split[four[2]:five[1]] <- 5
# nihr$Beat$split[(five+1):length(nihr$Beat$split)] <- 6

split_data <- nihr$Beat %>%
  group_by(split) %>%
  group_split(.keep = T)
#View(split_data)

#Find HR for each split
summary_dfa <- nihr$Beat %>%
  group_by(split) %>%
  summarise(HR = mean(niHR))
#View(summary_dfa)

dfa_list <- list()
i=1
for (i in 1:length(split_data)){
#if(nrow(split_data[[i]])<(step/2)){next}

dfa_analysis <- nonlinearTseries::dfa(time.series = split_data[[i]]$RR, npoints = 30, window.size.range = c(4,300))

regression_range <- c(4,30) #based on Kubios

dfa_est <- estimate(dfa_analysis, do.plot=T, regression.range = regression_range)
dfa_est[1]
dfa_list <- rlist::list.append(dfa_list, dfa_est[1])
}

summary_dfa <- dfa_list %>% unlist() %>% as_tibble_col() %>% cbind(summary_dfa,.)

lm_object <- summary_dfa %>%
  #filter(value < 0.75) %>%
  lm(value ~ HR, data = .) 

as.numeric(lm_object$coefficients[1])
as.numeric(lm_object$coefficients[2])

summary_dfa %>%
  #filter(value < 0.75) %>%
ggplot(data = ., aes(x=HR, y=value)) + geom_point() + geom_smooth(method = "lm")


# #Pulls together RR intervals from single file if in list
# rr_data <- matrix(ncol=1)
# for (i in 1:length(hrv)){
#   if("list" %in% class(hrv)){
#     if (i==1){
#       rr_data <- tibble(hrv=as.numeric(unlist(hrv[i]))) %>%
#         pivot_longer(cols = hrv, names_to = NULL, values_to = "rr") %>%
#         mutate(rr = case_when(rr < 60 ~ rr)) #drops 'NA' values
#       } else{
#       rr_data <- tibble(hrv=as.numeric(unlist(hrv[i]))) %>%
#         pivot_longer(cols = hrv, names_to = NULL, values_to = "rr") %>%
#         mutate(rr = case_when(rr < 60 ~ rr)) %>% #drops 'NA' values
#         bind_rows(rr_data,.) %>%
#         filter(is.na(rr) == F) %>%
#         select(rr)
#     }
#   } else {
#     stop("HRV data isn't a list. This for loop isn't needed")
#   }
#   
# }
# 
# #Filters RR intervals from single file if in tibble already
# rr_data <- hrv %>% 
#   rename(rr = hrv) %>%
#   mutate(rr = case_when(rr < 60 ~ rr)) %>%
#   drop_na()
# 
# #Plot of RR intervals over time
# ggplot(data = rr_data, aes(y=rr, x=row_number(rr))) + geom_point()