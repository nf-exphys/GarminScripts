library(arsenal)
library(fitFileR); library(data.table); library(tidyverse); 

load("C:/Users/Nick.Foreman/Desktop/Nick Mobile Folders/School/2020-2021/R Files/GarminData/FitDFCleaned_0823.Rdata")
length(Fit.DF) <- 1351
n <- length(Fit.DF)

ListofNames <- vector('list', n)
for (i in 1:n) {
  if(nrow(Fit.DF[[i]]$activity) < 1) {print(i)}
  if(nrow(Fit.DF[[i]]$activity) > 1) {print(i*-1)}
}

names(Fit.DF[[1]])

for (i in 1:n) {
  
 if (any(head(Fit.DF[[i]]$device_info$product, 1) == "connect") == TRUE) {
   
   if(nrow(Fit.DF[[i]]$session) < 1) {print(i)}
   if(nrow(Fit.DF[[i]]$session) > 1) {print(i*-1)}
 }
  
}


# Figuring Out Times ------------------------------------------------------

times <- data.table(ActivityTimestamp=rnorm(2), SessionTimestamp=rnorm(2), 
                    SessionStartTime=rnorm(2), FileIDTimeCreated=rnorm(2))

for (i in 58:59) {
  class(i) <- class(Fit.DF[[1]]$activity$timestamp)
  times[i,] <- data.table(Fit.DF[[i]]$activity$timestamp, Fit.DF[[i]]$session$timestamp,
           Fit.DF[[i]]$session$start_time, Fit.DF[[i]]$file_id$time_created)
  times <- rbind(times, times[i,])
  
}
cat(as.POSIXct(Fit.DF[[1]]$activity$timestamp))
class(Fit.DF[[1]]$activity$timestamp)
times[1,] #returns row 1


# Consistent Naming -------------------------------------------------------

sumdataNames <- head(Fit.DF[[1]]$sum_data,1)
save(sumdataNames, file = "GarminData//sumdataNames.Rdata")

compare_df_cols(Fit.DF[[1]]$record, Fit.DF[[2]]$record, return = 'match')
alike(Fit.DF[[1]]$record, Fit.DF[[2]]$record)

#Option for comparing data frames
for (i in 1:3){
  
 #(all.equal(Fit.DF[[1]]$record, Fit.DF[[2]]$record, ignore_col_order = FALSE, ignore_row_order = TRUE, 
   #         check.attributes = FALSE))
  #summary(comparedf(Fit.DF[[1]]$record, Fit.DF[[2]]$record),max.print.vars = NA, max.print.obs = NA, tol.factor="labels")
  
  ListofNames[[i]] <- comparedf(Fit.DF[[1]]$record, Fit.DF[[i]]$record)
  
  #setdiff(order(names(Fit.DF[[1]]$record)), order(names(Fit.DF[[2]]$record))) 
}
#Would just have to compare ListofNames[[3]]$vars.summary$var.y to the same but $var.x


library(janitor); library(vetr)
