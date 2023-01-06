#Sandbox of old code/ideas

#Still need a way to compare against files that have already been read, not needed for this mass import
  #Look at this: https://stackoverflow.com/questions/17598134/compare-two-character-vectors-in-r

#Consider this code for calling scripts in eventual updating script
if(condition==X){
  source("myscript_A.R")
}else{
  source("myscript_B.R")
}

list2DF() #Could be good for converting fit file in list form to data frame, haven't tried though

#Copy line-by-line record data and place in new list of data frames
#Not sure why I chose to do this... Maybe to segment out record-based analysis vs. other analysis, such as HR zones or TRIMP?
#Seems less necessary if I'm going to be moving data to a database

#Create empty vector to add record data to
FitRecord <- vector('list', n)

#DIDN'T WORK FOR MASS IMPORT, COME BACK TO THIS!
for (i in 1:n) {
  
  if (Fit.DF[[i]]$sport$name == "Run") { #If the activity type is run, pull out the record and put it in FitRecord
    FitRecord[[i]] <- select(pluck(Fit.DF, i)$record, 1:7)
  }
  
  if (Fit.DF[[i]]$sport$name == "Run Indoor") { #If the activity type is run indoor...
    j <- ncol(Fit.DF[[i]]$record) #Create a value that corresponds to the number of columns, sometimes there are 6 columns and not 7
    FitRecord[[i]] <- select(pluck(Fit.DF, i)$record, all_of(1:j)) #Pulls out the record and puts it in FitRecord
  }
  
}
