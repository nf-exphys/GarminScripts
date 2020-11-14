library(RHRV); library(stringr)

# Import Data ---------------------------------------------------

DateChrTxt <- list.files(path = ".\\HRV Files to be Read\\", pattern = ".txt", full.names = FALSE) 
  #For use in naming/setting datetime field
DateChrNoTxt <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(DateChrTxt)) #removes file extension
FilesToRead <- list.files(path = ".\\HRV Files to be Read", pattern = ".txt", full.names = TRUE)
  #Creates a list with the HRV files and their file path (better than pasting in the file path)

#Sets n as the number of files to be read
n <- length(FilesToRead)

#Creates an empty list to be filled with data frames
#hrvdb <- vector('list', n)
#i=150
#Read in files from FilesToRead; convert read-in file to a data frame and store it in a list; 
for (i in 1:n) {
  HRVdata <- CreateHRVData() #Create data structure to store HRV data
  
  #Converting yyyy-mm-dd to dd/mm/yyyy HH:MM:SS
  splitDC <- unlist(str_split(string = DateChrNoTxt[i], pattern = "[:punct:]|[:blank:]")) 
  #splits DateChr at spaces and punctuation then applies unlist to fix output
  d <- paste0(splitDC[c(3,2,1)], collapse = "/") #date as dd/mm/yyyy
  t <- paste0(splitDC[c(4,5,6)], collapse = ":") #time as HH:MM:SS
  dt <- paste0(c(d,t), collapse = " ")
  
  HRVdata <- LoadBeatRR(HRVdata, RecordName = DateChrTxt[i], scale = 0.001,
                        RecordPath = ".\\HRV Files to be Read", datetime = dt)
    #loads RR data
  HRVdata <- BuildNIHR(HRVdata)
    #interprets RR data into HR
  HRVdata <- FilterNIHR(HRVdata, minbpm = 30, maxbpm = 75)
    #filters within physiological ranges
  HRVtime <- CreateTimeAnalysis(HRVdata, size = 5) #performs simple time analysis with size set to default
  if (length(HRVdata$Beat$Time) > 400){ #if there are lots of rows (i.e. it's exercise not resting)
    print(i) #As of 10/19/20, this if statement doesn't do anything - need to update
    #next #skips over copying data into dataframe
  }
  if (i == 1){ #the first time through, just make the data frame
    timeanalysis <- cbind(as.data.frame(HRVtime$datetime),
                          as.data.frame(HRVtime$TimeAnalysis))
    }
  if (i>1){ #after the first time, create a temporary storage timeanalysis.new 
    timeanalysis.new <- cbind(as.data.frame(HRVtime$datetime),as.data.frame(HRVtime$TimeAnalysis))
    timeanalysis <- rbind(timeanalysis, timeanalysis.new) #then add that row to the previous data frame
  }
  
}
write.csv(timeanalysis, file = "Jan20toSept20HRVdata.csv")

##### Frequency analysis #####
#NEXT STEPS: figure out frequency and spectral analyses

Interp.HRVdata <- InterpolateNIHR(HRVdata) #Creates equally spaced HR series
#Gives a bunch of NA warnings, not sure if that's normal?
  #Also not needed for time analysis

Interp.HRVdata <- CreateFreqAnalysis(Interp.HRVdata)

Interp.HRVdata <- CalculatePowerBand(Interp.HRVdata, indexFreqAnalysis = 1,
                    type = "wavelet", wavelet = "la8",
                    bandtolerance = 0.01, relative = FALSE,
                    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                    LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )
PlotPowerBand(Interp.HRVdata)

##### Plotting RMSD #####
