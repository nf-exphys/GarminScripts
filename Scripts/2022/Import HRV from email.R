#Call python script to get download link

library(reticulate); library(utils)
path_to_python <- "C:/Users/nick.foreman/AppData/Local/Programs/Python/Python39/"
use_python(path_to_python)

source_python("pull_hrv_from_gmail.py")

link <- py$link

#Check if the download link is still live
testURL <- function(url){
  out <- tryCatch({
    
    readLines(con=url, warn=FALSE) 
  },
  error=function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  warning=function(cond) {
    return(NULL)
  },finally={
    # NOTE:
    # Here goes everything that should be executed at the end,
    # regardless of success or error.
    # If you want more than one expression to be executed, then you 
    # need to wrap them in curly brackets ({...}); otherwise you could
    # just have written 'finally=<expression>' 
    
  }
  
  )
  return(out)
}

url_check <- testURL(link)

continue <- T
if(is.null(url_check) == T){
  continue <- F
}

#write something to clear out old export.zip files from downloads folder

if(continue == T){
  #open download link
  browseURL(url = link)
  
  #copy from downloads folder
  file.copy(from = "C:\\Users\\nick.foreman\\Downloads\\export.zip", to = ".")
  
  #store in current_hrv
  unzip(zipfile = "./export.zip", exdir = "./current_hrv")
  
  #copy all the text files, expect a lot of FALSE which is okay
  new_hrv_files <- list.files(path = "./current_hrv/Nick_Foreman/", pattern = ".txt", full.names = T)
  file.copy(new_hrv_files, "HRV Files to be Read")
  
  #clear out intermediate files
  file.remove("./export.zip")
  unlink("./current_hrv", recursive = T)
  unlink("C:\\Users\\nick.foreman\\Downloads\\export.zip", recursive = T)
  
  print("Files successfully downloaded and copied")
  } else(print("Download link unsuccessful."))



