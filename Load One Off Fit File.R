#Load one file

library(fitFileR); library(tidyverse);
loadfile <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles\\A6NI5029.fit"
GarminFile <- readFitFile(loadfile)

loadfile2 <- "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles\\A6JB2928.fit"
GarminFile2 <- readFitFile(loadfile2)