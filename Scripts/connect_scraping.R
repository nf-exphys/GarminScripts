library(RSelenium)
library(rvest)
library(tidyverse)

rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]