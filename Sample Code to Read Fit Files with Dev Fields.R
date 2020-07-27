library(fitFileR)

## download the linked file from this issue - should work on any system
destfile = tempfile(fileext = ".zip")
url = "https://github.com/grimbough/fitFileR/files/4915543/July.5.Easy.Run.zip"
dl = download.file(url = url, destfile = destfile)
tdir = tempfile()
dir.create(tdir)
files = unzip(destfile, exdir = tdir)

## read file
fit <- readFitFile(files[1])

## examine the output
names(fit)

#Trying on my own
IndivFitFile <-  "C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\2020-2021\\R Files\\RandomFitFiles\\4470572265.fit"

Garmin2 <- readFitFile(IndivFitFile)