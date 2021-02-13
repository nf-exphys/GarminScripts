library(dbplyr); library(odbc); library(DBI)
library(RODBC)
sort(unique(odbcListDrivers()[[1]]))

#Location of SQL server database
file_path <- "C:\\Program Files\\Microsoft SQL Server\\MSSQL15.MSSQLSERVER\\MSSQL\\DATA\\"

con <- dbConnect(odbc(), 
                 Driver = "{SQL Server}", 
                 Server = "{BRUTUS\\SQLEXPRESS}", 
                 Database = "{RunningData}", 
                 UID      = rstudioapi::askForPassword("Database user"),
                 PWD      = rstudioapi::askForPassword("Database password"),
                 TrustedConnection = "TRUE",
                 Port     = 1433)

DBI::dbConnect(odbc::odbc(),
               Driver   = "[your driver's name]",
               Server   = "[your server's path]",
               Database = "[your database's name]",
               UID      = rstudioapi::askForPassword("Database user"),
               PWD      = rstudioapi::askForPassword("Database password"),
               Port     = 1433)

con<-odbcDriverConnect(connection="Driver={SQL Server};server=BRUTUS\\SQLEXPRESS;database=RunningData;trusted_connection=TRUE;")


dbconnection <- odbcDriverConnect("Driver=ODBC Driver 17 for SQL Server;Server=BRUTUS\\SQLEXPRESS; Database=RunningData;Uid=; Pwd=; trusted_connection=yes")
