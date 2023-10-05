set root=C:\Users\Nick.Foreman\Anaconda3

call %root%\Scripts\activate.bat %root% 

%root%\python.exe "%~dp0\connect_scraping.py"

TIMEOUT /T 185

"C:\Program Files\R\R-4.2.2\bin\R.exe" CMD BATCH "%~dp0%Scripts\current\periodic_fit_import.R"

TIMEOUT /T 180

REM root is the location for anaconda, and the activate.bat is used to open Anaconda
REM this bypasses problems with module loading & modules being in the right spot
REM %~dp0 fills in the path with the current file location
REM timeout adds a delay before running periodic_fit_import to clean the files