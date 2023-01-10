# GarminData

## Codebook

### Automation

As of 01/07/2023, download and procesing of fit files is automated using a batch file.
This is scheduled to run twice daily using Windows Task Scheduler

HRV data is exported to Dropbox by the user after morning measurement is complete.



### These scripts can be run routinely to handle the import/cleaning of fit files:

1) connect_scraping.py to pull fit files in from Garmin Connect
2) periodic_fit_import.R to process the data locally and turn fit files to CSVs
	This calls functions in master_fit_func.R and helper_fit_func.R



### Next, any number of other scripts can be used to examine data:

* fix_record_data.R does some data processing but shouldn't be sourced. It's more useful as an exploratory file.
* fix_and_smooth_elevation_data.R tries to correct drops in elevation and smooth data
	* Then, fix_bridge_drops.R or grade_adjusted_pace.R can be run
* record_density_func.R tries to use density functions to better understand running data.
	This is based on the idea that easy runs should be "more dense" (read: more similar pace) than workouts, which typically have more variation in pace
