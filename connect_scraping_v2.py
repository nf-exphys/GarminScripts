# -*- coding: utf-8 -*-
"""
Created on Sat Dec 30 15:54:40 2023

@author: Nick.Foreman
"""

import garth
from garth import GarthException
from getpass import getpass

email = input("Enter email address: ")
password = getpass("Enter password: ")
# If there's MFA, you'll be prompted during the login
garth.login(email, password)

garth.save("~/.garth")

garth.resume("~/.garth")


try:
    garth.client.username
except GarthException:
    print("Session is expired. You'll need to log in again")
    
sleep = garth.connectapi(
    f"/wellness-service/wellness/dailySleepData/{garth.client.username}",
    params={"date": "2023-07-05", "nonSleepBufferMinutes": 60},
)
list(sleep.keys())

stress =  garth.connectapi("/usersummary-service/stats/stress/weekly/2023-07-05/52")

garth.download("/download-service/files/activity")



#start here using updated garminconnect via Garth
from getpass import getpass

email = input("Enter email address: ")
password = getpass("Enter password: ")

garmin = garminconnect.Garmin(email, password)
garmin.login()

garmin.display_name


'get_activities',
'get_activities_by_date',
'get_activities_fordate',

import datetime
today = datetime.date.today()
lastweek = today - datetime.timedelta(days=7)

running_activities = garmin.get_activities_by_date(startdate = str(lastweek), 
                                                enddate = str(today), 
                                                activitytype = 'running')

activity_id = running_activities[0]['activityId']

zip_data = garmin.download_activity(running_activities[0]['activityId'])

#change directory to GarminData folder
import os
garmin_path = os.path.abspath("C:/Users/Nick.Foreman/Desktop/Code/GarminData")
os.chdir(garmin_path)

#set up the file path with activity_id and file extension
file_ending = str(activity_id) + ".zip"
zip_file_path = os.path.join(garmin_path, os.path.abspath("zip_fit_files"))
output_file = os.path.join(zip_file_path, file_ending)

with open(output_file, "wb") as fb: #not sure how this works but it does
    fb.write(zip_data)

msg = "Zip file downloaded for activity # {}".format(activity_id)
print(msg)

#garmin.get_activities_by_date(startdate = '2023-12-01', enddate = '2023-12-05')


#garmin.logout()