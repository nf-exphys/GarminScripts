# -*- coding: utf-8 -*-
"""
Created on Wed Feb 16 18:32:26 2022

@author: Nick.Foreman
"""

import logging
import datetime
import re
import os

import zipfile

from time import sleep

#use this to stagger download times
from random import randint

from garminconnect import (
    Garmin,
    GarminConnectConnectionError,
    GarminConnectTooManyRequestsError,
    GarminConnectAuthenticationError,
)

#ideally add a function to get IDs from file name
#def extract_id_from_file(file_path, ):
    

# Configure debug logging
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

today = datetime.date.today()
last2week = today - datetime.timedelta(days=14)

pwd = open("Desktop/garmin_login.txt").read()

try:
    # API

    ## Initialize Garmin api with your credentials
    api = Garmin("nick.foreman@pillarit.com", pwd)

    ## Login to Garmin Connect portal
    api.login()

    # Get full name from profile: #logger.info(api.get_full_name())
    # Get unit system from profile: #logger.info(api.get_unit_system())
    # Get last activity: #logger.info(api.get_last_activity())
    
    #not sure how this logger thing works...
    
    #activities = api.get_activities(0,10) # 0=start, 1=limit
    #logger.info(activities)

    # Get activities data from startdate 'YYYY-MM-DD' to enddate 'YYYY-MM-DD', with (optional) activitytype
    # Possible values are [cycling, running, swimming, multi_sport, fitness_equipment, hiking, walking, other]
    
    #get all running activities for the last 2 weeks from today
    activitytype = 'running'
    running_activities = api.get_activities_by_date(today, last2week, activitytype)

    #compare to files already downloaded (raw_fit_files)
    raw_file_path = 'Desktop/Desktop Code/GarminData/Data/raw_fit_files'
    fit_files = os.scandir(raw_file_path)
    
    fit_files_ids = list()
    
    #get IDs for the files that have already been downloaded and extracted
    for fit_file in fit_files:
        #extract number from file name
        id = re.findall(pattern="\d", string=str(fit_file))
        id = int(''.join(id))
        #print(str(id))
        fit_files_ids.append(id) #add current id to the list of ids  
    
    #compare to zip files already downloaded but not extracted
    zip_files = os.scandir('Desktop/Desktop Code/GarminData/Data/zip_fit_files')
        
    zip_files_ids = list()
    
    #get IDs for the zip files that have been downloaded but not extracted
    for zip_file in zip_files:
        #extract number from file name
        id = re.findall(pattern="\d", string=str(zip_file))
        id = int(''.join(id))
        #print(str(id))
        zip_files_ids.append(id) #add current id to the list of ids  
    
    for activity in running_activities:
        activity_id = activity["activityId"]
        #print(str(activity_id))
        
        #if id has already been downloaded and processed, don't download again
        if activity_id in fit_files_ids:
            continue
        
        #if id has already been downloaded but not processed, don't downloaded again
        if activity_id in zip_files_ids:
            continue
        
        #otherwise, download it to zip_fit_files
        else: 
            zip_data = api.download_activity(activity_id, dl_fmt=api.ActivityDownloadFormat.ORIGINAL)
            output_file = f"./Desktop/Desktop Code/GarminData/Data/zip_fit_files/{str(activity_id)}.zip"
            with open(output_file, "wb") as fb: #not sure how this works but it does
                fb.write(zip_data)
    
            msg = "Zip file downloaded for activity # {}".format(activity_id)
            print(msg)
            sleep(randint(5,10)) #add a delay to make download more realistic
            
    #now get all the zip files so the loop can extract them all       
    zip_path = 'Desktop/Desktop Code/GarminData/Data/zip_fit_files'
    zip_files = os.listdir(zip_path)
    
    #for loop to extract all the zip files real quick
    for zip_file in zip_files:
        zip_file = (zip_path + "/" + str(zip_file)) #adds on file path
        with zipfile.ZipFile(zip_file,"r") as zip_ref:
            zip_dir = raw_file_path + "/"
            zip_ref.extractall(zip_dir) #extracts zips and writes to raw_file_path
    
    #confirm that all zip files now exist in raw_fit_files
    fit_files = os.scandir(raw_file_path)
    
    fit_files_ids = list()
    
    #get IDs for the files that have already been downloaded and extracted
    for fit_file in fit_files:
        #extract number from file name
        id = re.findall(pattern="\d", string=str(fit_file))
        id = int(''.join(id))
        fit_files_ids.append(id) #add current id to the list of ids  
    
    zip_files_ids = [(s.replace('.zip', '')) for s in zip_files]
    zip_files_ids = [int(x) for x in zip_files_ids]
    
    raw_fit_ids = set(fit_files_ids)
    zip_files_ids = set(zip_files_ids)
     
    #If all zip files are in raw fit files, then delete the zip files
    if zip_files_ids.issubset(raw_fit_ids):
        for z in zip_files:
            z = (zip_path + "/" + str(z)) #adds on file path
            #print(z)
            os.remove(z)
    
except (
        GarminConnectConnectionError,
        GarminConnectAuthenticationError,
        GarminConnectTooManyRequestsError,
    ) as err:
    logger.error("Error occurred during Garmin Connect communication: %s", err)
    
    