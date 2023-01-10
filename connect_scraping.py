# -*- coding: utf-8 -*-
"""
Created on Wed Feb 16 18:32:26 2022

@author: Nick.Foreman
"""

#This script is set up to be automated for daily activity download from Garmin Connect

#Define path to project to batch file works

#sys.path.append('C:\\Users\\Nick.Foreman\\Desktop\\Code\\GarminData')

#Import other modules
import logging
import datetime
import re
import os
import zipfile
import sys

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

#Set date for today and one week ago to use as bounds for download
today = datetime.date.today()
lastweek = today - datetime.timedelta(days=7)

#Changes directory to desktop to load login in
path = os.path.abspath("C:/Users/Nick.Foreman/Desktop")
os.chdir(path)

pwd = open("garmin_login.txt").read()

#Changes path back
path = os.path.abspath("C:/Users/Nick.Foreman")
os.chdir(path)

# Use try/except to handle errors

try:
    # API

    ## Initialize Garmin api with your credentials
    api = Garmin("nick.foreman@pillarit.com", pwd)

    ## Login to Garmin Connect portal
    api_attempt = api.login()

    if api_attempt:
        #get all running activities for the last week
        running_activities = api.get_activities_by_date(startdate = str(lastweek), 
                                                        enddate = str(today), 
                                                        activitytype = 'running')
    else:
        print("Connection to Garmin API failed!")
        sys.exit(1)
    
    #change directory to GarminData folder
    garmin_path = os.path.abspath("C:/Users/Nick.Foreman/Desktop/Code/GarminData")
    os.chdir(garmin_path)
    
    #compare to files already downloaded (raw_fit_files)
    raw_file_path = os.path.join(garmin_path, os.path.abspath("Data/raw_fit_files"))
        
    fit_files = os.scandir(raw_file_path)
    
    fit_files_ids = list()
    
    #get IDs for the files that have already been downloaded and extracted
    for fit_file in fit_files:
    #extract number from file name
        id = re.findall(pattern="\d", string=str(fit_file))
        id = int(''.join(id))
        fit_files_ids.append(id) #add current id to the list of ids  

    #compare to zip files already downloaded but not extracted
    zip_file_path = os.path.join(garmin_path, os.path.abspath("Data/zip_fit_files"))
    
    zip_files = os.scandir(zip_file_path)
    
    zip_files_ids = list()
    
    #get IDs for the zip files that have been downloaded but not extracted
    for zip_file in zip_files:
    #extract number from file name
        id = re.findall(pattern="\d", string=str(zip_file))
        id = int(''.join(id))
    #print(str(id))
        zip_files_ids.append(id) #add current id to the list of ids  

    #process the ID of each fit file from Garmin
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
            
            #set up the file path with activity_id and file extension
            file_ending = str(activity_id) + ".zip"
            output_file = os.path.join(zip_file_path, file_ending)
            
            with open(output_file, "wb") as fb: #not sure how this works but it does
                fb.write(zip_data)
    
            msg = "Zip file downloaded for activity # {}".format(activity_id)
            print(msg)
            sleep(randint(5,10)) #add a delay to make download more realistic
        
    #now get all the zip files so the loop can extract them all       
    zip_files = os.listdir(zip_file_path)

    if not zip_files: #if zip folder is empty, don't try to delete anything
        print("No zip files to extract/move/delete")    
        sys.exit(1)
    else: 
                #for loop to extract all the zip files and write to raw_fit_files     
        for zip_file in zip_files:
            zip_file = os.path.join(zip_file_path, zip_file)
            
            with zipfile.ZipFile(zip_file,"r") as zip_ref:
                zip_dir = raw_file_path + "/"
                zip_ref.extractall(zip_dir) #extracts zips and writes to raw_file_path
        
        print("Zip files extracted and written to raw_fit_files")
        
        #confirm that all zip files now exist in raw_fit_files
        fit_files = os.scandir(raw_file_path)
        
        fit_files_ids = list()
        
    #get IDs for the files that have already been downloaded and extracted
        for fit_file in fit_files:
        #extract number from file name
            id = re.findall(pattern="\d", string=str(fit_file))
            id = int(''.join(id))
            fit_files_ids.append(id) #add current id to the list of ids  
    
    #update zip file list just in case
        zip_files = os.scandir(zip_file_path)
        
        zip_files_ids = list()
        
        #get IDs for the zip files that have been downloaded but not extracted
        for zip_file in zip_files:
        #extract number from file name
            id = re.findall(pattern="\d", string=str(zip_file))
            id = int(''.join(id))
            print(str(id))
            zip_files_ids.append(id) #add current id to the list of ids  
    
        #zip_files_ids = [(s.replace('.zip', '')) for s in zip_files]
        zip_files_ids = [int(x) for x in zip_files_ids]
    
        raw_fit_ids = set(fit_files_ids)
        zip_files_ids = set(zip_files_ids)
     
        print("Set logic established")
     
        #If all zip files are in raw fit files, then delete the zip files
        if zip_files_ids.issubset(raw_fit_ids):
            for z in zip_files_ids:
                file_ending = str(z) + ".zip"
                zf = os.path.join(zip_file_path, file_ending)
                #print(zf)
                os.remove(zf)
    




except (
        GarminConnectConnectionError,
        GarminConnectAuthenticationError,
        GarminConnectTooManyRequestsError,
    ) as err:
    logger.error("Error occurred during Garmin Connect communication: %s", err)
    

api.logout()

  