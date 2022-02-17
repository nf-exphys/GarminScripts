# -*- coding: utf-8 -*-
"""
Created on Wed Feb 16 18:32:26 2022

@author: Nick.Foreman
"""

import logging
import datetime

from garminconnect import (
    Garmin,
    GarminConnectConnectionError,
    GarminConnectTooManyRequestsError,
    GarminConnectAuthenticationError,
)

# Configure debug logging
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

today = datetime.date.today()
lastweek = today - datetime.timedelta(days=7)

pwd = open("Desktop/garmin_login.txt").read()

try:
    # API

    ## Initialize Garmin api with your credentials
    api = Garmin("nick.foreman@pillarit.com", pwd)

    ## Login to Garmin Connect portal
    api.login()

    # USER INFO

    # Get full name from profile
    #logger.info(api.get_full_name())

    ## Get unit system from profile
    #logger.info(api.get_unit_system())
    
    #not sure how this logger thing works...
    
    activities = api.get_activities(0,10) # 0=start, 1=limit
    logger.info(activities)

    # Get activities data from startdate 'YYYY-MM-DD' to enddate 'YYYY-MM-DD', with (optional) activitytype
    # Possible values are [cycling, running, swimming, multi_sport, fitness_equipment, hiking, walking, other]
    
    startdate = '2022-02-16'
    enddate = '2022-01-01'
    activitytype = 'running'
    running_activities = api.get_activities_by_date(startdate, enddate, activitytype)

    # Get last activity
    #logger.info(api.get_last_activity())
    
    folder_to_save = "./Desktop Code/GarminData/"
    
    for activity in activities:
        activity_id = activity["activityId"]
        zip_data = api.download_activity(activity_id, dl_fmt=api.ActivityDownloadFormat.ORIGINAL)
        output_file = f"./{str(activity_id)}.zip"
        print(output_file)
        with open(output_file, "wb") as fb:
            fb.write(zip_data)

#Next steps:
    #Fit 
    
except (
        GarminConnectConnectionError,
        GarminConnectAuthenticationError,
        GarminConnectTooManyRequestsError,
    ) as err:
    logger.error("Error occurred during Garmin Connect communication: %s", err)
    
    