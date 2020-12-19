#Import libraries
import numpy as np
from fitparse import FitFile
import plotly.graph_objects as go

#Instantiate our HRV file
fit_file = FitFile('AC3F5204.FIT')

#Print out our HRV data
for record in fit_file.get_messages('hrv'):
  for record_data in record:
  print(record_data)
