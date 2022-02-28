# -*- coding: utf-8 -*-
"""
Created on Tue Oct  6 21:21:22 2020

@author: eachr
"""

import os 

abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)

import pandas as pd
import numpy as np

path = "Data\DailyData"

temp = []
for root, dirs, files in os.walk(path, topdown=False):
        for name in files:
            if ".csv" in name:
                name = os.path.join(path,name)
                df = pd.read_csv(name)
                df = df[['STATION', 'NAME', 'DATE','TMIN','TMAX', 'TAVG', 'SNWD', 'SNOW',
                         'PGTM','AWND', 'WSF2', 'WSF5', 'WSFG', 'TSUN', 'PRCP']]
                temp.append (df)
                print(df.columns)
                
                

dailydat = pd.concat(temp)

dailydat['calcTAVG'] = (dailydat['TMAX'] + dailydat['TMIN'])/2


dailydat['TAVG'] = dailydat.apply(
    lambda row: (row['TMAX']+row['TMIN'])/2 if np.isnan(row['TAVG']) else row['TAVG'],
    axis=1)

dailydat['TAVG'] = dailydat.apply(lambda row: row['calcTAVG'] if (row['TAVG'] < row['TMIN']) |
                                                                  (row['TAVG'] > row['TMAX'])
                                                                  else row['TAVG'], axis = 1)

dailydat.to_csv("Data\DailyData.csv", index = False)
