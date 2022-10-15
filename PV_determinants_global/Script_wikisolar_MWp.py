#!/usr/bin/env python
import numpy as np
import pandas as pd
import math
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from mpl_toolkits.axes_grid1.inset_locator import inset_axes


wiki    = '/vol/milkunB/jbosmans/WikiSolar/200929_RUN365_B99--.xlsx'
wiki_pv = pd.read_excel(wiki)

print(list(wiki_pv))

PR = 0.80
for i, PV in wiki_pv.iterrows():
    if math.isnan(wiki_pv['MWac'].loc[i]):
        wiki_pv.at[i,'MWac_used'] = wiki_pv['MWp'].loc[i] * PR
    else:
        wiki_pv.at[i,'MWac_used'] = wiki_pv['MWac'].loc[i]
    if math.isnan(wiki_pv['MWp'].loc[i]):
        wiki_pv.at[i,'MWp_used'] = wiki_pv['MWac'].loc[i] / PR
    else:
        wiki_pv.at[i,'MWp_used'] = wiki_pv['MWp'].loc[i]

    #LatLonStr = str(wiki_pv['Lat,Long'].loc[i])
    #wiki_pv.at[i,'lat_wiki'] = float(LatLonStr.split(',')[0])
    #wiki_pv.at[i,'lon_wiki'] = float(LatLonStr.split(',')[1])

wiki_current = wiki_pv[wiki_pv['Status'] != 'C']
wiki_future  = wiki_pv[wiki_pv['Status'] == 'C']

wiki_current = wiki_current[wiki_current['Pt_TcArr'] != 'Floating']
wiki_current = wiki_current[wiki_current['Lat,Long'].notnull()]

wiki_current['MWp_used'].min()
wiki_current['MWp_used'].max()
wiki_current['MWp_used'].median()

# min value is 0. Min value of MWac is 0.7 -> 0.9 MWp. 

len(wiki_current[wiki_current['MWp_used'] < 1])	# 5 below 1 MWp (out of 10306)
len(wiki_current[wiki_current['MWp_used'] < 3]) # 71 below 3 MWp
