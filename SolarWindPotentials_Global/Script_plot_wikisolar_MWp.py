#!/usr/bin/env python
import numpy as np
import pandas as pd
import math
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from mpl_toolkits.axes_grid1.inset_locator import inset_axes


wiki    = '/vol/milkunB/jbosmans/WikiSolar/200929_RUN365_B99--.xlsx'
wiki_in = pd.read_excel(wiki)

print(list(wiki_in))

print('Remove items without lat,lon and floating')

wiki_nf = wiki_in[wiki_in['Pt_TcArr'] != 'Floating']
wiki_pv = wiki_nf.dropna(subset=['Lat,Long'])

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

    LatLonStr = str(wiki_pv['Lat,Long'].loc[i])
    if ',' in LatLonStr:
        try:
            wiki_pv.at[i,'lat_wiki'] = float(LatLonStr.split(',')[0])
            wiki_pv.at[i,'lon_wiki'] = float(LatLonStr.split(',')[1])
        except:
            print('Other error in LatLonStr',  i, LatLonStr)
            LatLonFix = LatLonStr.replace('(','')	# found 1 entry with a bunch of (((
            print('Fixed?:                 ',  i, LatLonFix)
            wiki_pv.at[i,'lat_wiki'] = float(LatLonFix.split(',')[0])
            wiki_pv.at[i,'lon_wiki'] = float(LatLonFix.split(',')[1]) 
    else:
        try:
            print('No comma in LatLonStr:', i, LatLonStr)
            wiki_pv.at[i,'lat_wiki'] = float(LatLonStr.split(' ')[0])
            wiki_pv.at[i,'lon_wiki'] = float(LatLonStr.split(' ')[1])
        except:
            print('Other error in LatLonStr',  i, LatLonStr)

wiki_current = wiki_pv[wiki_pv['Status'] != 'C']
wiki_future  = wiki_pv[wiki_pv['Status'] == 'C']

plt.rcParams.update({'font.size': 14})
fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(14, 12))
fig.suptitle('Input from the Wiki-Solar database for utility-scale PV')

# Upper subplot for status A and B 
axes[0].set_title("MWp for %4i current facilities, total capacity %3.0f GWp" % (len(wiki_current),wiki_current['MWp_used'].sum()/1000.))
m = Basemap(projection='cyl',llcrnrlat=-70,urcrnrlat=90,\
            llcrnrlon=-180,urcrnrlon=180,resolution='c',area_thresh=1000.,ax=axes[0])
m.drawcoastlines()

x,y   = m(wiki_current['lon_wiki'], wiki_current['lat_wiki'])
data  = m.scatter(x,y, c=wiki_current['MWp_used'],s=15.0,edgecolors='none',cmap='rainbow',vmin=3,vmax=100)
#m.colorbar(data,extend='max')

# inset histogram MWp
axins1 = inset_axes(axes[0], width="20.5%", height="30%", loc=3,borderpad=3.1)
wiki_current['MWp_clipped'] = np.where(wiki_current['MWp_used']>100.0, 100,wiki_current['MWp_used'])
wiki_current['MWp_clipped'].plot.hist(bins=40,ax=axins1)
plt.sca(axins1)
plt.xlim(0,105)
plt.ylabel('')
#plt.ylim(0,1600)
plt.yticks(np.arange(0,2001,1000))
plt.xticks(np.arange(0,120,20), np.append(np.arange(0,100,20),'100+'),rotation=45,ha='center')
plt.axvline(wiki_current['MWp_used'].median(), color='r', linestyle='dashed', linewidth=0.99)

# Upper subplot for status C
axes[1].set_title("MWp for %4i planned facilities, total capacity %3.0f GWp" % (len(wiki_future),wiki_future['MWp_used'].sum()/1000.))
m = Basemap(projection='cyl',llcrnrlat=-70,urcrnrlat=90,\
            llcrnrlon=-180,urcrnrlon=180,resolution='c',area_thresh=1000.,ax=axes[1])
m.drawcoastlines()

x,y   = m(wiki_future['lon_wiki'], wiki_future['lat_wiki'])
data  = m.scatter(x,y, c=wiki_future['MWp_used'],s=15.0,edgecolors='none',cmap='rainbow',vmin=3,vmax=100)
#m.colorbar(data,extend='max')

# inset histogram MW0
axins1 = inset_axes(axes[1], width="21%", height="30%", loc=3,borderpad=2.8)
wiki_future['MWp_clipped'] = np.where(wiki_future['MWp_used']>100.0, 100,wiki_future['MWp_used'])
wiki_future['MWp_clipped'].plot.hist(bins=40,ax=axins1)
plt.sca(axins1)
plt.xlim(0,105)
plt.ylabel('')
plt.yticks(np.arange(0,501,250))
plt.xticks(np.arange(0,120,20), np.append(np.arange(0,100,20),'100+'),rotation=45,ha='center')
plt.axvline(wiki_future['MWp_used'].median(), color='r', linestyle='dashed', linewidth=0.99)

# colorbar
fig.subplots_adjust(right=0.85)
cbar_ax = fig.add_axes([0.86, 0.1, 0.03, 0.8]) #x_0, y_0, width, height
fig.colorbar(data, cax=cbar_ax,extend='max')


plt.savefig('Wiki-solar-capacity-MWp-all.pdf')
plt.show()
plt.close()


