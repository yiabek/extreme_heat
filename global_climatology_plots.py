#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 11 12:28:08 2022

@author: yiannabekris
"""

import numpy as np
from netCDF4 import Dataset
import xarray as xr
import matplotlib.pyplot as plt
import cartopy
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import geopandas as gpd
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from mpl_toolkits.axes_grid1 import make_axes_locatable
from mpl_toolkits.basemap import shiftgrid
import matplotlib.colors as colors
import matplotlib.patheffects as pe
from scipy import signal
import glob


### ========== MidPointNormalize ========== ###
### Used to choose midpoint on colorbar in Moments_Plot
class MidpointNormalize(colors.Normalize):
    def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
        self.midpoint = midpoint
        colors.Normalize.__init__(self, vmin, vmax, clip)

    def __call__(self, value, clip=None):
        # I'm ignoring masked values and all kinds of edge cases to make a
        # simple example...
        x, y = [self.vmin, self.midpoint, self.vmax], [0, 0.5, 1]
        return np.ma.masked_array(np.interp(value, x, y))


### ========== Plotting ========== ###

## Figure filename and resolution setting
metric = 'mean'
resolution = 300
fig_filename = f'/Users/yiannabekris/Documents/record_smashers/figures/ERA5_{metric}_trends_1940_2022.jpg'


## File name
netcdf_filename = "/Users/yiannabekris/Documents/record_smashers/netcdf/T_daily_max_ERA5_historical_an-sfc_19400101_20221231_std_anom15.nc"
# lon = np.asarray(var_netcdf.variables['longitude'][:], dtype="float")
# lat = np.asarray(var_netcdf.variables['latitude'][:], dtype="float")


## Data
## Open nc file with standardized anomalies calculated
# std_anoms = xr.open_dataset(file_name, chunks={"time": 3000})
std_anoms = xr.open_dataset(netcdf_filename)

## Seasonal datasets

if metric=='max':
    std_seasons = std_anoms.t2m.resample(time="QS-DEC").max()
elif metric=='mean':
    std_seasons = std_anoms.t2m.resample(time="QS-DEC").mean()
elif metric=='min':
    std_seasons = std_anoms.t2m.resample(time="QS-DEC").min()    

## Create season datasets
seasons_jja = std_seasons.sel(time=std_seasons['time.season']=='JJA')
seasons_son = std_seasons.sel(time=std_seasons['time.season']=='SON')
seasons_djf = std_seasons.sel(time=std_seasons['time.season']=='DJF')
seasons_mam = std_seasons.sel(time=std_seasons['time.season']=='MAM')

seasons_djf = seasons_djf.sel(time=slice("1940-03-01", "2022-11-30"))



## Colormap list for plotting
# cmaps = ['seismic','seismic','seismic','seismic']

cmaps = ['coolwarm','coolwarm','coolwarm','coolwarm']

# cmaps = ['BrBG','BrBG','BrBG','BrBG']



## Title list for plotting
if metric=='max':
    title = 'Maximum Temperature Anomaly Trends 1940-2022'
elif metric=='mean':
    title = 'Mean Temperature Anomaly Trends 1940-2022'
elif metric=='min':
    title = 'Minimum Temperature Anomaly Trends 1940-2022'  


## Date list for plotting
dates = ['JJA','SON','DJF','MAM']


jja_trends = seasons_jja.polyfit(dim='time', deg=1)
son_trends = seasons_son.polyfit(dim='time', deg=1)
djf_trends = seasons_djf.polyfit(dim='time', deg=1)
mam_trends = seasons_mam.polyfit(dim='time', deg=1)

## For converting from nanoseconds
nanoseconds_in_a_year = 365 * 24 * 60 * 60 * 10**9 

degree_1_jja = jja_trends.polyfit_coefficients.isel(degree=0)
degree_1_son = son_trends.polyfit_coefficients.isel(degree=0)
degree_1_djf = djf_trends.polyfit_coefficients.isel(degree=0)
degree_1_mam = mam_trends.polyfit_coefficients.isel(degree=0)

degree_1_jja = degree_1_jja * nanoseconds_in_a_year * 83
degree_1_son = degree_1_son * nanoseconds_in_a_year * 83
degree_1_djf = degree_1_djf * nanoseconds_in_a_year * 83
degree_1_mam = degree_1_mam * nanoseconds_in_a_year * 83

data_list = [degree_1_jja, degree_1_son, degree_1_djf, degree_1_mam]



## Set figure size
fig = plt.figure(figsize=(20, 13))

## Set figure resolution
# fig.set_dpi(resolution)

### ========== Loop through numpy arrays and map ========== ###
for ind in np.arange(0,len(data_list)):
    print(f'plotting subplot {ind}')
    # ax=fig.add_subplot(int('33'+str(ind+1)),projection=cartopy.crs.PlateCarree())
    crs = ccrs.Robinson()
    subplot = ind + 1
    ax=fig.add_subplot(2, 2, subplot, projection=crs)
    


    ### First deal with latitude and longitude
    ### Read in longitude directly from model and use shiftdata function to avoid wrapping while plotting
    var_netcdf = Dataset(netcdf_filename, "r")
    lon = np.asarray(var_netcdf.variables['longitude'][:], dtype="float")
    lat = np.asarray(var_netcdf.variables['latitude'][:], dtype="float")


    ### align latitudes with land borders
    # lat = lat - np.true_divide((lat[2] - lat[1]), 2)

    ## Fix longitudes so values range from -180 to 180 
    if lon[lon>180].size>0:
        lon[lon>180]=lon[lon>180]-360

    if lon[lon > 180].size > 0:  # 0 to 360 grid
        data_plt, lon = shiftgrid(180., data_list[ind], lon, start=False)
    else:
        data_plt = data_list[ind]
        
    # Convert to C
    # data_plt = data_plt - 273.15


    ## Plotting
    levels = np.arange(-3.0, 3.5, 0.5)
    p1=ax.contourf(lon, lat, data_plt, cmap=cmaps[ind],
                        levels=levels,
                        norm=colors.TwoSlopeNorm(0, -3, 3),
                        extend='both',
                        linewidth=0, rasterized=True, transform=ccrs.PlateCarree())
        

      
    ## Draw features
    ax.add_feature(cartopy.feature.BORDERS, edgecolor='black', linewidth=3)
    ax.add_feature(cartopy.feature.COASTLINE, zorder=1)
    ax.add_feature(cartopy.feature.LAKES, zorder=1, linewidth=1, edgecolor='k', facecolor='none')    
    ax.text(0.02, 0.92, dates[ind], fontsize=30, transform=ax.transAxes, weight='bold', color='black')
    ## Create individual colorbars per subplot
    # axpos = ax.get_position()
    # axpos0 = axpos.x0
    # pos_x = axpos0 + axpos.width - 0.06
    # cax = inset_axes(ax, width="60%", height="14%", loc='lower center', bbox_to_anchor=(0, -0.05, 1, 0.3),
    #                  bbox_transform=ax.transAxes, borderpad=-0.25)
    # cbar = fig.colorbar(p1, cax=cax, orientation='horizontal', extend='both')
    # cbar.ax.tick_params(labelsize=14)
    

cbar_ax = fig.add_axes([0.17, -0.02, 0.66, 0.03])
cbar = fig.colorbar(p1, cax=cbar_ax, orientation='horizontal', extend='both')
cbar.ax.tick_params(labelsize=20)

## Titles
fig.suptitle(title, fontsize=30)

## Tight layout for saving
fig.tight_layout(pad=0.7)

## Save figure
plt.savefig(fig_filename, bbox_inches='tight', pad_inches=0.4)



