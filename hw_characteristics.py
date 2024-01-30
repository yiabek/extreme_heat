#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""


@author: yiannabekris
"""

## Import necessary modules
import numpy as np
import pandas as pd
import xarray as xr
import glob
import dask
from scipy import ndimage
import time
import os
import sys
my_var = int(sys.argv[2]) # argv[2] because you need the 2nd arg from the job array script
print(my_var)

## Import custom functions
from label_yearly_blobs import area_grid

## First define years
year_list = [*range(1939, 2023)]

year = int(year_list[my_var])

## Start and end dates
start_date = f"{year-1}-12-01"
end_date = f"{year}-11-30"

## Folder and file paths
folder_path = "/data/singh/yianna/nparrays/"
array_filename = f"T_daily_max_ERA5_historical_an-sfc_{start_date}_{end_date}_3sig_50k.npy"
label_filename = f"T_daily_max_ERA5_historical_an-sfc_{start_date}_{end_date}_3sig_labs_50k.npy"

## Anomaly dataset filename
std_anoms_filename = "/data/singh/yianna/ERA5temp/T_daily_max_ERA5_historical_an-sfc_19400101_20221231_std_anom15_float32.nc"

# files = glob.glob(sorted(lab_filenames))

## Variable name
var_name = "t2m"

## Set threshold, should be the same as NumPy files
threshold = 3

## Open land sea mask
lsm_filename = "/data/singh/data/ERA5/land_sea_mask/land_sea_mask_ERA5.nc" 
lsm_mask = xr.open_dataset(lsm_filename)

## Label array and get label names
labeled_array = np.load(folder_path + array_filename, allow_pickle=True)
labels =  np.load(folder_path + label_filename, allow_pickle=True)

## Open nc file with standardized anomalies calculated
std_anoms = xr.open_dataset(std_anoms_filename)

## Filter the dataset by date
std_filt = std_anoms.sel(time=slice(start_date, end_date))

## Declare write path
csv_file = '/data/singh/yianna/csv/characteristics_3sig_50k_19400101_20221231.csv'

# Check if the CSV file already exists or not
if not os.path.isfile(csv_file):
    # Create the initial CSV file with header
    with open(csv_file, "w") as f:
        f.write('Date','Label','Total Extent','Land Extent',
                'Event Land Fraction','Cumulative Intensity',
                'Weighted Mean Intensity', 'Peak Intensity',
                'Centroid Latitude','Centroid Longitude','Duration')

## Land-sea mask
lsm_mask_numpy = lsm_mask.lsm.to_numpy()

land = lsm_mask_numpy == 1
sea = lsm_mask_numpy == 0

# Land blobs
land_blobs = labeled_array * lsm_mask_numpy

## Find area of each grid cell for later
xda, total_area, area_grid_km = area_grid(lsm_mask.latitude, lsm_mask.longitude) 
    
## Add new axis for broadcasting
area_grid_km = area_grid_km[np.newaxis,:,:]

## Find weights
weights = area_grid_km / total_area

## Multiply the data variable by the area
#std_weighted_anoms = std_filt.t2m * area_grid_km

## Arrays of latidude and longitude
lat_array = std_anoms.latitude.to_numpy()
lon_array = std_anoms.longitude.to_numpy()

## Convert longitude from 0 to 360 to -180 to 180
lon_array[lon_array>180]=lon_array[lon_array>180]-360

## Dates for CSV
date_range = xr.cftime_range(start_date, end_date, freq='D', calendar = 'noleap')

## Create lists for pandas dataframe for CSV
ci_list = []
extent_list = []
land_area_list = []
centroids = []
centroid_lats = []
centroid_lons = []
dates = []
labels = []
land_fraction = []
intensity = []
peak_intensity = []

## Loop through each time and latitude index to find where labels do not overlap
for time_ind in range(labeled_array.shape[0]):

    ## Binary mask of threshold
    blobs_2d = std_filt.t2m[[time_ind],:,:] > threshold
    
    ## Find unique labels
    unique_labels_nan = np.unique(labeled_array[[time_ind],:,:])
    unique_labels = [lab for lab in unique_labels_nan if str(lab) != 'nan']
    
    day_labs_nan = np.unique(labeled_array[[time_ind],:,:])
    day_labs = [lab for lab in day_labs_nan if str(lab) != 'nan']

    ## Masked anoms for SciPy NDimage functions
    std_anoms_masked = std_filt.t2m[[time_ind],:,:]
        
    ## Copy of area grid array for masking
    area_grid1_masked = area_grid_km.copy()

    ## Mask area grid array with land sea mask to get land grid cells
    weighted_land = area_grid1_masked * lsm_mask_numpy
    
    ## Multiply anomalies by area of grid cell for cumulative intensity
    ci_masked = area_grid1_masked * std_anoms_masked
    
    ## Weight anomalies by area
    weighted_anoms = weights * std_anoms_masked

    
    ## Sum of all the anomalies in each connected region
    sum_blobs = ndimage.sum_labels(std_anoms_masked, labels=labeled_array[[time_ind],:,:],
                                    index=[i for i in unique_labels]) 
    
    ## Cumulative intensity of each connected region
    ci_blobs = ndimage.sum_labels(ci_masked, labels=labeled_array[[time_ind],:,:],
                                    index=[i for i in unique_labels]) 
    
    ## Total area in kilometers of each connected region
    total_area_blobs = ndimage.sum_labels(area_grid1_masked, labels=labeled_array[[time_ind],:,:],
                                    index=[i for i in unique_labels]) 

    ## Land area of each connected region
    land_area_blobs = ndimage.sum_labels(weighted_land, labels=labeled_array[[time_ind],:,:],
                                    index=[i for i in unique_labels])       
    
    ## Weights for calculating the weighted mean of intensity
    weights_image = ndimage.sum_labels(weights, labels=labeled_array[[time_ind],:,:],
                                      index=[i for i in unique_labels])
    
    ## Sum of the weighted intensity
    sum_weighted_intensity = ndimage.sum_labels(weighted_anoms, labels=labeled_array[[time_ind],:,:],
                                      index=[i for i in unique_labels])
    
    ## Calculate weighted intensity
    weighted_intensity = sum_weighted_intensity / weights_image
    
    ## Append to arrays and lists to append to CSV later
    intensity.extend(weighted_intensity)
    labels.extend(unique_labels)
    ci_list.extend(ci_blobs)
    extent_list.extend(total_area_blobs)
    land_area_list.extend(land_area_blobs)
    
    ## Convert weighted_anoms Xarray array to NumPy array for ease
    weighted_anoms = weighted_anoms.to_numpy()


    ## Create an array of the same shape as the original array with sum values at labeled regions
    sum_array = np.zeros_like(labeled_array[[time_ind],:,:], dtype=np.float64)
    area_array = np.zeros_like(labeled_array[[time_ind],:,:])
    land_area_array = np.zeros_like(labeled_array[[time_ind],:,:])
    std_anoms_np = std_anoms_masked.to_numpy()    
 
    ## Loop through each label
    for i, lab in enumerate(unique_labels):
    
        ## Dates of events
        date = date_range[time_ind]
        lab_on_land = land[labeled_array[[time_ind],:,:] == lab]
        peak = np.nanmax(std_anoms_np[labeled_array[[time_ind],:,:] == lab])
        peak_intensity.append(peak)

        ## Append date to date list
        dates.append(date)
        
        ## Find labels which overlap meridian 0 and calculate their centroid appropriately  
        if (np.any(labeled_array[time_ind, :, [0]] == lab))\
            and (np.any(labeled_array[time_ind, :, [-1]] == lab)):
            
            ## Remove extra dimension of 1 from weighted anomaly array
            squeezed_anoms = np.squeeze(weighted_anoms)
            
            ## Find x and y indices of the connected region
            inds_x, inds_y = np.where(labeled_array[time_ind, :, :] == lab)
                            
            ## Adjust y indices to wrap correctly
            inds_y[inds_y>720]=inds_y[inds_y>720]-1440
            
            ## Subset weighted anomalies to the x and y indices of the grid cells
            ## and in the appropriate shape for calculating the centroid
            weighted_grids = squeezed_anoms[inds_x, inds_y]

            ## Find the total of the anomalies across the connection region                                
            total_anoms = np.nansum(weighted_grids)

            ## Calculate the centroid which returns the indices of the array
            centroid = round(np.nansum(weighted_grids * inds_x) / total_anoms),\
                             round(np.nansum(weighted_grids * inds_y) / total_anoms)
                             
            ## Find the latitude and longitude of the centroid by using the centroid indices                
            centroid_lat = lat_array[centroid[0]]
            centroid_lon = lon_array[centroid[1]]

            ## Append the latitude, longitude, and indices of the centroid to the centroid lists
            centroid_lats.append(float(centroid_lat))
            centroid_lons.append(float(centroid_lon))
            centroids.append(centroid)
                
        ## If the centroid does not overlap the 0 meridian,
        ## calculate without making adjustments to the y axis
        else:

            ## Remove extra dimension of 1 from weighted anomaly array
            squeezed_anoms = np.squeeze(weighted_anoms)
            
            ## Find x and y indices of the connected region
            inds_x, inds_y = np.where(labeled_array[time_ind, :, :] == lab)
                                
            ## Subset weighted anomalies to the x and y indices of the grid cells
            ## and in the appropriate shape for calculating the centroid
            weighted_grids = squeezed_anoms[inds_x, inds_y]

            ## Find the total of the anomalies across the connection region  
            total_anoms = np.nansum(weighted_grids)

            ## Calculate the centroid which returns the indices of the array
            centroid = round(np.nansum(weighted_grids * inds_x) / total_anoms),\
                             round(np.nansum(weighted_grids * inds_y) / total_anoms)
                                   
            
            ## Find the latitude and longitude of the centroid by using the centroid indices
            centroid_lat = lat_array[centroid[0]]
            centroid_lon = lon_array[centroid[1]]
            
            ## Append the latitude, longitude, and indices of the centroid to the centroid lists
            centroid_lats.append(float(centroid_lat))
            centroid_lons.append(float(centroid_lon))
            centroids.append(centroid)

            
## Find the fraction of land area of the connected region
event_land_fraction = np.array(land_area_list) / np.array(extent_list)

## Find the duration of the event
## This is across time indices so cannot be int he above loop
duration_list = []
duration_labs = []

## Loop through each label and t
for lab in np.unique(labeled_array):
    
    ## Find how many times each label occurs on the 
    ## time axis to find the duration of each event
    duration = len(np.unique(np.where(labeled_array == lab)[0]))
    duration_list.append(duration)
    duration_labs.append(lab)
     
## Create a duration dataframe to merge with the other characteristics   
duration_df = pd.DataFrame({'Label' : duration_labs, 'Duration' : duration_list})
    
## Create a dataframe of all the dates, labels, and characteristics
object_df = pd.DataFrame({'Date' : dates, 'Label' : labels,
                            'Total Extent': extent_list, 'Land Extent' : land_area_list,
                            'Event Land Fraction' : event_land_fraction,
                            'Cumulative Intensity': ci_list,
                            'Weighted Mean Intensity': intensity,
                            'Peak Intensity': peak_intensity,
                            'Centroid Latitude' : centroid_lats,
                            'Centroid Longitude' : centroid_lons})
    
## Merge the duration and characteristics dataframe together
characteristics_df = object_df.merge(duration_df, how='inner', on='Label')


## Append the characteristics dataframe to the existing CSV file
characteristics_df.to_csv(csv_file, mode="a", header=False, index=False)
