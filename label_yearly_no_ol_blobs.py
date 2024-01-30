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
from copy import copy, deepcopy
import sys
my_var = int(sys.argv[2]) # argv[2] because you need the 2nd arg from the job array script

from geo_idx import area_grid

""" Label array, wrap labels around longitude,
### find where centroids do not overlap with previous day,
### and then change labels
"""
def wrap_labels_3d(array3d, thresh, area_thresh):
    
    ## Binary array to identify where anomalies meet condition
    blobs = array3d >= thresh
    
    ## Intial labeling of array and finding number of labels
    labeled_array, num_labels1 = ndimage.label(blobs)
    labeled_array = labeled_array.astype(float) # Convert to float
    labeled_array[blobs == 0] = np.nan

    ## Area threshold
    area_thresh = area_thresh
    
    ## Calculate area of each grid cell in meters 
    xda, total_area, area_grid_km = area_grid(array3d.latitude, array3d.longitude)  
       

    ## Add an axis for broadcasting
    area_grid_km = area_grid_km[np.newaxis,:,:]
    
    ## Intiate empty NumPy array for wrapped labels
    wrapped_labels = np.empty([0, labeled_array.shape[1], labeled_array.shape[2]])
    
    ## Intiate empty list for valid labels and centroids
    labs = []
    
    ## Loop through each time and latitude index to find where labels do not overlap
    for time_ind in range(labeled_array.shape[0]):
        
        for lat_ind in range(labeled_array.shape[1]):    
             
            ## If there are labeled grid cells at the first and last longitude index,
            ## change their labeling to the labeling of the first longitude index
            if (labeled_array[time_ind, lat_ind, 0] != 0)\
              & (labeled_array[time_ind, lat_ind, -1] != 0)\
              & (labeled_array[time_ind, lat_ind, 0] != labeled_array[time_ind, lat_ind, -1]):
   
                new_lab_inds = np.where(labeled_array == labeled_array[time_ind, lat_ind, -1])             
                labeled_array[new_lab_inds] = labeled_array[time_ind, lat_ind, 0]
        
        ## Find the new unique labels        
        new_labs = np.unique(labeled_array[time_ind, :, :])
        new_labs = new_labs[~np.isnan(new_labs)]
        
         
        ## Calculate the area of each blobs 
        area_blobs = ndimage.sum_labels(area_grid_km, labels=labeled_array[time_ind, :, :],
                                        index=[i for i in new_labs]) 
        
       
        ## Delineate valid indices as those over the threshold and not the 
        ## background object
        valid_inds = np.where(area_blobs > area_thresh)[0]
           
        ## Subset labels and  area to valid indices
        area_blobs = [area_blobs[i] for i in valid_inds]
        valid_labs = [new_labs[i] for i in valid_inds]


        ## Find valid labs for that day based on area
        new_wrapped_labs = np.where(np.isin(labeled_array[[time_ind], :, :], valid_labs), 
                                                 labeled_array[[time_ind], :, :], np.nan)

        ## Concatenate to array of all labels
        wrapped_labels = np.concatenate([wrapped_labels, new_wrapped_labs], axis=0)
        
        ## Append lists
        labs.extend(valid_labs)
        
    return wrapped_labels, labs


## Folder and file paths
folder_path = "/data/singh/yianna/ERA5temp/"
file_name = "T_daily_max_ERA5_historical_an-sfc_19400101_20221231_std_anom15_float32.nc"

year_list = [*range(1939, 2023)]

year = int(year_list[my_var])

## Variable namke
var_name = "t2m"

## Open nc file with standardized anomalies calculated
std_anoms = xr.open_dataset(folder_path + file_name)

start_date = f"{year-1}-12-01"
end_date = f"{year}-11-30"

# Filter the dataset by date
std_filt = std_anoms.sel(time=slice(start_date, end_date))

## Set threshold
threshold = 3
area_threshold = 50000 # sq km

## Label array and get label names
labeled_array_new, labels = wrap_labels_3d(std_filt.t2m, thresh=threshold, area_thresh=area_threshold)

labeled_array_int = labeled_array_new.astype(int)
#labels_int = [int(i) for i in labels]

np.save(f"/data/singh/yianna/nparrays/T_daily_max_ERA5_historical_an-sfc_{start_date}_{end_date}_3sig_50k.npy", labeled_array_int)
np.save(f"/data/singh/yianna/nparrays/T_daily_max_ERA5_historical_an-sfc_{start_date}_{end_date}_3sig_labs_50k.npy", labels)
