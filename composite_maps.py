#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct  8 13:19:26 2023

@author: yiannabekris
"""

## Import packages
import numpy as np
import pandas as pd
import xarray as xr


from composite_map_plotting import map_composites
from composite_map_plotting import map_seas_cycle
from composite_map_plotting import map_seas_objects
from composite_map_plotting import map_year_objects


## Load file with characteristics
char_filename = "/Users/yiannabekris/Documents/record_smashers/csv/processed_char_3sig_100kpeak_19400301_20221130_wide.csv"
# char_filename = "/Users/yiannabekris/Documents/record_smashers/csv/processed_char_3sig_100kpeak_19400301_20221130_ct_enso.csv"
# char_filename = "/Users/yiannabekris/Documents/record_smashers/csv/processed_char_3sig_100kpeak_19400301_20221130_nwp_enso.csv"
char_df = pd.read_csv(char_filename)

## Load SST file and open
sst_filename = "/Users/yiannabekris/Documents/record_smashers/netcdf/sst.mnmean.nc"
sst_ds = xr.open_dataset(sst_filename)
sst_ds = sst_ds.sel(time=((sst_ds.time.dt.year > 1939) & (sst_ds.time.dt.year < 2023)))

## Climatology period
clim_min_year = 1951
clim_max_year = 2020

## Trim to climatological period
sst_4clim = sst_ds.sel(time=((sst_ds.time.dt.year >= clim_min_year) & (sst_ds.time.dt.year <= clim_max_year)))
sst_4clim_gb = sst_4clim.groupby("time.month").mean("time", skipna=True)
sst_ds_gb = sst_ds.groupby("time.month")

sst_anom_calc = sst_ds_gb - sst_4clim_gb.sst

sst_anom = sst_anom_calc.resample(time='QS-DEC').mean()
# sst_anom = sst_anom.sel(time=sst_anom['time.season']=='DJF')

## Load ENSO data
nino34_filename = "/Users/yiannabekris/Documents/record_smashers/csv/processed_char_3sig_100kpeak_19400301_20221130_wide.csv"
nino34_df = pd.read_csv(nino34_filename)



### Seasonal Plots
## Find all positive, negative, and neutral years
JJA1P = nino34_df[(nino34_df['Seasonal_Cycle'] == 'JJA0') & (nino34_df['Cycle'] == 'Positive')]
SON1P = nino34_df[(nino34_df['Seasonal_Cycle'] == 'SON') & (nino34_df['Cycle'] == 'Positive')]
DJF1P = nino34_df[(nino34_df['Seasonal_Cycle'] == 'DJF') & (nino34_df['Cycle'] == 'Positive')]
MAM1P = nino34_df[(nino34_df['Seasonal_Cycle'] == 'MAM') & (nino34_df['Cycle'] == 'Positive')]
JJA2P = nino34_df[(nino34_df['Seasonal_Cycle'] == 'JJA1') & (nino34_df['Cycle'] == 'Positive')]


JJA1N = nino34_df[(nino34_df['Seasonal_Cycle'] == 'JJA0') & (nino34_df['Cycle'] == 'Negative')]
SON1N = nino34_df[(nino34_df['Seasonal_Cycle'] == 'SON') & (nino34_df['Cycle'] == 'Negative')]
DJF1N = nino34_df[(nino34_df['Seasonal_Cycle'] == 'DJF') & (nino34_df['Cycle'] == 'Negative')]
MAM1N = nino34_df[(nino34_df['Seasonal_Cycle'] == 'MAM') & (nino34_df['Cycle'] == 'Negative')]
JJA2N = nino34_df[(nino34_df['Seasonal_Cycle'] == 'JJA1') & (nino34_df['Cycle'] == 'Negative')]

JJA1 = nino34_df[(nino34_df['Seasonal_Cycle'] == 'JJA0') & (nino34_df['Cycle'] == 'Neutral')]
SON1 = nino34_df[(nino34_df['Seasonal_Cycle'] == 'SON') & (nino34_df['Cycle'] == 'Neutral')]
DJF1 = nino34_df[(nino34_df['Seasonal_Cycle'] == 'DJF') & (nino34_df['Cycle'] == 'Neutral')]
MAM1 = nino34_df[(nino34_df['Seasonal_Cycle'] == 'MAM') & (nino34_df['Cycle'] == 'Neutral')]
JJA2 = nino34_df[(nino34_df['Seasonal_Cycle'] == 'JJA1') & (nino34_df['Cycle'] == 'Neutral')]


## Subset each SST dataset by years
sst_JJA1P = sst_anom.sel(time=sst_anom['time.year'].isin(JJA1P['Year'].values) & (sst_anom['time.season'] == "JJA"))
sst_SON1P = sst_anom.sel(time=sst_anom['time.year'].isin(SON1P['Year'].values) & (sst_anom['time.season'] == "SON"))
sst_DJF1P = sst_anom.sel(time=sst_anom['time.year'].isin(DJF1P['Year'].values -1) & (sst_anom['time.season'] == "DJF"))
sst_MAM1P = sst_anom.sel(time=sst_anom['time.year'].isin(MAM1P['Year'].values) & (sst_anom['time.season'] == "MAM"))
sst_JJA2P = sst_anom.sel(time=sst_anom['time.year'].isin(JJA2P['Year'].values) & (sst_anom['time.season'] == "JJA"))

sst_JJA1N = sst_anom.sel(time=sst_anom['time.year'].isin(JJA1N['Year'].values) & (sst_anom['time.season'] == "JJA"))
sst_SON1N = sst_anom.sel(time=sst_anom['time.year'].isin(SON1N['Year'].values) & (sst_anom['time.season'] == "SON"))
sst_DJF1N = sst_anom.sel(time=sst_anom['time.year'].isin(DJF1N['Year'].values -1) & (sst_anom['time.season'] == "DJF"))
sst_MAM1N = sst_anom.sel(time=sst_anom['time.year'].isin(MAM1N['Year'].values) & (sst_anom['time.season'] == "MAM"))
sst_JJA2N = sst_anom.sel(time=sst_anom['time.year'].isin(JJA2N['Year'].values) & (sst_anom['time.season'] == "JJA"))

sst_JJA1 = sst_anom.sel(time=sst_anom['time.year'].isin(JJA1['Year'].values) & (sst_anom['time.season'] == "JJA"))
sst_SON1 = sst_anom.sel(time=sst_anom['time.year'].isin(SON1['Year'].values) & (sst_anom['time.season'] == "SON"))
sst_DJF1 = sst_anom.sel(time=sst_anom['time.year'].isin(DJF1['Year'].values -1) & (sst_anom['time.season'] == "DJF"))
sst_MAM1 = sst_anom.sel(time=sst_anom['time.year'].isin(MAM1P['Year'].values) & (sst_anom['time.season'] == "MAM"))
sst_JJA2 = sst_anom.sel(time=sst_anom['time.year'].isin(JJA2['Year'].values) & (sst_anom['time.season'] == "JJA"))

sst_JJA1P_array = np.array(sst_JJA1P.sst.mean(dim='time'))
sst_SON1P_array = np.array(sst_SON1P.sst.mean(dim='time'))
sst_DJF1P_array = np.array(sst_DJF1P.sst.mean(dim='time'))
sst_MAM1P_array = np.array(sst_MAM1P.sst.mean(dim='time'))
sst_JJA2P_array = np.array(sst_JJA2P.sst.mean(dim='time'))

sst_JJA1N_array = np.array(sst_JJA1N.sst.mean(dim='time'))
sst_SON1N_array = np.array(sst_SON1N.sst.mean(dim='time'))
sst_DJF1N_array = np.array(sst_DJF1N.sst.mean(dim='time'))
sst_MAM1N_array = np.array(sst_MAM1N.sst.mean(dim='time'))
sst_JJA2N_array = np.array(sst_JJA2N.sst.mean(dim='time'))

sst_JJA1_array = np.array(sst_JJA1.sst.mean(dim='time'))
sst_SON1_array = np.array(sst_SON1.sst.mean(dim='time'))
sst_DJF1_array = np.array(sst_DJF1.sst.mean(dim='time'))
sst_MAM1_array = np.array(sst_MAM1.sst.mean(dim='time'))
sst_JJA2_array = np.array(sst_JJA2.sst.mean(dim='time'))

empty_array = np.zeros(sst_JJA1_array.shape)

empty_array[empty_array==0] = np.nan

labels_seas =['JJA0','JJA0','JJA0',
              'SON','SON','SON',
              'DJF','DJF','DJF',
              'MAM','MAM','MAM',
              'JJA1','JJA1','JJA1'] 

data_seas = [sst_JJA1P_array, sst_JJA1N_array, sst_JJA1_array,
             sst_SON1P_array, sst_SON1N_array, sst_SON1_array,
             sst_DJF1P_array, sst_DJF1N_array, sst_DJF1_array,
             sst_MAM1P_array, sst_MAM1N_array, sst_MAM1_array,
             sst_JJA2P_array, sst_JJA2N_array, sst_JJA2_array]

cmaps_seas = ['coolwarm','coolwarm','coolwarm',
         'coolwarm','coolwarm','coolwarm',
         'coolwarm','coolwarm','coolwarm',
         'coolwarm','coolwarm','coolwarm',
         'coolwarm','coolwarm','coolwarm']

seas_titles = ['El Ni\u00F1o', 'La Ni\u00F1a', 'Neutral']

seas_filename = '/Users/yiannabekris/Documents/record_smashers/figures/wp_seasonal_composites.png'


map_seas_cycle(data_seas, np.array(sst_ds.lat), np.array(sst_ds.lon), 
               cmaps_seas, seas_titles, labels_seas, seas_filename)

# char_df['Extent'] = char_df['Extent']

# Positive conditions
JJA1P = char_df[(char_df['Seasonal_Cycle'] == 'JJA0') & (char_df['Cycle'] == 'Positive')]
SON1P = char_df[(char_df['Seasonal_Cycle'] == 'SON') & (char_df['Cycle'] == 'Positive')]
DJF1P = char_df[(char_df['Seasonal_Cycle'] == 'DJF') & (char_df['Cycle'] == 'Positive')]
MAM1P = char_df[(char_df['Seasonal_Cycle'] == 'MAM') & (char_df['Cycle'] == 'Positive')]
JJA2P = char_df[(char_df['Seasonal_Cycle'] == 'JJA1') & (char_df['Cycle'] == 'Positive')]

# Negative conditions
JJA1N = char_df[(char_df['Seasonal_Cycle'] == 'JJA0') & (char_df['Cycle'] == 'Negative')]
SON1N = char_df[(char_df['Seasonal_Cycle'] == 'SON') & (char_df['Cycle'] == 'Negative')]
DJF1N = char_df[(char_df['Seasonal_Cycle'] == 'DJF') & (char_df['Cycle'] == 'Negative')]
MAM1N = char_df[(char_df['Seasonal_Cycle'] == 'MAM') & (char_df['Cycle'] == 'Negative')]
JJA2N = char_df[(char_df['Seasonal_Cycle'] == 'JJA1') & (char_df['Cycle'] == 'Negative')]

# Neutral conditions
JJA1 = char_df[(char_df['Seasonal_Cycle'] == 'JJA0') & (char_df['Cycle'] == 'Neutral')]
SON1 = char_df[(char_df['Seasonal_Cycle'] == 'SON') & (char_df['Cycle'] == 'Neutral')]
DJF1 = char_df[(char_df['Seasonal_Cycle'] == 'DJF') & (char_df['Cycle'] == 'Neutral')]
MAM1 = char_df[(char_df['Seasonal_Cycle'] == 'MAM') & (char_df['Cycle'] == 'Neutral')]
JJA2 = char_df[(char_df['Seasonal_Cycle'] == 'JJA1') & (char_df['Cycle'] == 'Neutral')]


data_char = [JJA1P, JJA1N, JJA1,
             SON1P, SON1N, SON1,
             DJF1P, DJF1N, DJF1,
             MAM1P, MAM1N, MAM1,
             JJA2P, JJA2N, JJA2]

obj_filename = '/Users/yiannabekris/Documents/record_smashers/figures/wp_seasonal_objects.png'


map_seas_objects(data_char, empty_array, np.array(sst_ds.lat), np.array(sst_ds.lon),
                 seas_titles, labels_seas, obj_filename)

year_filename = '/Users/yiannabekris/Documents/record_smashers/figures/wp_year_seasonal_objects.png'

map_year_objects(data_char, empty_array, np.array(sst_ds.lat), np.array(sst_ds.lon),
                 seas_titles, labels_seas, year_filename)

## Find all positive, negative, and neutral years
elnino = nino34_df[nino34_df['Phase'] == 'Positive']
lanina = nino34_df[nino34_df['Phase'] == 'Negative']
neutral = nino34_df[nino34_df['Phase'] == 'Neutral']

## Extract years
elnino_years = elnino.Year.values.tolist()
lanina_years = lanina.Year.values.tolist()
neutral_years = neutral.Year.values.tolist()

## Subset each SST dataset by years
sst_elnino = sst_anom.sel(time=sst_anom['time.year'].isin(elnino_years))
sst_lanina = sst_anom.sel(time=sst_anom['time.year'].isin(lanina_years))
sst_neutral = sst_anom.sel(time=sst_anom['time.year'].isin(neutral_years))

sst_nino_array = np.array(sst_elnino.sst.mean(dim='time'))
sst_nina_array = np.array(sst_lanina.sst.mean(dim='time'))
sst_neutral_array = np.array(sst_neutral.sst.mean(dim='time'))

empty_array = np.zeros(sst_nino_array.shape)

empty_array[empty_array==0] = np.nan

data_list = [sst_nino_array,
             sst_nina_array,
             sst_neutral_array,
             empty_array,
             empty_array,
             empty_array]

cmaps = ['coolwarm','coolwarm','coolwarm','coolwarm','coolwarm','coolwarm']

fig_filename = '/Users/yiannabekris/Documents/record_smashers/figures/wp_enso_composites.png'
titles = ['El Nino', 'La Nina', 'Neutral','','','']

labels =['','','','','',''] 


map_composites(data_list, char_df, np.array(sst_ds.lat), np.array(sst_ds.lon), cmaps, titles, labels, fig_filename)