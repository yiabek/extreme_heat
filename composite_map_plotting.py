#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 17 10:07:50 2023

@author: yiannabekris
"""

## Import packages
import numpy as np
import pandas as pd
from netCDF4 import Dataset
import matplotlib.pyplot as plt
import cartopy
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import geopandas as gpd
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
import matplotlib.patheffects as pe
import matplotlib as mpl
from mpl_toolkits.basemap import Basemap, shiftgrid
import matplotlib.colors as mcolors

## Set font to Arial
mpl.rcParams['font.family'] = 'sans-serif'
mpl.rcParams['font.sans-serif'] = ['Arial']
mpl.rcParams['savefig.dpi']=300

# Define custom intensity levels
custom_levels = [*np.arange(0, 5.5, 0.5)]  # You can adjust these levels as needed

# Create a colormap with the desired levels
# custom_cmap = plt.get_cmap('magma_r', len(custom_levels) - 1)

custom_cmap = plt.get_cmap('magma_r', len(custom_levels))

alpha_val = 0.5
outline_color = 'black'

# Create a normalization object to map values to the colormap
norm = mcolors.BoundaryNorm(custom_levels, custom_cmap.N)


def map_composites(data_list, char_df, lat, lon, cmaps, titles, labels, fig_filename):
    
    ## Set figure size
    fig = plt.figure(figsize=(20, 14))
    
    
    ### ========== Loop through NumPy arrays and map ========== ###
    for i in np.arange(0,len(data_list)):
            
        ## Define projection and boundaries
        crs = ccrs.Robinson()
        
        ## Subplot and axes
        subplot = i + 1
        ax = fig.add_subplot(2, 3, subplot, projection=crs)
        
        # centroid_x, centroid_y = char_df['Centroid Longitude'].values, char_df['Centroid Latitude'].values


                
        ## Data from data list
        data_plt = data_list[i] 
    
    
        ### ------------ Plotting ------------ ###
        levels = np.arange(-0.6, 0.7, .1)
        p1=ax.contourf(lon, lat, data_plt, cmap=cmaps[i],
                            levels=levels, extend="both",
                            linewidth=0, rasterized=True, transform=ccrs.PlateCarree())
        
        if i == 3:
            
            ## Filter to El Nino Years
            elnino = char_df[char_df['Phase'] == 'Positive']
            
            centroid_x, centroid_y = elnino['Centroid Longitude'].values, elnino['Centroid Latitude'].values
        
            # Plot points with varying sizes and colors based on DataFrame columns
            ax.scatter(
                centroid_x,
                centroid_y,
                s=elnino['Extent']/10000,  # Size of points based on 'size_column_i'
                c=elnino['Intensity'],  # Color of points based on 'color_column_i'
                cmap=custom_cmap,
                edgecolors=outline_color,
                alpha=alpha_val, transform=ccrs.PlateCarree()
                )
            
        elif i == 4:
            
            ## Filter to La Nina Years
            lanina = char_df[char_df['Phase'] == 'Negative']
            
            centroid_x, centroid_y = lanina['Centroid Longitude'].values, lanina['Centroid Latitude'].values
            
            # Plot points with varying sizes and colors based on DataFrame columns
            ax.scatter(
                centroid_x,
                centroid_y,
                s=lanina['Extent']/10000,  # Size of points based on 'size_column_i'
                c=lanina['Intensity'],  # Color of points based on 'color_column_i'
                cmap=custom_cmap,
                edgecolors=outline_color,
                alpha=alpha_val, transform=ccrs.PlateCarree()
                )
            
        elif i == 5:
            
            ## Filter to Neutral Years
            neutral = char_df[char_df['Phase'] == 'Neutral']
            
            centroid_x, centroid_y = neutral['Centroid Longitude'].values, neutral['Centroid Latitude'].values
            
            # Plot points with varying sizes and colors based on DataFrame columns
            ax.scatter(
                centroid_x,
                centroid_y,
                s=neutral['Extent']/10000,  # Size of points based on 'size_column_i'
                c=neutral['Intensity'],  # Color of points based on 'color_column_i'
                cmap=custom_cmap,
                edgecolors=outline_color,
                alpha=alpha_val, transform=ccrs.PlateCarree()
                )
            
            
            
            
            
        ## Draw features -- borders, coastlines, lakes, states, and NERC regions
        ax.add_feature(cartopy.feature.BORDERS, edgecolor='black', linewidth=3)
        ax.add_feature(cartopy.feature.COASTLINE, zorder=1)
        ax.add_feature(cartopy.feature.LAKES, zorder=1, linewidth=1, edgecolor='k', facecolor='none')
        # ax.add_feature(states_provinces, edgecolor='black', linewidth=2, 
        #                path_effects=[pe.Stroke(linewidth=3, foreground='w'), pe.Normal()])

        
    
        ## Add the season in the lower left corner
        ax.text(0.02, 0.02, labels[i], fontsize=30, transform=ax.transAxes, weight='bold', color='black')
        
        
        ## If subplot is on top of plot, draw a title
        
        ax.set_title(titles[i], color="black", fontdict={'fontsize': 30, 'fontweight': 'bold'})

        cax = inset_axes(ax, width="60%", height="18%", loc='lower center', bbox_to_anchor=(0, -0.05, 1, 0.3),
                         bbox_transform=ax.transAxes, borderpad=-1)
        cbar = fig.colorbar(p1, cax=cax, orientation='horizontal', extend='both')
        cbar.ax.tick_params(labelsize=17)
    
    
    ## Tight layout for saving
    fig.tight_layout(pad=0.6)
    
    ## Save figure
    plt.savefig(fig_filename, bbox_inches='tight', pad_inches=0.4)
    

def map_seas_cycle(data_list, lat, lon, cmaps, titles, labels, fig_filename):
    
    ## Set figure size
    fig = plt.figure(figsize=(20, 14))
    
    # Define custom intensity levels
    custom_levels = [*np.arange(3, 6.5, 0.5)]  # You can adjust these levels as needed

    # Create a colormap with the desired levels
    custom_cmap = plt.get_cmap('Reds', len(custom_levels) - 1)

    alpha_val = 0.2
    outline_color = 'darkred'

    # Create a normalization object to map values to the colormap
    norm = mcolors.BoundaryNorm(custom_levels, custom_cmap.N)
    
    
    ### ========== Loop through NumPy arrays and map ========== ###
    # for i in np.arange(0,len(data_list)):
    for i, data_plt in enumerate(data_list):
            
        ## Define projection and boundaries
        crs = ccrs.Robinson()
        
        ## Subplot and axes
        subplot = i + 1
        ax = fig.add_subplot(5, 3, subplot, projection=crs)
        
        # centroid_x, centroid_y = char_df['Centroid Longitude'].values, char_df['Centroid Latitude'].values


                
        # ## Data from data list
        # data_plt = data_list[i] 
    
    
        ### ------------ Plotting ------------ ###
        levels = np.arange(-1, 1.1, .1)
        p1=ax.contourf(lon, lat, data_plt, cmap=cmaps[i],
                            levels=levels, extend="both",
                            linewidth=0, rasterized=True, transform=ccrs.PlateCarree())
        
        
        
        
        

            
        ## Draw features -- borders, coastlines, lakes, states, and NERC regions
        ax.add_feature(cartopy.feature.BORDERS, edgecolor='black', linewidth=2)
        ax.add_feature(cartopy.feature.COASTLINE, zorder=1)
        ax.add_feature(cartopy.feature.LAKES, zorder=1, linewidth=1, edgecolor='k', facecolor='none')
        # ax.add_feature(states_provinces, edgecolor='black', linewidth=2, 
        #                path_effects=[pe.Stroke(linewidth=3, foreground='w'), pe.Normal()])

        
    
        ## Add the season in the lower left corner
        ax.text(0, 0, labels[i], fontsize=18, transform=ax.transAxes, weight='bold', color='black')
        
        
        ## If subplot is on top of plot, draw a title
        
        if i < 3:
        
            ax.set_title(titles[i], color="black", fontdict={'fontsize': 30, 'fontweight': 'bold'})

        
        cax = inset_axes(ax, width="60%", height="18%", loc='lower center', bbox_to_anchor=(0, -0.05, 1, 0.3),
                         bbox_transform=ax.transAxes, borderpad=-1)
        cbar = fig.colorbar(p1, cax=cax, orientation='horizontal', extend='both')
        cbar.ax.tick_params(labelsize=14)
    
    
    ## Tight layout for saving
    fig.tight_layout(pad=0.6)
    
    ## Save figure
    plt.savefig(fig_filename, bbox_inches='tight', pad_inches=0.4)
    
def map_seas_objects(data_list, plot_map, lat, lon, titles, labels, fig_filename):
    
    ## Set figure size
    fig = plt.figure(figsize=(20, 14))
    
    
    
    ### ========== Loop through NumPy arrays and map ========== ###
    for i in np.arange(0,len(data_list)):
            
        ## Define projection and boundaries
        crs = ccrs.Robinson()
        
        ## Subplot and axes
        subplot = i + 1
        ax = fig.add_subplot(5, 3, subplot, projection=crs)

                
        ## Data from data list
        data_plt = data_list[i] 
        
        centroid_x, centroid_y = data_plt['Centroid Longitude'].values, data_plt['Centroid Latitude'].values
        print(data_plt)
    
    
        ### ------------ Plotting ------------ ###
        # levels = np.arange(-1, 1.1, .1)
        p1=ax.contourf(lon, lat, plot_map, cmap='coolwarm',
                            # levels=levels, extend="both",
                            linewidth=0, rasterized=True, transform=ccrs.PlateCarree())
        
        # Plot points with varying sizes and colors based on DataFrame columns
        ax.scatter(
            centroid_x,
            centroid_y,
            s=data_plt['Extent']/5000,  # Size of points based on 'size_column_i'
            c=data_plt['Intensity'],  # Color of points based on 'color_column_i'
            cmap=custom_cmap,
            edgecolors=outline_color,
            alpha=alpha_val, transform=ccrs.PlateCarree()
            )
        

            
        ## Draw features -- borders, coastlines, lakes, states, and NERC regions
        ax.add_feature(cartopy.feature.BORDERS, edgecolor='black', linewidth=1)
        ax.add_feature(cartopy.feature.COASTLINE, zorder=1)
        ax.add_feature(cartopy.feature.LAKES, zorder=1, linewidth=1, edgecolor='k', facecolor='none')
        # ax.add_feature(states_provinces, edgecolor='black', linewidth=2, 
        #                path_effects=[pe.Stroke(linewidth=3, foreground='w'), pe.Normal()])

        
    
        ## Add the season in the lower left corner
        ax.text(0, 0, labels[i], fontsize=18, transform=ax.transAxes, weight='bold', color='black')
        
        
        ## If subplot is on top of plot, draw a title
        
        if i < 3:
        
            ax.set_title(titles[i], color="black", fontdict={'fontsize': 30, 'fontweight': 'bold'})

        
        # cax = inset_axes(ax, width="60%", height="18%", loc='lower center', bbox_to_anchor=(0, -0.05, 1, 0.3),
        #                  bbox_transform=ax.transAxes, borderpad=-1)
        # cbar = fig.colorbar(p1, cax=cax, orientation='horizontal', extend='both')
        # cbar.ax.tick_params(labelsize=14)
    
    
    ## Tight layout for saving
    fig.tight_layout(pad=0.6)
    
    ## Save figure
    plt.savefig(fig_filename, bbox_inches='tight', pad_inches=0.4)


def map_year_objects(data_list, plot_map, lat, lon, titles, labels, fig_filename):
    
    ## Set figure size
    fig = plt.figure(figsize=(20, 14))
    
    
    
    ### ========== Loop through NumPy arrays and map ========== ###
    for i in np.arange(0,len(data_list)):
            
        ## Define projection and boundaries
        crs = ccrs.Robinson()
        
        ## Subplot and axes
        subplot = i + 1
        ax = fig.add_subplot(5, 3, subplot, projection=crs)

                
        ## Data from data list
        data_plt = data_list[i] 
        
        centroid_x, centroid_y = data_plt['Centroid Longitude'].values, data_plt['Centroid Latitude'].values
        print(data_plt)
    
    
        ### ------------ Plotting ------------ ###
        # levels = np.arange(-1, 1.1, .1)
        p1=ax.contourf(lon, lat, plot_map, cmap='coolwarm',
                            # levels=levels, extend="both",
                            linewidth=0, rasterized=True, transform=ccrs.PlateCarree())
        
        # Plot points with varying sizes and colors based on DataFrame columns
        ax.scatter(
            centroid_x,
            centroid_y,
            c=data_plt['Year'],  # Color of points based on 'color_column_i'
            cmap=custom_cmap,
            edgecolors=outline_color,
            alpha=alpha_val, transform=ccrs.PlateCarree()
            )
        

            
        ## Draw features -- borders, coastlines, lakes, states, and NERC regions
        ax.add_feature(cartopy.feature.BORDERS, edgecolor='black', linewidth=2)
        ax.add_feature(cartopy.feature.COASTLINE, zorder=1)
        ax.add_feature(cartopy.feature.LAKES, zorder=1, linewidth=1, edgecolor='k', facecolor='none')
        # ax.add_feature(states_provinces, edgecolor='black', linewidth=2, 
        #                path_effects=[pe.Stroke(linewidth=3, foreground='w'), pe.Normal()])

        
    
        ## Add the season in the lower left corner
        ax.text(0, 0, labels[i], fontsize=18, transform=ax.transAxes, weight='bold', color='black')
        
        
        ## If subplot is on top of plot, draw a title
        
        if i < 3:
        
            ax.set_title(titles[i], color="black", fontdict={'fontsize': 30, 'fontweight': 'bold'})

        
        # cax = inset_axes(ax, width="60%", height="18%", loc='lower center', bbox_to_anchor=(0, -0.05, 1, 0.3),
        #                  bbox_transform=ax.transAxes, borderpad=-1)
        # cbar = fig.colorbar(p1, cax=cax, orientation='horizontal', extend='both')
        # cbar.ax.tick_params(labelsize=14)
    
    
    ## Tight layout for saving
    fig.tight_layout(pad=0.6)
    
    ## Save figure
    plt.savefig(fig_filename, bbox_inches='tight', pad_inches=0.4)


