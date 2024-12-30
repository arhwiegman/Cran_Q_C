# Load dependancies ***************************************
print("loading python modules...")
import arcpy as ap
import os, glob, re, sys, tempfile
import pandas as pd
import numpy as np
import arcpy
#import scipy.stats

# make a list of loaded modules
module_list = [module.__name__ for module in sys.modules.values()]
print("","","`module_list` contains names of all loaded modules")

# Set up arcpy environment ******************************
print("setting up arcpy environment...")

# input data directory
idr = "C:\\Workspace\\Geodata\\Massachusetts\\"
print("input file directory (idr):",idr)

# working directory this should be where arcgis project is located
wdr = "C:\\Workspace\\Geodata\\MEP\\"
print("working directory (wdr):",wdr)

# temporary directory
#tdr = tempfile.TemporaryDirectory()
temp_dir_name = "temp"
if not os.path.exists(temp_dir_name):
    os.mkdir(temp_dir_name)
tdr = os.path.join(wdr,temp_dir_name)
print("temp dir (tdr):",tdr)

# arcpy settings
gdb_default = os.path.join(wdr,"Default.gdb")
print("default geodatabase path:",gdb_default)
ap.env.workspace = gdb_default # set arcpy environment working directory

aprx = arcpy.mp.ArcGISProject("Current") # current project
ap.CheckOutExtension("Spatial") # check out spatial extension
ap.env.outputCoordinateSystem = arcpy.SpatialReference("NAD 1983 UTM Zone 19N") # define cordinate system

# Allow overwriting of output
arcpy.env.overwriteOutput = True
#ap.SetProgressor("step", "processing...", step_value = 5)

print("environment setup complete")

