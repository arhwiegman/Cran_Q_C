# U.S. Geological Survey National Water Information System
# https://waterdata.usgs.gov/nwis

# clear environment
rm(list=ls ())

# load packages
require(readr)
require(dplyr)

NWISMapperExport <- read_csv("0_raw/USGS/NWIS/SurfaceWater/Active/NWISMapperExport.csv")
View(NWISMapperExport)