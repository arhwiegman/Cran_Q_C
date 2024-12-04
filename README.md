# Cran_Q_C
 Modeling nitrogen reductions to coastal embayments from ecological restoration of cultivated wetlands

## Metadata
Date: 2024-11-26
Journal: Journal of Geophysical Research (Original Research)
Title: Modeling nitrogen reductions to coastal embayments from ecological restoration of cultivated wetlands 
Authors:
 - Adrian R. H. Wiegman 1 *
 - Casey D. Kennedy 1
 - Christopher Neill 2
 - Rachel Jakuba 3
 - Molly Welsh 1
 - David Millar 1
 - Anthony Buda 4
Affiliations 
 - 1 U.S. Department of Agriculture, Agricultural Research Service, Pasture Systems and Watershed Management Research Unit, One State Bog Rd, East Wareham, MA, 02538
 - 2 Woodwell Climate Research Center, 149 Woods Hole Road, Falmouth, MA, 02540, USA
 - 3 Buzzards Bay Coalition, 114 Front St, New Bedford, MA, 02740, USA
 - 4 U.S. Department of Agriculture, Agricultural Research Service, Pasture Systems and Watershed Management Research Unit, Building 3702, Curtin Road, University Park, PA 16802, USA
 - *Contact and Corresponding author: adrian.wiegman@gmail.com; adrian.wiegman@usda.gov 
Keywords: 
 - [groundwater, nitrate, hydrology, water quality, septic systems, peatlands, estuaries, agriculture, Cape Cod]

## Notes

Currently all .Rdata, .csv, .xlsx, .xls, data files are being withheld by the authors to protect the privacy of private citizens who are identifyable in cranberry farm data.

Delete: .Rdata, .csv, .xlsx, .xls from the .gitignore file in order to upload this data to the repository. 

## Contents
This directory holds a project "Cran_Q_C" whereby groundwater discharges (Q) and nitrate concenctrations (C) are modeled at streams and cranberry bogs within the Plymouth-Carver Kingston Duxbury (PCKD) aquifer and Cape Cod. 

There are four active subfolders in this directory:

folder   | description
------   | -----------
`0_raw`    | raw unmodified input data from various primary sources, and links to online sources
`1_prep`   | scripts, images and data outputs for/from preprocessing (prep) 0_raw data
`2_gis`*    | scripts, images and data outputs for/from GIS software (ArcGIS) 1_prep data
`3_post`   | scripts, images and data outputs for/from postprocessing (post) 2_gis data

*due to large file size, only gis outputs that feed into postprocessing scripts are stored in the `2_gis` folder see additional documentation in `2_gis`

flow of information and data

0_raw -> 1_prep -> 2_gis -> 3_post

scripts in the `1_prep` folder read inputs from `0_raw` and write outputs to `1_prep`
scripts in the `2_gis` folder read  inputs from `1_prep` and write outputs to `2_gis`*
scripts in the `3_post` folder read  inputs from `2_gis` and write outputs to `3_post`
tabular data files (e.g. `csv`) are noted with the prefix `df_`

folders entitled `_superceded` contain which are staged for deletion since they superceded the current version of the project
