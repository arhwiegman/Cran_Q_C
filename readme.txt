A geospatial assessment of the  potential for cranberry bogs to intercept and reduce watershed nitrate loads in southeast Massachusetts
Adrian Wiegman, USDA-ARS (lead author, secondary point of contact: adrian.wiegman)
Rachel Jakuba, Buzzard Bay Coalition
Chris Neill, Woodwell Climate Research Center
Casey Kennedy, USDA-ARS (primary point of contact: casey.kennedy@usda.gov) 
Oct 25, 2023

# Contents
This directory holds a project "Cran_Q_C" whereby groundwater discharges (Q) and nitrate concenctrations (C) are modeled at streams and cranberry bogs within the PCKD aquifer and Cape Cod. 

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
