# Prep discharge data to verify flow estimates

# SET UP ENVIRONMENT -----------------

# clear objects from environment 
rm(list=ls()) 

# make sure pacman and tidyverse are installed
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all") # detach all non-base R packages
pacman::p_load(tidyverse) # load tidyverse

# type package names in the string below, use comma as separator
package_names <- "tidyverse,readxl" 

# convert string of package names to a vector
packages <- str_split_1(package_names,",") %>%
  str_extract("\\w+") %>%
  (\(x) x[!is.na(x)])()

# load packages
packages  %>%
   (\(x) try(pacman::p_load(char=x,character.only=T),F))()


# read in data ------------
# USGS gauge discharge
source("1_prep/scripts/_read_USGS_gauges.R")
df_MA_USGS_GUAGE_summary_raw <- read_csv("1_prep/data/df_MA_USGS_GUAGE_summary.csv")
df_MA_USGS_GUAGE_summary <- df_MA_USGS_GUAGE_summary_raw %>%
  mutate(system_type="Stream Gauge") %>% 
  rename(Lat = dec_lat_va,Long = dec_long_va) %>%
  mutate(site_id=abbreviate(site_name,8)) %>% 
  rename(Q_m3d = Q_m3d_mean) %>%
  mutate(dates = paste0(year(datetime_min),"-",year(datetime_max)),
         source = "USGS")

# Cold Brook Bog section discharge
df_Cold_Brook_Q_Coords <- read_csv("0_raw/Cold_Brook_Q_Coords.csv") %>%
  mutate(
    Q_m3d = Q_m3d_15,
    dates = "2014-2015")
  
# MEP Discharge 
df_MEP_SummaryData_Coords <- read_csv("0_raw/MEP_SummaryData_Coords.csv") %>%
  rename(Long=Lon,
         Q_m3d = `Qmeas (m3/d)`,
         Q_m3d_mod = `Qmod (m3/d)`,
         dates = Dates) %>%
  mutate(site_id=abbreviate(SiteName,8),
         source="MEP",
         system_type = "Stream Gauge")

# Cranberry bog discharge
df_BogFlowData_raw <- read_excel("0_raw/BogFlowData.xlsx")
df_BogFlowData <- df_BogFlowData_raw %>% 
  mutate(system_type=paste("Cranberry",Scale))%>% 
  rename(Long=Lon,dates=Year) %>%
  mutate(site_id=abbreviate(Site,8)) %>%
  mutate(Q_m3y=`Qsw+gw-in (m3/yr)`,
         Q_m3d=Q_m3y/365.25,
         section_area_m2 = Study_ha*10000,
         HLR_md = Q_m3d/section_area_m2,
         source = ifelse(Reference=="unpublished","Kennedy, unpublished",Reference))

# Join the data sets -----------------------------
df_Q_bogs_streams = df_BogFlowData %>% 
  full_join(df_Cold_Brook_Q_Coords) %>%
  full_join(df_MA_USGS_GUAGE_summary) %>%
  full_join(df_MEP_SummaryData_Coords) %>%
  select(source,site_id,Lat,Long,source,dates,system_type,Q_m3d) %>%
  na.omit()


save.image("1_prep/data/prep_Discharge_data.Rdata")
df_Q_bogs_streams %>% write_csv("1_prep/df_Q_bogs_streams.csv")