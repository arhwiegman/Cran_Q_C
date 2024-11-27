# buzzards bay coalition (bbc) surface water quality data

# clear environment
# rm(list=ls ())

# load packages
require(readxl)
require(dplyr)

# all observations
xf ="0_raw/bbcdata1992to2022-ver01June2023-export-Freshwater.xlsx"
df_bbc_all_raw <- read_excel(xf,sheet="all",skip=1)

# station descriptions and coordinates
df_bbc_stations_raw <- read_excel(xf,sheet="Stations",skip=1) 

# methods descriptions for water quality parameters
df_bbc_methods_raw <- read_excel(xf,sheet="Methods",skip=2)

. <- df_bbc_stations_raw %>% filter(STN_ID=="AR0")
y = .$LATITUDE
x = .$LONGITUDE

df_bbc_stations <- df_bbc_stations_raw %>% 
  rename(Lat=LATITUDE,
         Long = LONGITUDE,
         STN_EQUIV = STAT_EQUIV) %>%
  mutate(
    STN = STN_ID,
    COMMENTS = ifelse(is.na(COMMENTS),"_",COMMENTS))

df_bbc_all <- df_bbc_all_raw %>% 
  # convert below detection limit to values 50% of detection limit
  mutate(
    PO4_UM = as.numeric(str_remove(PO4_UM,"<")),
    TP_UM = as.numeric(str_remove(TP_UM,"<")),
    NO2NO3_UM = as.numeric(str_remove(NO2NO3_UM,"<")),
    NH4_UM = as.numeric(str_remove(NH4_UM,"<"))) %>%
  # initialize new columns to whole per element mass concentrations (e.g.  mg/L N as NO3, mg/L P as PO4)
  mutate(NH4 = NH4_UM,
         NO3 = NO2NO3_UM,
         TDN = TDN_UM,
         TN = TN_UM,
         PON = PON_UM,
         TP = TP_UM,
         PO4 = PO4_UM) %>%
  # convert from UM (umol/L) to mg/L 
  mutate(across(c(PO4,TP),~.x*0.030974)) %>%
  mutate(across(c(NH4,NO3,TDN,TN,PON),~.x*0.014007))

# summarize data by station 
summary_func_list <- list(mean = ~mean(.,na.rm=T),
                          n = ~sum(!is.na(.)),
                          sd = ~sd(.,na.rm=T),
                          max = ~max(.,na.rm=T),
                          min = ~min(.,na.rm=T))

df_bbc_stations_summary <- df_bbc_all %>%
  # remove duplicate samples
  filter(DUP==0) %>%
  # group by station id
  group_by(STN_EQUIV,Embayment,SOURCE) %>% 
  # select only numeric variables 
  select(YEAR,SAMP_DATE,TEMP_C,DO_MGL,DO_SAT,NH4, 
         NO3,
         TDN,
         TN,
         PON,
         TP,
         PO4) %>%
  summarize_all(summary_func_list) %>%
  mutate(NO3 = NO3_mean,
         TN=TN_mean,
         n = NO3_n) 

x = df_bbc_stations_summary %>% select(STN_EQUIV,NO3,TN,NO3_n,TN_n,YEAR_min,YEAR_max)
y = df_bbc_stations %>% select(STN_ID,EMBAYMENT,WQI_Area,Lat,Long,Town)
df_bbc_stations_summary_XY_NO3_TN <- left_join(x,y,by=join_by(STN_EQUIV==STN_ID)) %>%
  filter(!is.na(NO3)|!is.na(TN))

df_bbc_stations_summary_XY_NO3_TN %>% ggplot() + geom_label(aes(x=NO3,y=TN,color=Embayment,label=STN_EQUIV)) 
print("outliers: Flume Pond (1), Buttonwood Brook (4), Gunning Point Pond (1)")
  
df_bbc_stations_summary_XY_NO3_TN %>% write_csv("1_prep/data/df_bbc_stations_summary_XY_NO3_TN.csv")

