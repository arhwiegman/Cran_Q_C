# Metadata --------------
# Author: Adrian Wiegman
# Date: 8/24/2024
# Description: prepare TN NO3 concentration data from surface water samples 

# Main program ------------
# clear environment
rm(list=ls ())

# load packages
library(readxl)
library(tidyverse)

# choose to refresh data or not
refresh = TRUE # if true this reruns preprocessing scripts for each dataset

# USGS 
df_USGS_Marston_TN_NO3 <- read_csv("1_prep/data/df_MarstonMillsUSGS_TN_NO3.csv") %>%
  group_by(sitecode,source) %>% 
  summarize(n=sum(!is.na(NO3_mgl)),
            NO3=mean(NO3_mgl),
            TN=mean(TN_mgl,na.rm=T),
            start_date=min(date(datetime)),
            end_date=max(date(datetime))) %>%
  ungroup() %>%
  rename(dataset=source) %>%
  mutate(
    siteinfo = paste("USGS Marston Mills River",sitecode),
    systemtype = "lotic",
    Lat = 41.66710556,
    Long = -70.42362778,
    year_start = year(start_date),
    year_end = year(end_date),
    year = (year_start + year_end)/2,
    years_of_obs = year(end_date) - year(start_date))

# cape cod rivers observatory
df_CCR <- read_excel("0_raw/ARCHIVED CCRO Data all archived 6MARCH2023_Summary_AW.xlsx",sheet=2) %>%
  rename(NO3_n=n,
         years_of_obs=Record_Years) %>%
  mutate(dataset="CCR",
         siteinfo = paste("CCRO",System,Code),
         systemtype = "lotic",
         year_start = year(Start_Date),
         year_end = year(End_Date),
         year = (year_start + year_end)/2,
         years_of_obs=1+year_end-year_start,
         obs_per_year = NO3_n/years_of_obs) 

df_CCR %>% filter(NO3_n > 1) %>% .$NO3_n %>% mean()
df_CCR %>% filter(NO3_n > 1) %>% .$NO3_n %>% sd()
df_CCR %>% filter(NO3_n > 1) %>% .$obs_per_year %>% mean()
df_CCR %>% filter(NO3_n > 1) %>% .$obs_per_year %>% sd()
df_CCR %>% filter(NO3_n > 1) %>% .$years_of_obs %>% mean()
df_CCR %>% filter(NO3_n > 1) %>% .$years_of_obs %>% sd()
df_CCR %>% filter(NO3_n > 1) %>% .$Start_Date %>% min()
df_CCR %>% filter(NO3_n > 1) %>% .$End_Date %>% max()

c(df_CCR %>% filter(NO3_n > 1) %>% .$Start_Date,df_CCR %>% filter(NO3_n > 1) %>% .$End_Date) %>% mean()

# mass estuaries program average of weekly sampling for one year
df_MEP <- read_excel("0_raw/MEP_SummaryData.xlsx",sheet="Data_NA",skip=1) %>%
  mutate(Long = as.double(Corrected_Lon)) %>%
  mutate(Lat = as.double(Corrected_Lat)) %>%
  mutate(Year = as.integer(Year_Start)) %>%
  # mutate(Lat = as.double(Corrected_Lat)) %>%
  select(Region_MEP,Lat,Long,NO3,TN,Year) %>%
  group_by(Region_MEP,Lat,Long) %>% 
  na.omit() %>%
  summarise(NO3 = mean(NO3,na.rm=T),
            TN = mean(TN,na.rm=T),
            year_start = min(Year),
            year_end = max(Year),
            year = (year_start + year_end)/2,
            years_of_obs = year_end - year_start + 1) %>%
  mutate(dataset="MEP",
         siteinfo=Region_MEP,
         systemtype="lotic",
         NO3_n = years_of_obs*52,
         TN_n = NO3_n)

1 - (df_MEP$TN %>% is.na() %>% sum())/(df_MEP$TN %>% length())
1 - (df_MEP$NO3 %>% is.na() %>% sum())/(df_MEP$NO3 %>% length())

# public supply wells on the cape
df_PSW <- read_excel("0_raw/Yearly_Nitrate_Concentration_Averages_in_Public_Supply_Wells_TableToExcel.xlsx") %>% 
  mutate(dataset="PSW",
         systemtype="lentic",
         siteinfo=paste(TOWN,", ",SITE_NAME,PWS_NAME)) %>%
  rename(Lat = LATITUDE,
         Long = LONGITUDE,
         NO3 = YR_AVG,
         NO3_n = 12) %>%
  mutate(year_start = YEAR,
         year_end = YEAR,
         year = YEAR,
         years_of_obs=1+year_end-year_start
         ) %>% 
  mutate(NO3=ifelse(NO3==0,0.01,NO3)) %>%
  filter(!is.na(PWS_NAME))

# buzzards bay coalition data 
if (refresh) source("1_prep/scripts/_prep_bbc_data.R")
df_BBC <- read_csv("1_prep/data/df_bbc_stations_summary_XY_NO3_TN.csv") %>%
  mutate(dataset="BBC",
         siteinfo=paste(WQI_Area,STN_EQUIV),
         systemtype=ifelse(str_detect(siteinfo,"River|Brook")&
                             !str_detect(siteinfo,"Bay|Pond|Marsh|Inlet"),
                           "lotic",
                           "lentic"),
         year_end = YEAR_max,
         year_start = YEAR_min,
         year = (year_start + year_end)/2,
         years_of_obs = 1+year_end - year_start,
         obs_per_year = (NO3_n)/years_of_obs)
1 - (df_BBC$TN %>% is.na() %>% sum())/(df_BBC$TN %>% length())
1 - (df_BBC$NO3 %>% is.na() %>% sum())/(df_BBC$NO3 %>% length())

df_BBC %>% filter(NO3_n > 1) %>% .$NO3_n %>% mean()
df_BBC %>% filter(NO3_n > 1) %>% .$NO3_n %>% sd()
df_BBC %>% filter(NO3_n > 1) %>% .$obs_per_year %>% mean()
df_BBC %>% filter(NO3_n > 1) %>% .$obs_per_year %>% sd()
df_BBC %>% filter(NO3_n > 1) %>% .$years_of_obs %>% mean()
df_BBC %>% filter(NO3_n > 1) %>% .$years_of_obs %>% sd()
# DEP Department of Watershed Protection, Watershed Planning Program (WPP)
if (refresh) source("1_prep/scripts/_read_DER_river_stream_data.R")
df_WPP <- read_csv("1_prep/data/df_DER_SurfaceWater_NO3_TN_summary.csv") %>% 
  mutate(dataset="WPP") %>%
  mutate(year_start=Datayear_min,
         year_end=Datayear_max,
         years_of_obs=1+year_end-year_start) %>%
  mutate(siteinfo=paste(Watershed,Waterbody,MLTYP_NAME,UNIQUE_ID,DESCRIPTOR,sep=" | "),
         systemtype=ifelse(str_detect(MLTYP_NAME,"RIVER/STREAM"),"lotic","lentic"))
         
df_WPP %>% .$Datayear_n %>% mean()
df_WPP %>% .$Datayear_n %>% sd()
df_WPP %>% .$Datayear_max %>% max()
df_WPP %>% .$Datayear_min %>% min()
df_WPP %>% .$Datayear_mean %>% mean()
df_WPP %>% mutate(years_of_obs = Datayear_max - Datayear_min) %>% .$years_of_obs %>% mean()

1 - (df_WPP$TN %>% is.na() %>% sum())/(df_WPP$TN %>% length())
1 - (df_WPP$NO3 %>% is.na() %>% sum())/(df_WPP$NO3 %>% length())

# All data including ponds, wells, etc. 
df_NO3_TN_merged <-  full_join(df_CCR,df_MEP) %>% 
  full_join(df_PSW) %>% 
  full_join(df_WPP) %>% 
  full_join(df_BBC) %>%
  full_join(df_USGS_Marston_TN_NO3)

# write to csv
df_NO3_TN_merged %>% 
  select(Long,Lat,dataset,TN) %>%
  na.omit() %>%
  write_csv("1_prep/df_TN_all.csv")

# write to csv
df_NO3_TN_merged %>% 
  select(Long,Lat,dataset,NO3) %>%
  na.omit() %>%
  write_csv("1_prep/df_NO3_merged.csv")

# write to csv
df_NO3_TN_merged %>% 
  select(Long,Lat,dataset,NO3,TN) %>%
  na.omit() %>%
  write_csv("1_prep/df_NO3_TN_merged.csv")

# Stream data only
df_NO3_TN_merged_XY_streams <- df_NO3_TN_merged %>% 
  filter(dataset!="PSW") %>%
  # filter only the WPP dataset
  group_by(dataset) %>%
  filter(if(any(dataset == "WPP")) MLTYP_NAME=="River/Stream" else TRUE) %>% 
  ungroup

df_NO3_TN_merged %>% 
  select(Long,Lat,dataset,NO3,TN) %>%
  na.omit() %>%
  write_csv("1_prep/df_NO3_TN_river_streams.csv")
  
df_NO3_TN_merged_XY_streams %>% 
    select(Long,Lat,dataset,NO3,TN) %>%
    na.omit() %>%
  write_csv("1_prep/df_TN_river_streams.csv")

df_NO3_TN_merged_XY_streams %>% 
  select(Long,Lat,dataset,NO3) %>% 
  na.omit() %>% 
  write_csv("1_prep/df_NO3_rivers_streams.csv")



## MISSING VALUE IMPUTATION ##

df <- df_NO3_TN_merged %>% 
  filter(Long>=-71.5,Lat<=42.5) %>%
  select(Lat,Long,NO3,TN,NO3_n,TN_n,dataset,year_start,year_end,years_of_obs,systemtype,siteinfo) %>% 
  mutate(TN_n = ifelse(is.na(TN_n),0,TN_n),
         NO3_n = ifelse(is.na(NO3_n),0,NO3_n))

df %>% filter(dataset!="PSW") %>% nrow()
df$NO3 %>% quantile(.5,na.rm=T)
df$TN %>% quantile(.5,na.rm=T)
df$NO3 %>% quantile(.05,na.rm=T)
df$TN %>% quantile(.01,na.rm=T)

mMEP <- lm(TN~NO3,data=df %>% filter(dataset=="MEP"))
mMEP %>% summary()
mWPP <- lm(TN~NO3,data=df %>% filter(dataset=="WPP"))
mWPP %>% summary()
mBBC <- lm(TN~NO3,data=df %>% filter(dataset=="BBC"))
mBBC %>% summary()
mTN <- lm(TN~NO3,data=df)
mNO3 <- lm(NO3~TN,data=df)
mTN %>% summary()
mNO3 %>% summary()

par(mfrow=c(1,1));plot(df$NO3,df$TN)
par(mfrow=c(2,2)); mTN %>% plot();par(mfrow=c(1,1))
mTNb1 = mTN$coefficients[2]
mTNb0 = mTN$coefficients[1]
mNO3b1 = mNO3$coefficients[2]
mNO3b0 = mNO3$coefficients[1]

df_NO3_TN_merged_SE_imputed <- df %>%
  mutate(TN_imp=NO3*mTNb1+mTNb0,
         NO3_imp=(TN-mTNb0)/mTNb1,
         NO3_raw = NO3,
         TN_raw = TN,
         # Impute TN/NO3 values only when TN/NO3 are absent
         # set imputed TN values that fall below intercept equal to intercept
         TN = ifelse(!is.na(TN_raw),
                     TN_raw,
                     ifelse(TN_imp<mTNb0,
                            mTNb0,
                            TN_imp)),
         # set imputed NO3 values that fall below 0 equal to 0.01
         NO3 = ifelse(!is.na(NO3_raw),
                      NO3,
                      ifelse(NO3_imp<0,0.01,NO3_imp))) %>%
  select(-TN_raw,-NO3_raw) %>% 
  mutate(NO3_imp=NO3_n==0,TN_imp=TN_n==0) %>%
  na.omit()

ggplot(df_NO3_TN_merged_SE_imputed) +
  geom_point(aes(NO3,TN_imp),color="red",pch=1)+
  geom_point(aes(NO3_imp,TN),color="blue",pch=1)+
  geom_point(aes(NO3,TN),alpha=0.5)

# Export All imputed data
df_NO3_TN_merged_SE_imputed %>% 
  write_csv("1_prep/df_NO3_TN_merged_SE_imputed.csv")

df_NO3_TN_merged_SE_imputed %>% lm(TN_imp~NO3_imp,data=.) %>% summary()
df_NO3_TN_merged_SE_imputed %>% group_by(dataset) %>% summarise(count=n())
df_NO3_TN_merged_SE_imputed %>% nrow()

save.image("1_prep/data/prep_NO3_data.Rdata")

## APPENDIX UNUSED CODE

# # select data
# x = df_NO3_TN_merged %>%
#   filter(Long>=-71.5,Lat<=42.25) %>% # filter data for SE mass.
#   select(Long,Lat,Year,dataset,TN,NO3)
# # select variables to impute
# myvars = c("NO3","TN")
# 
# 
# x %>% group_by(dataset) %>% summarize(n())
# 
# library(mice)
# mice::md.pattern(x)
# set.seed(3)
# mthd = "norm.predict" # Random indicator for nonignorable data
# x_lm <- complete(mice::mice(x, method = mthd)) %>%
#   select(myvars)
# 
# mthd = "lasso.select.norm" # Random indicator for nonignorable data
# x_ls <- complete(mice::mice(x, method = mthd)) %>%
#   select(myvars)
# 
# x_imputed = x %>% 
#   cbind(x_lm %>% rename_with(~ paste(., "lm", sep = "_"))) %>% 
#   cbind(x_ls %>% rename_with(~ paste(., "ls", sep = "_"))) %>%
#   mutate(TN_imp=TN_ls,
#          NO3_imp=NO3_ls,
#          NO3_imp = ifelse(NO3_imp>TN_imp,TN_imp,NO3_imp),
#          NO3_imp = ifelse(NO3_imp<0,0.01,NO3_imp),
#          TN_imp = ifelse(TN_imp<0,0.02,TN_imp))
# df_NO3_TN_merged_SE_imputed = x_imputed #%>%
# #filter(Long>=-71.5,Lat<=42.25)
# 
# ggplot(df_NO3_TN_merged_SE_imputed)+
#   geom_point(aes(NO3_lm,TN_lm),pch=0,color="blue",stroke=1,alpha=0.5)+
#   geom_point(aes(NO3_ls,TN_ls),pch=3,color="red",stroke=1,alpha=0.25)+
#   geom_point(aes(NO3_imp,TN_imp),pch=10,color="yellow",stroke=1,alpha=0.5)+
#   geom_point(aes(NO3,TN),color="black")+
#   geom_abline(intercept=0,slope=1)+
#   theme_bw()+
#   geom_smooth(aes(NO3,TN),method="lm")
# 
# #xlim(c(-1,7))+
# #ylim(c(-1,7))
# 
# ggplot(df_NO3_TN_merged_SE_imputed)+
#   geom_point(aes(NO3_imp,TN_imp),pch=10,color="yellow",stroke=1,alpha=0.5)+
#   geom_point(aes(NO3,TN),color="black")+
#   geom_abline(intercept=0,slope=1)+
#   theme_bw()#+