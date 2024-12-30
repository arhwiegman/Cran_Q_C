library(tidyverse)
# get file names ending with 
setwd("C:/Users/Adrian.Wiegman/Documents/GitHub/Wiegman_USDA_ARS/MEP/data/USGS_Guage_Discharge_Data")
filenames = list.files(getwd()) %>% 
  .[str_detect(.,"txt$")] %>% 
  .[!str_detect(.,"_Info")]

# make a dataframe to store summary statistics
df_info = read_delim("_Info.txt",
                        comment= c("#")) %>%
  .[2:nrow(.),] %>% 
  select(site_no,station_nm,dec_lat_va,dec_long_va,dec_coord_datum_cd)

# read in data from each station and combine into a long dataframe
i = 0
for (fn in filenames){
  print(fn)
  df=read_delim(fn,
                col_names = c("agency","site_no","datetime","tz","Q_cfs","qc_flag"),
                col_types = "ccTcdc",
                comment= c("#")) %>% 
    mutate(site_name=fn %>% str_sub(1,-5)) %>%
    .[3:nrow(.),] # remove first row
  print(head(df))
  if(i == 0){
    df_combined = df
  }else{
      df_combined=bind_rows(df_combined,df)
      }
  i = i + 1
} 

df_summary <- df_combined %>% 
  group_by(site_name,site_no) %>% 
  mutate(Q_m3d=Q_cfs*0.028316847*60*60*24) %>% 
  na.omit() %>%
  summarize_at(vars(datetime,Q_m3d),list(~min(.),~max(.),~mean(.),~sd(.))) %>%
  mutate(timespan = datetime_max - datetime_min) %>%
  select(-c(datetime_mean,datetime_sd)) %>% ungroup

df_info_summary <- left_join(df_info,df_summary,by="site_no") 

df_info_summary %>% write_csv('df_MA_USGS_GUAGE_summary.csv')

