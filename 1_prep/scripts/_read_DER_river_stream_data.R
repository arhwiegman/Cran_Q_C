# this script modifies raw data from DER streams
require(tidyverse)
#DER_rivers_streams_NO3_TN_data <- read_csv("0_raw/DER_rivers_streams_NO3_TN_data.csv")
DER_alldata_NO3_TN_data <- read_csv("0_raw/DER_all_data_NO3_TN_data.csv")
#View(DER_rivers_streams_NO3_TN_data)
#df <- DER_alldata_NO3_TN_data %>% filter(str_detect(DESCRIPTOR,"Carver"))
#df <- DER_rivers_streams_NO3_TN_data 
df <- DER_alldata_NO3_TN_data
df_DER_wider <- df %>%
  # filter out QAQC duplicate samples 
  filter(QAQCType!="Duplicate") %>%
  group_by(UNIQUE_ID,Latitude,Longitude,StartDate,StartTime,MLTYP_NAME) %>% 
  mutate(ResVal=str_replace(ResVal,"<(.*)","\\1"),
         ResVal=as.numeric(ResVal)) %>%
  pivot_wider(values_from = ResVal,names_from=DWM_Name) %>%
  rename(NO3=`Nitrate/Nitrite-N`,
         TN=`Total Nitrogen`,
         Lat = Latitude,
         Long = Longitude) %>%
  filter(!(is.na(NO3)&
    is.na(TN)))

summary_func_list <- list(mean = ~mean(.,na.rm=T),
                          n = ~sum(!is.na(.)),
                          sd = ~sd(.,na.rm=T),
                          median = ~mean(.,na.rm=T),
                          max = ~max(.,na.rm=T),
                          min = ~min(.,na.rm=T))

df_DER_summary <- df_DER_wider %>% 
  ungroup %>%
  select(UNIQUE_ID,Watershed,DESCRIPTOR,Waterbody,MLTYP_NAME,Datayear,Lat,Long,NO3,
         TN) %>%
  group_by(UNIQUE_ID,Watershed,DESCRIPTOR,Waterbody,MLTYP_NAME,Lat,Long) %>%
  summarize(across(everything(),summary_func_list)) %>% 
  rename(NO3 = NO3_mean, 
         TN = TN_mean)

df_DER_summary %>% 
  write_csv("1_prep/data/df_DER_AllData_NO3_TN_summary.csv")

df_DER_summary %>% 
  filter((MLTYP_NAME=="River/Stream" | MLTYP_NAME=="Lake" | MLTYP_NAME=="Other-Surface Water")) %>%
  write_csv("1_prep/data/df_DER_SurfaceWater_NO3_TN_summary.csv")

df_DER_stream_summary <- df_DER_summary %>% 
  filter(MLTYP_NAME=="River/Stream")
df_DER_stream_summary %>% write_csv("1_prep/data/df_DER_stream_NO3_TN_summary.csv")

df_DER_stream_summary %>% #filter(str_detect(Watershed,"Buzzards Bay|Cape Cod|South Coastal|Taunton")) %>%
ggplot(aes(NO3,TN,color=Watershed))+
  geom_point()
df_DER_stream_summary %>% filter(!str_detect(Watershed,"Blackstone")) %>%
  ggplot(aes(NO3,TN,color=Watershed))+
  geom_point()

