library(readr)
DER_rivers_streams_NO3_TN_data <- read_csv("data/DER_rivers_streams_NO3_TN_data.csv")
DER_alldata_NO3_TN_data <- read_csv("data/DER_all_data_NO3_TN_data.csv")
#View(DER_rivers_streams_NO3_TN_data)
df <- DER_rivers_streams_NO3_TN_data 
df <- DER_alldata_NO3_TN_data
df_DER_streams_wider <- df %>%
  group_by(UNIQUE_ID,Latitude,Longitude,StartDate,StartTime,MLTYP_NAME) %>% 
  mutate(ResVal=str_replace(ResVal,"<(.*)","\\1"),
         ResVal=as.numeric(ResVal)) %>%
  pivot_wider(values_from = ResVal,names_from=DWM_Name) %>%
  rename(NO3=`Nitrate/Nitrite-N`,
         TN=`Total Nitrogen`,
         Lat = Latitude,
         Long = Longitude)

df_DER_streams_summary <- df_DER_streams_wider %>% 
  ungroup %>% 
  select(UNIQUE_ID,Watershed,Waterbody,MLTYP_NAME,Lat,Long,NO3,TN) %>%
  group_by(UNIQUE_ID,Watershed,Waterbody,MLTYP_NAME,Lat,Long) %>%
  summarize(across(everything(),~mean(.,na.rm=T)))

df_DER_streams_summary %>% na.omit() %>% write_csv("df_DER_alldata_NO3_TN_summary.csv")

ggplot(df_DER_streams_summary,aes(NO3,TN,color=Watershed))+
  geom_point()
