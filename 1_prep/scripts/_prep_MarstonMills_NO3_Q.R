library(tidyverse)
cnames = c("source","sitecode","datetime","timezone","H_ft","H_QC","Q_cfs","Q_QC","NO3_mgl","C_QC")
df_Marston_QC_raw <- read_delim("0_raw/USGS/Gauges/MarstonMills_Nitrate_20240702.txt",skip=29,
           col_names = cnames)

cnames = c("source","sitecode","date","time","sample_end_dt","sample_end_tm","sample_start_time_datum_cd","tm_datum_rlbty_cd","coll_ent_cd","medium_cd","tu_id","body_part_id",	"r00600",	"TN_mgl",	"r00607",	"p00607",	"r00608",	"p00608",	"r00613",	"p00613",	"r00631",	"NO3_mgl")
ctypes = rep("guess",length(cnames))
ctypes[3:4] = "character"
df_Marston_TN_NO3 <- read_delim("0_raw/USGS/Gauges/MarstonMills_Lab_NO3_TN.txt",
                                skip=72,
                                col_names = cnames) %>%
  mutate(
    datetime=as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S")) %>%
  select(source,sitecode,datetime,TN_mgl,NO3_mgl)


df_Marston_TN_NO3 %>% lm(TN_mgl~NO3_mgl,data=.) %>% summary()
df_Marston_TN_NO3 %>% ggplot(aes(TN_mgl,NO3_mgl))+
  geom_point()+ 
  geom_smooth(method="lm")
df_Marston_TN_NO3$NO3_mgl %>% summary()
df_Marston_TN_NO3$TN_mgl %>% summary()

df_Marston_TN_NO3 %>% write_csv("1_prep/data/df_MarstonMillsUSGS_TN_NO3.csv")

df_Marston_QC <- df_Marston_QC_raw %>% mutate(Q_m3s = Q_cfs * 0.028316847,
         Q_m3d = Q_m3s * 60 * 60 * 24,
         L_NO3_kgNd = NO3_mgl * Q_m3d) %>%
  filter(datetime > "2021-10-01") %>%
  filter(datetime < "2024-09-30") %>%
  na.omit()
df <- df_Marston_QC


sum_Q_m3d <- df$Q_m3d %>% sum()
sum_L_NO3_kgNd <- df$L_NO3_kgNd %>% sum()
mean_NO3_mgL <- sum_L_NO3_kgNd/sum_Q_m3d
print(mean_NO3_mgL)
mean_Q_m3d <- df$Q_m3d %>% mean()
print(mean_Q_m3d)
mean_L_NO3_kgNy <- mean_Q_m3d*mean_NO3_mgL*365/1000
print(mean(mean_L_NO3_kgNy))
mean_est_TN_mgL <- 0.958*mean_NO3_mgL + 0.451 # regression for 25 points 
print(mean(mean_est_TN_mgL))
mean_est_L_TN_kgNy <- mean_est_TN_mgL*mean_Q_m3d*365/1000
print(mean(mean_est_L_TN_kgNy))


