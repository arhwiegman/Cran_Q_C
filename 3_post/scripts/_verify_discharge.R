# Metadata =====================
# author: Adrian Wiegman
# Verify Flow predictions made by various models. 
# Discharge (Q) units are in cubic meters per day (Q_m3d)


# The text below contains information to pull flow values from cranberry bogs where discharge was measured. 8/27/24
text1 <- "
ID, FID, ORIG_FID, OBJECTID
QshntRJP, 18,192, 876
MrstnMll, 15,155, 802
ChasBrok, 51, 201, 799
BornsBrk, 46, 977, 880
CB-2, 13, 137, 835
CB-5, 11, 136, 837
CB-8, 10, 136, 801
SttBgSc3, 3, 781, 979
Ny&GrlnB, 8, 957, 84
Frogfoot, 6, 527, 1258
WhtIsPFF, 5, 534, 355
WhtIPADM, 7, 935, 1231
WhtSprng, 2, 887, 453
RckyBgNS, 1, 620, 620
AtwoodBg, 4, 323, 384"

# Notes on points that were moved. 
text2 <- "
ID, FA_D8_gwe_lid_lt1m, Notes
HosptlBg, 67, 14744, Move E
LkElzbtS, 71, 4775, Move NE
LittlRvr, 72, 36456, Move E snap to river
MorsPond, 79, 9823, Pond Drains Different Direction, Modified NHD flowline to drain to bay
ScrtnCrk, 86, 25427, Move W To Just Above Jones Ln Bridge
"

#Scaling Factor for Q at bed scale cranberry farm studies. 
#StateBog Section3 = 3.802302711
#Atwood Bog  = 15.75176494

# Farms to pull Q values from. 
text <- "
OBJECTID, ORIG_FID
957
534
323
887
497
201
136
155
137
136
192
977
781
935
527
620
"


# SET UP ENVIRONMENT -----------------

# clear objects from environment 
rm(list=ls()) 

# make sure pacman and tidyverse are installed
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all") # detach all non-base R packages
pacman::p_load(tidyverse) # load tidyverse

# type package names in the string below, use comma as separator
package_names <- "tidyverse,readxl,plotly,broom,corrplot,ggrepel" 

# convert string of package names to a vector
packages <- str_split_1(package_names,",") %>%
  str_extract("\\w+") %>%
  (\(x) x[!is.na(x)])()

# load packages
packages  %>%
   (\(x) try(pacman::p_load(char=x,character.only=T),F))()

# plotting parameters
log_x_scale <- scale_x_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x)))
log_y_scale <- scale_y_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x)))

# read in data ------------
# USGS gauge discharge
df_Q_bogs_streams_XYTableToPoint_TableToExcel <- read_excel("2_gis/df_Q_bogs_streams_XYTableToPoint_edit_extract_Q_C.xls") %>% 
  filter(OBJECTID!="20") %>%
  separate(system_type,c("system",'type')) 

df_Q_stream_verify <- df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% 
  filter(is.na(FID_bogs_agg)) %>%
  select(OBJECTID,starts_with("Q")) %>%
  mutate(cranberry = F) %>%
  na.omit()

df_Q_bogs_verify <- read_excel("2_gis/bogs_agg_verify_extract_Q_C.xls") %>%
  select(OBJECTID=FID_df_Q_bogs_streams,site_id,starts_with("Q")) %>%
  mutate(cranberry = T) %>%
  na.omit()

d <- df_Q_bogs_streams_XYTableToPoint_TableToExcel[,1:8] %>% 
  right_join(df_Q_stream_verify) 
d2 <- df_Q_bogs_streams_XYTableToPoint_TableToExcel[,1:8] %>% 
  right_join(df_Q_bogs_verify)
# Stitch together bogs and non bog sites
df_Q_bogs_streams_verify <- full_join(d,d2) %>%
  mutate(org=ifelse(source=="USGS",source,"USDA"),
         org=ifelse(source=="MEP",source,org)) %>%
  mutate(
    resid = Q_m3d-Q_m3d_D8gwelid0,
    sqresid = resid^2,
    rtsqresid = sqrt(sqresid),
    RPD=100*(resid)/Q_m3d,
    rtsqRPD = sqrt(RPD^2))
df <- df_Q_bogs_streams_verify 

# CONVERT TO LONG FORMAT ------------

df_Q_bogs_streams_verify_long <- df_Q_bogs_streams_verify %>% 
  group_by(OBJECTID,site_id,Q_m3d,cranberry) %>%
  rename(Q_obs=Q_m3d) %>% 
  select(starts_with("Q_m3d")) %>%
  pivot_longer(cols=!group_cols(),
               names_to="method",
               values_to="Q_pred") %>%
  mutate(
    resid = Q_obs-Q_pred,
    sqresid = resid^2,
    rtsqresid = sqrt(sqresid),
    RPD=100*(resid)/Q_obs,
    rtsqRPD = sqrt(RPD^2)) %>%
  na.omit()

df_Q_bogs_verify_long <- df_Q_bogs_verify %>%
  group_by(OBJECTID,site_id,Q_m3d) %>%
  rename(Q_obs=Q_m3d) %>% 
  select(starts_with("Q_m3d")) %>%
  pivot_longer(cols=!group_cols(),
               names_to="method",
               values_to="Q_pred") %>%
  mutate(
    resid = Q_obs-Q_pred,
    sqresid = resid^2,
    rtsqresid = sqrt(sqresid),
    RPD=100*(resid)/Q_obs,
    rtsqRPD = sqrt(RPD^2)) %>%
  na.omit()


# FIT LINEAR MODELS TO obs vs predicted Q ---------


library(broom)
# Analyze bogs only -------------------
df_Q_bogs_verify_long_nest <- df_Q_bogs_verify_long %>% 
  group_by(method) %>% 
  nest() 
df_Q_bogs_lmfit <- df_Q_bogs_verify_long_nest %>%
  mutate(
    lmfit = map(data,~lm(Q_obs ~ Q_pred, data = .x))) %>%
  mutate(tidy = map(lmfit, tidy),
         augment = map(lmfit,augment),
         glance = map(lmfit, glance)) %>%
  unnest(glance) %>%
  arrange(by=AIC)
df_Q_bogs_RMSE <- df_Q_bogs_verify_long %>%
  group_by(method) %>%
  summarise(RMSE = mean(rtsqresid),
            MAE = mean(abs(resid))) %>%
  arrange(RMSE)

df_Q_bogs_RMSE %>% write_csv("3_post/df_Q_bogs_RMSE.csv")

###  bogs and streams separately ------------------------------
df_Q_bogs_streams_grp_long_nest <- df_Q_bogs_streams_verify_long %>% 
  group_by(method,cranberry) %>% 
  nest() 
df_Q_bogs_streams_grp_lmfit <- df_Q_bogs_streams_grp_long_nest %>%
  mutate(
    lmfit = map(data,~lm(Q_obs ~ Q_pred, data = .x))) %>%
  mutate(tidy = map(lmfit, tidy),
         augment = map(lmfit,augment),
         glance = map(lmfit, glance)) %>%
  unnest(glance) %>%
  arrange(by=AIC)
df_Q_bogs_streams_grp_RMSE <- df_Q_bogs_streams_verify_long %>%
  group_by(method,cranberry) %>%
  summarise(RMSE = sqrt(mean(sqresid,ra.rm=T)),
            MAE = mean(abs(resid),na.rm=T)) %>%
  arrange(RMSE)
slope <- df_Q_bogs_streams_grp_lmfit %>% 
  select(method,cranberry,tidy) %>%
  unnest(tidy) %>%
  filter(term=="Q_pred") %>%
  select(method,cranberry,estimate) %>%
  pivot_wider(names_from=cranberry,values_from=estimate,
                    names_glue = "b1_{cranberry}")
intcpt <- df_Q_bogs_streams_grp_lmfit %>% 
  select(method,cranberry,tidy) %>%
  unnest(tidy) %>%
  filter(term=="(Intercept)") %>%
  select(method,cranberry,estimate) %>%
  pivot_wider(names_from=cranberry,values_from=estimate,
              names_glue = "b0_{cranberry}")
r2 <- df_Q_bogs_streams_grp_lmfit %>% 
  select(method,cranberry,r.squared) %>% 
  pivot_wider(names_from=cranberry,values_from=c(r.squared),
              names_glue = "{.value}_{cranberry}")
rmse <- df_Q_bogs_streams_grp_RMSE %>% pivot_wider(names_from=cranberry,
                                           values_from=c(RMSE,MAE))
df_Q_verif_summary <- left_join(r2,rmse) %>% 
  left_join(slope) %>% 
  left_join(intcpt) %>% 
  write_csv("3_post/df_Q_verif_summary.csv")

### Analyse bogs and streams together-------------------
df_Q_bogs_streams_long_nest <- df_Q_bogs_streams_verify_long %>% 
  group_by(method) %>% 
  nest() 
df_Q_bogs_streams_lmfit <- df_Q_bogs_streams_long_nest %>%
  mutate(
    lmfit = map(data,~lm(Q_obs ~ Q_pred, data = .x))) %>%
  mutate(tidy = map(lmfit, tidy),
         augment = map(lmfit,augment),
         glance = map(lmfit, glance)) %>%
  unnest(glance) %>%
  arrange(by=AIC)
df_Q_bogs_streams_RMSE <- df_Q_bogs_streams_verify_long %>%
  group_by(method) %>%
  summarise(RMSE = sqrt(mean(sqresid,ra.rm=T)),
            MAE = mean(abs(resid),na.rm=T)) %>%
  arrange(RMSE) %>% left_join(df_Q_bogs_streams_lmfit %>% select(r.squared))
df_Q_bogs_streams_RMSE %>%
  arrange(MAE) %>% 
  write_csv("3_post/df_Q_bogs_streams_RMSE.csv")

# Obs vs Pred PLOTS ---------------

ggplot()+
  geom_point(data=df_Q_bogs_streams_RMSE,aes(MAE,RMSE))

df_Q_stream_verify_model <- df_Q_bogs_streams_lmfit %>% unnest(data) 
df_select <- df_Q_stream_verify_model %>%
  filter(r.squared>0.87) %>%
  filter(!str_detect(method,"r|lt|k1lid"))
df_select %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=method))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  ylim(c(0,120000))+
  xlim(c(0,120000))
ggplotly()

df_select %>% filter(method=="Q_m3d_D8gwelid0") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  ylim(c(0,120000))+
  xlim(c(0,120000))

df_select %>% filter(method=="Q_m3d_D8gweblidbk1") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_text_repel(aes(label=site_id))+
  ylim(c(0,120000))+
  xlim(c(0,120000))

df_select %>% filter(method=="Q_m3d_D8gweblidbk1") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_text_repel(aes(label=site_id))+
  ylim(c(0,20000))+
  xlim(c(0,20000))

df_select %>% filter(method=="Q_m3d_D8gwebc") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_text_repel(aes(label=site_id))+
  ylim(c(0,120000))+
  xlim(c(0,120000))

df_select %>% filter(method=="Q_m3d_D8gweblidbk5") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)#+
  #ylim(c(0,120000))+
  #xlim(c(0,120000))

df_select %>% filter(method=="Q_m3d_D8gwelid1") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)
  #ylim(c(0,120000))+
  #xlim(c(0,120000))

df_select %>% filter(method=="Q_m3d_D8gwelid0") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_text_repel(aes(label=site_id))+
  ylim(c(0,120000))+
  xlim(c(0,120000))
df_select %>% filter(method=="Q_m3d_D8gwelid0") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_text_repel(aes(label=site_id))+
  ylim(c(0,25000))+
  xlim(c(0,25000))

df_avg <- df_Q_bogs_streams_verify_long %>% 
  filter(method %in% c("Q_m3d_D8gweblidbk1","Q_m3d_D8gweblidbk5",
                       "Q_m3d_D8gweb","Q_m3d_D8gwebc",
                       "Q_m3d_D8gwelid0","Q_m3d_D8gwelid1")) %>%
  ungroup() %>%
  select(site_id,OBJECTID,cranberry,Q_obs,Q_pred) %>% 
  group_by(site_id,OBJECTID,cranberry,Q_obs) %>% 
  summarise(Q_pred_med=median(Q_pred),
            Q_pred_mean=mean(Q_pred))
lm(Q_obs~Q_pred_mean,data=df_avg) %>% summary()
lm(Q_obs~Q_pred_med,data=df_avg) %>% summary()

library(ggrepel)
ggplot(df_avg,aes(x=Q_pred_med,y=Q_obs,color=cranberry))+
  geom_abline(slope=1,intercept=0)+
  geom_point()+
  geom_smooth(method="lm")+
  geom_text_repel(aes(label=site_id))+
  xlim(c(0,120000))+
  ylim(c(0,120000))
ggplot(df_avg,aes(x=Q_pred_med,y=Q_obs,color=cranberry))+
  geom_abline(slope=1,intercept=0)+
  geom_point()+
  geom_text_repel(aes(label=site_id))+
  xlim(c(0,25000))+
  ylim(c(0,25000))


## ANALIZE RELATIVE RESIDUAL AR

df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% 
  filter(source=="USGS") %>%
  lm(Q_m3d~Q_m3d_D8gwelid0,data=.) %>%
  summary()
df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% 
  filter(!is.na(FID_bogs_agg)) %>%
  lm(Q_m3d~Q_m3d_D8gwelid0,data=.) %>%
  summary()
df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% 
  filter(source=="USGS") %>%
  lm(Q_m3d~Q_m3d_D8gweblidbk1,data=.) %>%
  summary()
df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% 
  filter(!is.na(FID_bogs_agg)) %>%
  lm(Q_m3d~Q_m3d_D8gweblidbk1,data=.) %>%
  summary()
  

df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% 
  filter(source=="USGS") %>%
  ggplot(aes(y=Q_m3d,x=Q_m3d_D8gwelid1))+geom_point()+
  geom_abline(slope=1,intercept=0)

df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% 
  filter(source=="USGS") %>%
  ggplot(aes(y=Q_m3d,x=Q_m3d_D8gweblidbk1))+geom_point()+
  geom_abline(slope=1,intercept=0)
df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% 
  filter(!is.na(FID_bogs_agg)) %>%
  ggplot(aes(y=Q_m3d,x=Q_m3d_D8gweblidbk1))+geom_point()+
  geom_abline(slope=1,intercept=0)





ggplot(data=df,aes(x=Q_m3d,y=rtsqresid)) +
  geom_point()+
  geom_smooth(method="lm",se=F) +
  geom_abline(slope=1,intercept=0)+
  log_x_scale+
  log_y_scale+
  theme(panel.grid.minor = element_blank())+
  annotation_logticks()+
  theme_bw()
ggplotly()

qplot(x=Q_m3d,y=rtsqresid,data=df) + 
  geom_smooth(method="lm",) +
  geom_abline(slope=1,intercept=0)+
  log_x_scale+
  log_y_scale+
  theme(panel.grid.minor = element_blank())+
  annotation_logticks()+
  theme_bw()
ggplotly()

library(corrplot)
df %>% 
  ungroup() %>% 
  select(Q_m3d,c("Q_m3d_D8gweblidbk1","Q_m3d_D8gweblidbk5",
                 "Q_m3d_D8gweb","Q_m3d_D8gwebc","Q_m3d_D8gwebr","Q_m3d_D8gwebcr",
                 "Q_m3d_D8gwelid0","Q_m3d_D8gwelid1")) %>% 
  na.omit %>%
  as.matrix() %>%
  cor() %>%
  corrplot(method="number")

df %>% 
  ungroup() %>% 
  select(Q_m3d,c("Q_m3d_D8gweblidbk1","Q_m3d_D8gweblidbk5",
                 "Q_m3d_D8gweb","Q_m3d_D8gwebc","Q_m3d_D8gwebr","Q_m3d_D8gwebcr",
                 "Q_m3d_D8gwelid0","Q_m3d_D8gwelid1")) %>% 
  na.omit %>%
  plot()

df_Q_bogs_verify %>% 
  ungroup() %>% 
  select(Q_m3d,c("Q_m3d_D8gweblidbk1","Q_m3d_D8gweblidbk5",
                 "Q_m3d_D8gweb","Q_m3d_D8gwebc","Q_m3d_D8gwebr",
                 "Q_m3d_D8gwelid0","Q_m3d_D8gwelid1")) %>% 
  na.omit %>%
  plot()

df_Q_bogs_verify %>% 
  ungroup() %>% 
  select(Q_m3d,c("Q_m3d_D8gweblidbk1","Q_m3d_D8gweblidbk5",
                 "Q_m3d_D8gweb","Q_m3d_D8gwebc","Q_m3d_D8gwebr",
                 "Q_m3d_D8gwelid0","Q_m3d_D8gwelid1")) %>% 
  na.omit %>%
  as.matrix() %>%
  cor() %>%
  corrplot(method="number")

# root square of relative percent difference between observed and predicted as a function of the observed value
ggplot(data=df,aes(x=Q_m3d,y=RPD,color==cranberry)) +
  geom_point(pch=21,fill="transparent")+
  log_x_scale+
  #log_y_scale+
  theme(panel.grid.minor = element_blank())


save.image("3_post/data/_verify_discharge.Rdata")