# Metadata =====================
# author: Adrian Wiegman
# Verify Flow predictions made by various models. 

# This contains information to pull flow values from cranberry bogs
# where discharge was measured. 8/27/24
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
package_names <- "tidyverse,readxl,plotly,broom,corrplot" 

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
df_Q_bogs_streams_RMSE %>% write_csv("3_post/df_Q_bogs_streams_RMSE.csv")

ggplot()+
  geom_point(data=df_Q_bogs_streams_RMSE,aes(MAE,RMSE))

# calculate average
df_rsq <- df_Q_stream_verify_long_nest %>% 
  select(method,rsqSt = r.squared) %>%
  left_join(df_Q_bogs_verify_long_nest %>% 
              select(method,rsqBg = r.squared)) %>%
  na.omit() %>%
  mutate(rsq_mean = (rsqBg+rsqSt)/2) %>%
  arrange(by=rsq_mean)
model<- df_Q_bogs_verify_long_nest$method
rsqb <- df_Q_bogs_verify_long_nest$r.squared
rsqs <- df_Q_stream_verify_long_nest$r.squared
(rsqBg+rsqSt)/2



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
  geom_abline(slope=1,intercept=0)+
  ylim(c(0,120000))+
  xlim(c(0,120000))
df_select %>% filter(method=="Q_m3d_D8gwelid1") %>%
  ggplot(aes(x=Q_pred,y=Q_obs,fill=method,color=cranberry))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  ylim(c(0,120000))+
  xlim(c(0,120000))

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



mycolors = scale_color_manual(values=c("grey20","blue"))
g1 <- ggplot(df%>% filter(is.na(FID_bogs_agg)))+ 
               geom_point(aes(y=Q_m3d,x=Q_m3d_D8gwelid0),pch=21,fill="grey90",size=1.5)+
             geom_smooth(aes(y=Q_m3d,x=Q_m3d_D8gwelid0),method="lm",linetype="dashed")
ggplotly()
lm(Q_m3d~Q_m3d_D8gwelid1,data=df%>% filter(is.na(FID_bogs_agg))) %>% summary
lm(resid~Q_m3d_D8gwelid1,data=df%>% filter(is.na(FID_bogs_agg))) %>% summary

# MAKE OBS VS PRED PLOTS -----------

myshapes = scale_shape_manual(values=c(21,22,24))
myfills = scale_fill_manual(values=c("grey90","yellow","blue2"))
g2 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  geom_point(aes(y=Q_m3d,x=Q_m3d_D8gwelid0,
                               fill=org,
                               shape=org),
             alpha=0.75,size=2.5)+
  geom_point(data=df %>% filter(cranberry),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid0),
             #position=position_nudge(x=100,y=100),
             color="white",
             alpha=0.75,pch=16,size=1.5)+
  geom_point(data=df %>% filter(cranberry),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid0),
             #position=position_nudge(x=100,y=100),
             color="red",
             alpha=0.75,pch=20,size=1)+

  myshapes+
  myfills
g2 +  
  xlim(c(0,12000))+
  ylim(c(0,12000))+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())
ggsave("images/fig_verify_Q_gwelid0_inset.png",width=4*.5,height=4*.5,dpi=600)
g2 +  
  xlim(c(0,120000))+
  ylim(c(0,120000))+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())
ggsave("images/fig_verify_Q_gwelid0.png",width=4,height=4,dpi=600)

df

g2 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  geom_point(data = df %>% filter(!is.na(FID_bogs_agg)),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid0),
             shape=22,fill="red",alpha=0.75,size=2)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid0_bogs_agg),
             shape=23,fill="transparent",alpha=0.75,size=2)
# FOR THE PAPER. 
ggplot(data=df,aes(x=Q_m3d,y=rtsqRPD,color==cranberry)) +
  geom_point(pch=21,fill="transparent")+
  geom_smooth(method="lm",se=F)+
  log_x_scale+
  log_y_scale+
  theme(panel.grid.minor = element_blank())+
  annotation_logticks()+
  theme_bw()
ggplotly()

lm(resid~Q_m3d_D8gwelid0,data=df%>% filter(cranberry)) %>% summary
lm(resid~Q_m3d_D8gwelid0,data=df%>% filter(org=="USGS"))%>% summary
lm(resid~Q_m3d_D8gwelid0,data=df) %>% summary
   
lm(Q_m3d~Q_m3d_D8gwelid0,data=df %>% filter(cranberry)) %>% summary
lm(Q_m3d~Q_m3d_D8gwelid0,data=df %>% filter(org=="USGS"))%>% summary
lm(Q_m3d~Q_m3d_D8gwelid0,data=df) %>% summary
df$resid^2%>%mean() %>% sqrt()

#EXTRA --------------



























ggplotly()
g2 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Q_m3d,x=Q_m3d_D8gwebflt1m),
             fill="grey90",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Q_m3d,x=Q_m3d_D8gwebflt1m,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Q_m3d,x=Q_m3d_D8gwebflt1m),
             shape=22,fill="red",alpha=0.75,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  theme_bw()
ggplotly()


g2 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid0),
             fill="grey90",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid0,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid0),
             shape=22,fill="red",alpha=0.75,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  xlim(c(0,120000))+
  ylim(c(0,120000))+
  theme_bw()

g2 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid1),
             fill="grey90",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid1,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid1),
             shape=22,fill="red",alpha=0.75,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  xlim(c(0,120000))+
  ylim(c(0,120000))+
  theme_bw()

ggplotly()

g2 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid1),
             fill="grey90",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid1,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelid1),
             shape=22,fill="red",alpha=0.75,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  xlim(c(0,120000))+
  ylim(c(0,120000))+
  theme_bw()

g2 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt1m),
             fill="grey90",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt1m,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt0m),
             shape=22,fill="red",alpha=0.75,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  xlim(c(0,120000))+
  ylim(c(0,120000))+
  theme_bw()

g2 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  #geom_line(data=df_pred, aes(x=Q_m3d_D8gwebf,y=Q_m3d_D8gwebf-rmse1),linetype="dashed")+
  #geom_line(data=df_pred, aes(x=Q_m3d_D8gwebf,y=Q_m3d_D8gwebf+rmse1),linetype="dashed")+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Q_m3d,x=Q_m3d_D8gwebf),
             fill="grey90",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Q_m3d,x=Q_m3d_D8gwebf,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2)+
  geom_point(data = df %>% filter(system == "Cranberry"),
                aes(y=Q_m3d,x=Q_m3d_D8gwebf),
                shape=22,fill="red",alpha=0.75,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  theme_bw()
ggsave("images/fig_verify_Q.png",width=4,height=4,dpi=600)


g2 + 
  log_x_scale+
  log_y_scale+
  theme(panel.grid.minor = element_blank())+
  annotation_logticks()
ggsave("images/fig_verify_Q_log.png",width=4,height=4,dpi=600)


# GW LIDAR HYBRID DEM

g3 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  #geom_line(data=df_pred, aes(x=Q_m3d_D8gwebf,y=Q_m3d_D8gwebf-rmse1),linetype="dashed")+
  #geom_line(data=df_pred, aes(x=Q_m3d_D8gwebf,y=Q_m3d_D8gwebf+rmse1),linetype="dashed")+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt1m),
             fill="grey90",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt1m,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt1m),
             shape=22,fill="red",alpha=0.75,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  theme_bw()
ggsave("images/fig_verify_Q.png",width=4,height=4,dpi=600)

g4 <- ggplot(df) + 
  geom_abline(intercept=0,slope=1)+
  #geom_line(data=df_pred, aes(x=Q_m3d_D8gwebf,y=Q_m3d_D8gwebf-rmse1),linetype="dashed")+
  #geom_line(data=df_pred, aes(x=Q_m3d_D8gwebf,y=Q_m3d_D8gwebf+rmse1),linetype="dashed")+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt0m),
             fill="grey90",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt0m,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Q_m3d,x=Q_m3d_D8gwelidlt0m),
             shape=22,fill="red",alpha=0.75,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  theme_bw()
ggsave("images/fig_verify_Q.png",width=4,height=4,dpi=600)

df$system



g3 +
  xlim(c(1500,15000))+
  ylim(c(1500,15000))
g2 +
  xlim(c(0,105000))+
  ylim(c(0,105000))
ggplotly()

g3 + 
  log_x_scale+
  log_y_scale+
  theme(panel.grid.minor = element_blank())+
  annotation_logticks()
ggplotly()
ggsave("images/fig_verify_Q_log.png",width=4,height=4,dpi=600)

lm(Q_m3d~0+Q_m3d_Dinfgwebf,data = df %>% filter(system == "Cranberry")) %>% summary

lm(Q_m3d~0+Q_m3d_Dinfgwebf,data = df %>% filter(system != "Cranberry") %>% filter(source=="USGS")) %>% summary

lm(Q_m3d~0+Q_m3d_Dinfgwebf,data = df %>% filter(system != "Cranberry") %>% filter(source=="MEP")) %>% summary

lm(Q_m3d~0+Q_m3d_Dinfgwebf,data = df) %>% summary
df %>% nrow()

lm(Q_m3d~Q_m3d_Dinfgwebf,data = df %>% filter(system == "Cranberry")) %>% summary

lm(Q_m3d~Q_m3d_Dinfgwebf,data = df %>% filter(system != "Cranberry") %>% filter(source=="MEP")) %>% summary

lm(Q_m3d~Q_m3d_Dinfgwebf,data = df %>% filter(system != "Cranberry") %>% filter(source=="USGS")) %>% summary

lm(Q_m3d~Q_m3d_D8gwebf,data = df) %>% summary
lm(Q_m3d~Q_m3d_D8gwelidlt1m,data = df) %>% summary
lm(Q_m3d~Q_m3d_D8gwelid1,data = df) %>% summary
lm(Q_m3d~Q_m3d_D8gwelid0,data = df) %>% summary


# D8 Model
lm(Q_m3d~Q_m3d_D8gwebf,data = df %>% filter(system == "Cranberry")) %>% summary

lm(Q_m3d~Q_m3d_D8gwebf,data = df %>% filter(system != "Cranberry") %>% filter(source=="MEP")) %>% summary

lm(Q_m3d~Q_m3d_D8gwebf,data = df %>% filter(system != "Cranberry") %>% filter(source=="USGS")) %>% summary

lm(Q_m3d~Q_m3d_D8gwebf,data = df) %>% summary

lm(Q_m3d~0+Q_m3d_D8gwebf,data = df %>% filter(system == "Cranberry")) %>% summary

lm(Q_m3d~0+Q_m3d_D8gwebf,data = df %>% filter(system != "Cranberry") %>% filter(source=="MEP")) %>% summary

lm(Q_m3d~0+Q_m3d_D8gwebf,data = df %>% filter(system != "Cranberry") %>% filter(source=="USGS")) %>% summary

lm(Q_m3d~Q_m3d_Dinfgwebf,data = df) %>% summary
lm(Q_m3d~Q_m3d_MDFgwebf,data = df) %>% summary


fn_prediction_error_of_Q_m3d_D8gwebf <- function(predicted_value){
  # ERROR BETWEEN OBSERVED AND PREDICTED
  # Call:
  #   lm(formula = Q_m3d ~ Q_m3d_D8gwebf, data = df)
  # 
  # Residuals:
  #   Min     1Q Median     3Q    Max 
  # -26435  -3219  -1068   1930  30145 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)   1.635e+03  1.015e+03    1.61    0.111    
  # Q_m3d_D8gwebf 8.271e-01  3.939e-02   21.00   <2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 8075 on 82 degrees of freedom
  # Multiple R-squared:  0.8432,	Adjusted R-squared:  0.8412 
  # F-statistic: 440.8 on 1 and 82 DF,  p-value: < 2.2e-16
  
  # ERROR BETWEEN DIFFERENT PREDICTION METHODS
  # Call:
  #   lm(formula = Q_m3d_Dinfgwebf ~ Q_m3d_D8gwebf, data = df)
  # 
  # Residuals:
  #   Min       1Q   Median       3Q      Max 
  # -19853.9   -873.2   -387.8    776.3  12709.0 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)   421.93237  538.38135   0.784    0.435    
  # Q_m3d_D8gwebf   0.99149    0.02089  47.458   <2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 4282 on 82 degrees of freedom
  # Multiple R-squared:  0.9649,	Adjusted R-squared:  0.9644 
  # F-statistic:  2252 on 1 and 82 DF,  p-value: < 2.2e-16
  
  intercept = 1.015e+03 + 538.38135
  slope = 3.939e-02 + 0.02089
  error = slope*predicted_value + intercept
  return(error)}

df <- df_Q_bogs_streams_XYTableToPoint_TableToExcel <- read_excel("2_gis/df_Q_bogs_streams_XYTableToPoint_extract_Q_C.xls") %>% 
  filter(OBJECTID!="20") %>%
  separate(system_type,c("system",'type')) 


# Compare D8 vs obs to get uncertainty
m1 <- lm(Q_m3d~0+Q_m3d_D8gwebf,data = df)
m1 %>% summary
rmse1 <- sqrt(mean(m1$residuals^2))

m1 <- lm(Q_m3d~0+Q_m3d_D8gwebf,data = df %>% filter(system == "Cranberry"))
m1 %>% summary
m1 %>% plot()
rmse1 <- sqrt(mean(m1$residuals^2))
m1$residuals %>% shapiro.test()
m2 <- lm(Q_m3d~0+Q_m3d_D8gwebf+source,data = df)
m2 %>% summary
rmse1 <- sqrt(mean(m1$residuals^2))

car::Anova(m2)

# Compare D8 vs Dinf to get uncertainty
m2 <- lm(Q_m3d_Dinfgwebf~Q_m3d_D8gwebf,data = df)
m2 %>% summary
m2 %>% plot()
rmse2 <- sqrt(mean(m2$residuals^2))
mD8 <-lm(Q_m3d~Q_m3d_D8gwebf,data=df) #%>% summary
mDinf <- lm(Q_m3d~Q_m3d_Dinfgwebf,data=df)# %>% summary
mMFD <- lm(Q_m3d~Q_m3d_MFDgwebf,data=df)# %>% summary
anova(mD8,mDinf,test="Chisq")

rmse1
rmse2


df_pred <- predict(m1,se.fit=T,interval="prediction",level=0.66) %>%
  cbind(m1$model)

g2  + 
  geom_line(data=df_pred, aes(x=Q_m3d_D8gwebf,y=Q_m3d_D8gwebf-rmse1),linetype="dashed")+
  geom_line(data=df_pred, aes(x=Q_m3d_D8gwebf,y=Q_m3d_D8gwebf+rmse1),linetype="dashed")

df_pred %>% ggplot()+
  geom_line(aes(Q_m3d_D8gwebf,fit.fit))+
  geom_line(aes(Q_m3d_D8gwebf,fit.fit+rmse1),col="red")+
  geom_line(aes(Q_m3d_D8gwebf,fit.fit-rmse1),col="red")+
  geom_line(aes(Q_m3d_D8gwebf,fit.lwr))+
  geom_line(aes(Q_m3d_D8gwebf,fit.upr))+
  geom_point(aes(Q_m3d_D8gwebf,Q_m3d))
  

save.image("3_post/data/analysis_Verify_Discharge.Rdata")

