NO3_RF_trained_TableToExcel <- read_excel("data/NO3_RF_trained_TableToExcel.xls")

df <- NO3_RF_trained_TableToExcel

ggplot(df,aes(x=NO3,y=NO3_predicted))+
  geom_point(fill="lightgrey",color="black",pch=21,alpha=0.8,size=3)+
  geom_abline(intercept=0,slope=1,linetype='dashed')+
  geom_smooth(method="lm")+
  xlim(0,4)+
  ylim(0,4)+
  theme_bw()

bog_points_predicted_NO3_RF_TableToExcel <- read_excel("data/bog_points_predicted_NO3_RF_TableToExcel.xls")

df_bogs_raw <- bog_points_predicted_NO3_RF_TableToExcel

df_bogs <- df_bogs_raw %>%
  rename(bogFID = OBJECTID,
         NO3_mgl = bayes_krig_NO3_rs_kb_250m) %>%
  select(bogFID,
         #WMA_NO,
         #OWNER,
         TOWN,
         BOG_NAME,
         CropStatus,
         bogsurf_m2,
         Q_m3d_D8gwebf,
         Q_m3d_D8gwebflt1m,
         Q_m3d_Dinfgwebf,
         Q_m3d_Dinfgwebflt1m,
         Q_m3d_LidAg10BFFlowAcc,
         NO3_mgl) %>%
  mutate(Q_bogshed_m3d = Q_m3d_Dinfgwebflt1m) %>%
  group_by(bogFID,TOWN,BOG_NAME) %>%
  summarize(across(is.numeric,mean)) %>%
  ungroup()
#%>%
#na.omit()
str(df_bogs)
save(df_bogs,file="data/df_bogs.Rdata")
