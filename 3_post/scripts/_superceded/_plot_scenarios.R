# MAKE SORTED COLUMN PLOT FOR EMBAYMENT LOADS
theme_set(theme_bw())

df <- df_EMBAY_merged_summary %>% filter(!is.na(bog_count))
df %>% write_csv("df75.csv")

df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
  geom_col(aes(x=bogshed_area_ha/marg_bogshed_area_ha))+
  labs(y=element_blank(),title="Bogshed Overlap",x="(sum of total bogshed areas)/(sum of marginal bogshed ares)")

df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
  geom_col(aes(x=bog_count))+
  labs(y=element_blank())

df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
  geom_col(aes(x=marg_TN_Load_kgN_y))+
  geom_point(aes(x=TN_Load_kgN_y),col="blue")+
  geom_point(aes(x=TN_Load_kgN_y_MEP),col="red")


df %>% ggplot(aes(x=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
  geom_text(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y),
                x=100*TN_removal_kgN_y_1/TN_Load_kgN_y,label="a1"),pch=20,col="blue")+
  geom_text(aes(y=EMBAY_NAME,
                x=100*TN_removal_kgN_y_2/TN_Load_kgN_y,label="a2"),pch=20,col="red")+
  geom_text(aes(y=EMBAY_NAME,
                x=100*TN_removal_kgN_y_3/TN_Load_kgN_y,label="a3"),pch=20,col="green")+
  geom_text(aes(y=EMBAY_NAME,
                 x=100*TN_removal_kgN_y_1_MEP/TN_Load_kgN_y_MEP,label="b1"),pch=21,col="purple")+
  geom_text(aes(y=EMBAY_NAME,
                 x=100*TN_removal_kgN_y_2_MEP/TN_Load_kgN_y_MEP,label="b2"),pch=20,col="orange")

df %>% ggplot(aes(x=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
  geom_point(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y),
                x=TN_removal_kgN_y_1,label="a1"),pch=16,col="blue")+
  geom_point(aes(y=EMBAY_NAME,
                x=TN_removal_kgN_y_2,label="a2"),pch=17,col="red")+
  geom_point(aes(y=EMBAY_NAME,
                x=TN_removal_kgN_y_3,label="a3"),pch=18,col="green")+
  geom_point(aes(y=EMBAY_NAME,
                x=TN_removal_kgN_y_1_MEP,label="b1"),pch=1,col="purple")+
  geom_point(aes(y=EMBAY_NAME,
                x=TN_removal_kgN_y_2_MEP,label="b2"),pch=2,col="orange")+
  scale_x_continuous(trans='log10')


df %>% ggplot(aes(x=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
  geom_linerange(aes(xmin=marg_TN_removal_kgN_y_2_MEP,xmax=marg_TN_removal_kgN_y_1_MEP))
  scale_x_continuous(trans='log10')
  
