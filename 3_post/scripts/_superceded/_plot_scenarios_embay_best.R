# Meta Data ============================
# MAKE SORTED COLUMN PLOT FOR EMBAYMENT LOADS
# this script plots data for embayment level cranberry load
# and load reduction scenarios


# clear objects from environment 
rm(list=ls()) 

# make sure pacman and tidyverse are installed
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all") # detach all non-base R packages
pacman::p_load(tidyverse) # load tidyverse

# type package names in the string below, use comma as separator
package_names <- "
readxl,
scales,
" 

package_names |>
  str_split_1(",") |> 
  str_extract("\\w+") |>
  (\(x) x[!is.na(x)])() |>
  (\(x) try(pacman::p_load(char=x,character.only=T),F))()

# load data
theme_set(theme_bw())
theme_update(panel.grid.minor=element_blank(),
             axis.text.x = element_text(color = "black",size=12),
             axis.text.y = element_text(color = "black",size=12))

load("3_post/data/1_model_loads.Rdata")


# this is the retire and restore only the best bog scenario
df <- df_EMBAY_bog_summary %>% 
  filter(!is.na(bog_count)) %>% 
  filter(!is.na(EMBAY_NAME))

# this is the 100% retirement and restoration scenario
df2 <-  df_EMBAY_merged_summary %>% filter(!is.na(bog_count))

# all bogs not aggregated by embayment
df3 <- df_bogs_processed

# prep dataframes
df. <- df %>% select(EMBAY_NAME,
                     embFID,
                     embayshed_area_ha,
                     bogshed_area_ha,
                     bogsurf_area_ha,
                     bogsurf_perim_m2,
                     HLR_md,
                     Q_m3d,
                     LR_kghay_MEP,
                     TN_Load_kgN_y,
                     marg_TN_Load_kgN_y,
                     TN_Load_kgN_y_MEP,
                     marg_TN_Load_kgN_y_MEP) %>% 
  inner_join(df2 %>% select(EMBAY_NAME,
                            embayshed_area_ha,
                            marg_bogshed_area_ha,
                            total_bogsurf_area_ha = bogsurf_area_ha,
                            total_TN_Load_kgN_y=TN_Load_kgN_y,
                            total_marg_TN_Load_kgN_y=marg_TN_Load_kgN_y,
                            total_TN_Load_kgN_y_MEP=TN_Load_kgN_y_MEP,
                            TN_mgl_p50,
                            NO3_mgl_p50,
                            total_marg_TN_Load_kgN_y_MEP=
                              marg_TN_Load_kgN_y_MEP)) %>%
  mutate(
    # adjust bogshed areas for best bog to account for error from method differences
    bogshed_area_ha_raw = bogshed_area_ha,
    bogshed_area_ha = ifelse(bogshed_area_ha<marg_bogshed_area_ha,
                             bogshed_area_ha,
                             marg_bogshed_area_ha),
    pct_bogsheds_drained_by_best =  100*bogshed_area_ha/marg_bogshed_area_ha,
    pct_embayshed_drained_by_all = 100*marg_bogshed_area_ha/embayshed_area_ha,
    pct_embayshed_drained_by_best = 100*bogshed_area_ha/embayshed_area_ha,
    watershed_load = (LR_kghay_MEP*embayshed_area_ha),
    pct_load_from_fertilizer_all = 100*total_bogsurf_area_ha*45.8*(0.31*0.8+0.61*0.2)*0.65/watershed_load,
    pct_load_from_fertilizer_best = 100*bogsurf_area_ha*45.8*(0.31*0.8+0.61*0.2)*0.65/watershed_load,
    pct_load_drained_by_all_L2 = 100*total_marg_TN_Load_kgN_y_MEP/watershed_load,
    pct_load_drained_by_all_L1 = 100*total_marg_TN_Load_kgN_y/watershed_load,
    pct_load_drained_by_best_L2 =100*TN_Load_kgN_y_MEP/watershed_load,
    pct_load_drained_by_best_L1 = 100*TN_Load_kgN_y/watershed_load) %>%
  filter(embFID!=6) %>%
  filter(embFID!=29)


# QUICK SUMMARY STATS
df. %>% select(total_bogsurf_area_ha) %>% summary()
df. %>% select(embayshed_area_ha) %>% summary()
df.$pct_load_from_fertilizer_all %>% hist()

# percent load from fertilizer for S1 restore all farms
df.$pct_load_from_fertilizer_all[df.$pct_load_from_fertilizer_all < 17] %>% summary()

# make plots =========================

##########################
# PLOTS FOR MANUSCRIPT 

# percent watershed area draining to cranberry bogs -------------------------------
100*mean(df.$marg_bogshed_area_ha/df.$embayshed_area_ha)

df. %>% ggplot(aes(y=reorder(paste0(EMBAY_NAME," (",embFID,")"),embayshed_area_ha)))+
  geom_col(aes(x=embayshed_area_ha),
           fill="transparent",
           col="black")
ggsave("images/embay_vs_area_ha_label.png",width=4,height=6,dpi=600)

# Contributing area
df. %>% ggplot(aes(y=reorder(embFID,embayshed_area_ha)))+
  geom_col(aes(x=embayshed_area_ha),
           fill="transparent",
           col="black")+
  geom_col(aes(x=marg_bogshed_area_ha),
           fill="blue",
           col="black")+
  geom_col(aes(x=bogshed_area_ha),
           fill="lightblue",col="black")
ggsave("images/fig5a_embay_vs_area_best.png",width=3,height=6,dpi=600)
plotly::ggplotly()


# Plot of percent of watershed area and nitrogen loads
df. %>% ggplot(aes(y=reorder(embFID,embayshed_area_ha)))+
  geom_col(aes(x=100),
           fill="transparent",
           col="black")+
  geom_col(aes(x=pct_embayshed_drained_by_all),
           fill="black",
           col="black")+
  geom_col(aes(x=pct_embayshed_drained_by_best),
           fill="lightgrey",col="black")+
  geom_point(aes(x=pct_bogsheds_drained_by_best),
              pch=21,fill="white",stroke=1,col="black",size=3)+
  geom_point(aes(x=100*NO3_mgl_p50/TN_mgl_p50,fill=100*NO3_mgl_p50/TN_mgl_p50),
             position = position_nudge(x = 0.2,y=-.2),
             fill="black",stroke=1,size=2,pch=24,col="white")+
  xlim(c(0,100))+
  geom_point(aes(x=pct_load_from_fertilizer_all),
             position = position_nudge(y = 0.1),
             pch=22,fill="yellow",stroke=1,col="black",size=2)
ggsave("images/fig5c_embay_vs_pct_area_best.png",width=3,height=6,dpi=600)
plotly::ggplotly()

# Nitrogen loads
df. %>% ggplot(aes(y=reorder(embFID,embayshed_area_ha)))+
  geom_col(aes(x=LR_kghay_MEP*embayshed_area_ha),
           fill="transparent",
           col="black")+
  geom_col(aes(x=LR_kghay_MEP*marg_bogshed_area_ha),
           fill="darkgreen",
           col="black")+
  geom_col(aes(x=LR_kghay_MEP*bogshed_area_ha),
           fill="lightgreen",col="black")
ggsave("images/fig5b_embay_vs_load_best.png",width=3,height=6,dpi=600)
plotly::ggplotly()



#################################
# EXTRA PLOTS



my_fill <- scale_fill_gradient2(
  low = "darkgreen",
  mid = "white",
  high = "magenta",
  midpoint = 50,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
)

df. %>% ggplot(aes(y=reorder(embFID,embayshed_area_ha)))+
  geom_point(aes(x=100*TN_Load_kgN_y_MEP/total_marg_TN_Load_kgN_y_MEP),col="black")+
  xlim(c(0,100))
df. %>% ggplot(aes(y=reorder(embFID,embayshed_area_ha)))+
  geom_point(aes(x=100*bogshed_area_ha/marg_bogshed_area_ha),col="black")+
  xlim(c(0,100))
ggsave("images/embay_vs_load_best_frac_allbogs.png",width=2.5,height=6,dpi=600)


df. %>% ggplot(aes(y=reorder(EMBAY_NAME,embayshed_area_ha)))+
  geom_col(aes(x=100),
           fill="transparent",
           col="black")+
  geom_col(aes(x=100*marg_bogshed_area_ha/embayshed_area_ha),
           fill="darkred",
           col="black")+
  geom_col(aes(x=100*bogshed_area_ha/embayshed_area_ha),
           fill="pink",col="black")


# Percent of bogsheds draining through best bog. 
df. %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha)))+
  geom_col(aes(x=100),
           fill="darkred",
           col="black")+
  geom_col(aes(x=100*bogshed_area_ha/marg_bogshed_area_ha),
           fill="greenyellow",col="black")+
  labs(y=element_blank(),
       title="Percent of bogsheds draining through best bog",
       subtitle = "greenyellow = the best bog, maroon = all the bogs",
       x="(bogshed area)/(embayshed area)")
df.
df. %>% ggplot(aes(y=reorder(EMBAY_NAME,pct_embayshed_drained_by_all)))+
  geom_col(aes(x=pct_embayshed_drained_by_all),fill="darkred",col="black") + 
  geom_col(aes(x=pct_embayshed_drained_by_best),fill="greenyellow",col="black")+
  labs(y=element_blank(),title="Percent of Watershed Area Draining Through Bogs",x="(sum of marginal bogshed area)/(embayshed area)")

# loads -------------------------------------------------------------
df. %>% ggplot(aes(y=reorder(EMBAY_NAME,pct_embayshed_drained_by_all)))+
  geom_col(aes(x=LR_kghay_MEP*embayshed_area_ha),
           fill="lightcyan",
           col="black")+
  geom_col(aes(x=LR_kghay_MEP*marg_bogshed_area_ha),
           fill="darkred",col="black")+
  geom_col(aes(x=LR_kghay_MEP*bogshed_area_ha),
           fill="greenyellow",col="black")+
  geom_col(aes(x=LR_kghay_MEP*embayshed_area_ha),
           fill="transparent",
           col="black")+
  labs(y=element_blank(),
       title="Embayment Watershed Nitrogen Loads",
       subtitle="total watersehd load (lightcyan)\nintercepted by all bogs (maroon)\nintercepted by best bog (greenyellow)",
       x="(kg N y-1)")
ggsave("images/fig_load_bar_plot_best_vs_all.jpg",dpi=600,height=7,width=6)
  

df %>% ggplot(aes(y=reorder(EMBAY_NAME,bogshed_area_ha/embayshed_area_ha)))+
  geom_col(aes(x=TN_mgl_p50),fill="lightgrey",col="black")+
  geom_linerange(aes(xmin=TN_mgl_p25,xmax=TN_mgl_p75),col="black",size=2)+
  geom_col(aes(x=NO3_mgl_p50),fill="pink",col="red")+
  geom_linerange(aes(xmin=NO3_mgl_p25,xmax=NO3_mgl_p75),col="red",size=1)+
  labs(y=element_blank(),title="Nitrogen Concentrations (mg/L)")


df. %>% ggplot(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y)))+
  geom_point(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                 x=TN_Load_kgN_y),pch=1,stroke=1,col="blue")+
  geom_point(aes(x=marg_TN_Load_kgN_y),col="blue")+
  scale_x_continuous(trans='log10')


df. %>% ggplot(aes(x=reorder(EMBAY_NAME,TN_Load_kgN_y)))+
  geom_point(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                x=TN_Load_kgN_y),pch=1,stroke=1,col="blue")+
  geom_point(aes(y=EMBAY_NAME,
                x=TN_Load_kgN_y_MEP),pch=3,stroke=1,col="orange")+
  scale_x_continuous(trans='log10')

df. %>% ggplot()+
  geom_col(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                 x=bogsurf_area_ha,label="b1"),fill="lightgrey",col="black")

df. %>% ggplot()+
  geom_point(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                 x=HLR_md),col="black")+
  geom_vline(aes(xintercept=0.1),linetype="dashed")+
  scale_x_continuous(trans='log10')

df. %>% ggplot(aes(x=reorder(EMBAY_NAME,TN_Load_kgN_y)))+
  geom_point(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                 x=TN_Load_kgN_y/bogsurf_area_ha,label="a1"),pch=1,stroke=1,col="blue")+
  geom_point(aes(y=EMBAY_NAME,
                 x=TN_Load_kgN_y_MEP/bogsurf_area_ha,label="b1"),pch=3,stroke=1,,col="orange")+
  scale_x_continuous(trans='log10')

df %>% ggplot(aes(x=reorder(EMBAY_NAME,TN_Load_kgN_y)))+
  geom_text(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                x=100*TN_removal_kgN_y_1/TN_Load_kgN_y,label="a1"),pch=20,col="blue")+
  geom_text(aes(y=EMBAY_NAME,
                x=100*TN_removal_kgN_y_2/TN_Load_kgN_y,label="a2"),pch=20,col="red")+
  geom_text(aes(y=EMBAY_NAME,
                x=100*TN_removal_kgN_y_3/TN_Load_kgN_y,label="a3"),pch=20,col="green")+
  geom_text(aes(y=EMBAY_NAME,
                 x=100*TN_removal_kgN_y_1_MEP/TN_Load_kgN_y_MEP,label="b1"),pch=21,col="purple")+
  geom_text(aes(y=EMBAY_NAME,
                 x=100*TN_removal_kgN_y_2_MEP/TN_Load_kgN_y_MEP,label="b2"),pch=20,col="orange")


df %>% ggplot(aes(x=reorder(EMBAY_NAME,TN_Load_kgN_y)))+
  geom_text(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                x=100*marg_TN_removal_kgN_y_1/marg_TN_Load_kgN_y,label="a1"),pch=20,col="blue")+
  geom_text(aes(y=EMBAY_NAME,
                x=100*marg_TN_removal_kgN_y_2/marg_TN_Load_kgN_y,label="a2"),pch=20,col="red")+
  geom_text(aes(y=EMBAY_NAME,
                x=100*marg_TN_removal_kgN_y_3/marg_TN_Load_kgN_y,label="a3"),pch=20,col="green")+
  geom_text(aes(y=EMBAY_NAME,
                x=100*marg_TN_removal_kgN_y_1_MEP/marg_TN_Load_kgN_y_MEP,label="b1"),pch=21,col="purple")+
  geom_text(aes(y=EMBAY_NAME,
                x=100*marg_TN_removal_kgN_y_2_MEP/marg_TN_Load_kgN_y_MEP,label="b2"),pch=20,col="orange")

df %>% ggplot()+
  geom_point(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
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

ggplot(df)+
  geom_point(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                 x=TN_removal_kgN_y_1),
             pch=16,col="cyan",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=TN_removal_kgN_y_2),
             pch=17,col="magenta",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=TN_removal_kgN_y_3),
             pch=4,size=0.5,stroke=0.9,col="black",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=TN_removal_kgN_y_1_MEP),
             pch=1,col="blue")+
  geom_point(aes(y=EMBAY_NAME,
                 x=TN_removal_kgN_y_2_MEP),
             pch=2,col="red")

ggplot(df)+
  geom_point(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                 x=marg_TN_removal_kgN_y_1),
             pch=16,col="cyan",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_2),
             pch=17,col="magenta",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_3),
             pch=4,size=0.5,stroke=0.9,col="black",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_1_MEP),
             pch=1,col="blue")+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_2_MEP),
             pch=2,col="red")+
  scale_x_continuous(trans='log10')

ggplot(df)+
  geom_point(aes(y=reorder(EMBAY_NAME,TN_Load_kgN_y),
                 x=marg_TN_removal_kgN_y_1),
             pch=16,col="cyan",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_2),
             pch=17,col="magenta",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_3),
             pch=4,size=0.5,stroke=0.9,col="black",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_1_MEP),
             pch=1,col="blue")+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_2_MEP),
             pch=2,col="red")+
  scale_x_continuous(trans='log10')


