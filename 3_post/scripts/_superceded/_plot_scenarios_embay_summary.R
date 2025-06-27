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
ggpubr,
ggrepel
" 

package_names |>
  str_split_1(",") |> 
  str_extract("\\w+") |>
  (\(x) x[!is.na(x)])() |>
  (\(x) try(pacman::p_load(char=x,character.only=T),F))()

# MAKE SORTED COLUMN PLOT FOR EMBAYMENT LOADS
load("3_post/data/1_model_loads.Rdata")
theme_set(theme_bw())
theme_update(panel.grid.minor=element_blank(),
             axis.text.x = element_text(color = "black",size=12),
             axis.text.y = element_text(color = "black",size=12))
log_x_scale <- scale_x_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x)))
log_y_scale <- scale_y_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x)))

df <- df_EMBAY_merged_summary %>% 
  filter(!is.na(bog_count)) %>% 
  filter(EMBAY_NAME != "ButtermilkBay") %>%
  filter(EMBAY_NAME != "PocassetHarbor") %>%
  mutate(resid=(marg_TN_Load_kgN_y - marg_TN_Load_kgN_y_MEP))
df %>% write_csv("3_post/df_EMBAY_merged_summary_TN_p50.csv")

# FIGURE 4 ---------------
## L1 VS L2 sum of marginal "reach" loads for 24 embayments
library(ggrepel)
library(plotly)
set.seed(5)
lmall <- lm(marg_TN_Load_kgN_y~marg_TN_Load_kgN_y_MEP+0,
            data=df) 
lmall %>%  summary()
df[22,]
lmsub <- lm(marg_TN_Load_kgN_y~marg_TN_Load_kgN_y_MEP+0,
            data=df %>% filter(marg_TN_Load_kgN_y_MEP<10000))
lmsub %>%  summary()
lmsub$fitted.values
g <- ggplot(df,aes(x=marg_TN_Load_kgN_y_MEP,y=marg_TN_Load_kgN_y,label=embFID))+
  geom_abline(slope=1,intercept = 0)+
  geom_point(pch=21,fill="grey",alpha=0.5,size=3) 

predict(lmsub)
ggplotly()
g + 
  geom_abline(slope=0.67,intercept=0,color="blue",size=.75,
              linetype="dashed")+
  geom_text_repel(min.segment.length = 0, seed = 42,
                  box.padding=0.3,
                  bg.color = "white", # shadow color
                  bg.r = 0.2,
                  point.padding = 1,
                  #nudge_x = 1,
                  #nudge_y = 1,
                  segment.curvature = -1e-20,
                  size=5)+
  xlim(c(0,65000))+
  ylim(c(0,65000))+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())
ggsave("images/fig_L1_vs_L2_all_labs.png",height=4,width=4)

g +
  xlim(c(0,6000))+
  ylim(c(0,6000))+
  geom_abline(slope=0.67,intercept=0,color="blue",size=.75,
              linetype="dashed")+
  geom_text_repel(min.segment.length = 0,
                  seed = 42, 
                  box.padding = 1,
                  bg.color = "white", # shadow color
                  bg.r = 0.2,
                  point.padding = 0.5, 
                  nudge_x = .3,
                  nudge_y = .5,
                  segment.curvature = -1e-20,
                  size=6)+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())
ggsave("images/fig_L1_vs_L2_lt6000.png",height=3,width=3)
ggplotly()

# MODEL LOAD REDUCTIONS 

dL1 <- df %>% 
  mutate(
    method = "L1",
    L_pct = 100*marg_TN_Load_kgN_y/(LR_kghay_MEP*embayshed_area_ha),
    #L_pctR_cheng = 100*marg_TN_removal_kgN_y_1/(LR_kghay_MEP*embayshed_area_ha),
    L_pctR_p05 = L_pct*0.05,
    L_pctR_p25 = L_pct*0.23,
    L_pctR_p50 = L_pct*0.38,
    L_pctR_p75 = L_pct*0.53,
    L_pctR_p95 = L_pct*0.81)

dL2 <- df %>% 
  mutate(
    method = "L2",
    L_pct = 100*marg_TN_Load_kgN_y_MEP/(LR_kghay_MEP*embayshed_area_ha),
    #L_pctR_cheng = 100*marg_TN_removal_kgN_y_1_MEP/(LR_kghay_MEP*embayshed_area_ha),
    L_pctR_p05 = L_pct*0.05,
    L_pctR_p25 = L_pct*0.23,
    L_pctR_p50 = L_pct*0.38,
    L_pctR_p75 = L_pct*0.53,
    L_pctR_p95 = L_pct*0.81)

df_L1L2 <- rbind(dL2,dL1)


# FIGURE 6 ------------
my_fills <- scale_fill_manual(values=c("magenta","darkblue"))
my_colors <- scale_color_manual(values=c("magenta","darkblue"))
g_b = ggplot(df_L1L2 %>% filter(embFID!=6) %>% filter(embFID!=29),
           aes(x=reorder(paste0(EMBAY_NAME," (",embFID,")"),marg_bogshed_area_ha/embayshed_area_ha),
                       y=L_pctR_p50,
                       color=method))+
  geom_linerange(aes(ymin=L_pctR_p25,ymax=L_pctR_p75),
                 position = position_dodge2(preserve = "single",width=0.4),
                 linewidth=2)+
  geom_linerange(aes(ymin=L_pctR_p05,ymax=L_pctR_p95),
                 position = position_dodge2(preserve = "single",width=0.4))+
  geom_point(aes(size=method),fill="white",
             position = position_dodge2(preserve = "single",width=0.4),
             pch=21,stroke=1.5,size=2)+
  geom_hline(aes(yintercept=3),linetype="dashed")+
  geom_hline(aes(yintercept=10),linetype="dashed")+
  geom_hline(aes(yintercept=30),linetype="dashed")+
  my_colors
g_b + ylim(0,65) + ggpubr::rotate()
ggsave("images/fig6_b_full.png",height=6,width=8)
g_b + ylim(0,175)+ ggpubr::rotate()
ggsave("images/fig6_b_200.png",height=6,width=8)



# APPENDIX UNUSED CODE

# my_fills <- scale_fill_manual(values=c("magenta","darkblue"))
# my_colors <- scale_color_manual(values=c("magenta","darkblue"))
# g_L1 = ggplot(dL1 %>% filter(embFID!=6) %>% filter(embFID!=29),
#              aes(x=reorder(paste0(EMBAY_NAME,"   (",embFID,")"),embayshed_area_ha),
#                  y=L_pctR_p50))+
#   #geom_linerange(aes(ymin=L_pctR_p25,ymax=L_pctR_p75),
#   #               position = position_dodge2(preserve = "single",width=0.4),
#   #               linewidth=2)+
#   #geom_linerange(aes(ymin=L_pctR_p05,ymax=L_pctR_p95),
#   #               position = position_dodge2(preserve = "single",width=0.4))+
#   geom_point(aes(size=method),fill="white",
#              position = position_dodge2(preserve = "single",width=0.4),
#              pch=21,stroke=1.5,size=2)+
#   geom_hline(aes(yintercept=2),linetype="dashed")+
#   geom_hline(aes(yintercept=5),linetype="dashed")+
#   geom_hline(aes(yintercept=10),linetype="dashed")
# g_L1 + ylim(0,30) + ggpubr::rotate()
# ggsave("images/fig6_L1_full.png",height=6,width=8)
# g_L1 + ylim(0,175)+ ggpubr::rotate()
# ggsave("images/fig6_L1_200.png",height=6,width=8)


# df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha)))+
#   geom_col(aes(x=100*marg_bogshed_area_ha/embayshed_area_ha),fill="lightgrey",col="black")+
#   labs(y=element_blank(),title="Percent of Watershed Area Draining Through Bogs",x="(sum of marginal bogshed area)/(embayshed area)")
# 
# df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha)))+
#   geom_col(aes(x=bogshed_area_ha/marg_bogshed_area_ha),fill="lightgrey",col="black")+
#   labs(y=element_blank(),title="Bogshed Overlap",x="(sum of total bogshed areas)/(sum of marginal bogshed ares)")
# 
# 
# df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha)))+
#   geom_col(aes(x=bog_count))+
#   labs(y=element_blank())
# 
# df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha)))+
#   geom_col(aes(x=marg_TN_Load_kgN_y))+
#   geom_point(aes(x=TN_Load_kgN_y),col="blue")+
#   geom_point(aes(x=TN_Load_kgN_y_MEP),col="red")
# 
# 
# df %>% filter(TN_Load_kgN_y_MEP>0) %>% ggplot()+
#   geom_point(aes(y=marg_TN_Load_kgN_y,x=TN_Load_kgN_y_MEP))+
#   geom_abline(slope=1,intercept = 0)+
#   lims(y=c(0,250000),x=c(0,250000))
# 
# df %>% filter(TN_Load_kgN_y_MEP>0) %>% ggplot()+
#   geom_point(aes(y=TN_Load_kgN_y,x=TN_Load_kgN_y_MEP))+
#   geom_abline(slope=1,intercept = 0)+
#   lims(y=c(0,250000),x=c(0,250000))
# 
# df %>% filter(TN_Load_kgN_y_MEP>0) %>% ggplot()+
#   geom_point(aes(y=marg_TN_Load_kgN_y,x=TN_Load_kgN_y_MEP))+
#   geom_abline(slope=1,intercept = 0)
# 
# df %>% filter(TN_Load_kgN_y_MEP>0) %>% ggplot()+
#   geom_point(aes(y=marg_TN_Load_kgN_y,x=marg_TN_Load_kgN_y_MEP))+
#   geom_abline(slope=1,intercept = 0)
# 
# 
# 
# # df %>% ggplot(aes(x=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
# #   geom_point(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y),
# #                  x=marg_TN_removal_kgN_y_1/marg_TN_Load_kgN_y,label="a1"),pch=16,col="blue")+
# #   geom_point(aes(y=EMBAY_NAME,
# #                  x=marg_TN_removal_kgN_y_2/marg_TN_Load_kgN_y,label="a2"),pch=17,col="red")+
# #   geom_point(aes(y=EMBAY_NAME,
# #                  x=marg_TN_removal_kgN_y_3/marg_TN_Load_kgN_y,label="a3"),pch=18,col="green")+
# #   geom_point(aes(y=EMBAY_NAME,
# #                  x=marg_TN_removal_kgN_y_1_MEP/marg_TN_Load_kgN_y_MEP,label="b1"),pch=1,col="blue")+
# #   geom_point(aes(y=EMBAY_NAME,
# #                  x=marg_TN_removal_kgN_y_2_MEP/marg_TN_Load_kgN_y_MEP,label="b2"),pch=2,col="red")
# # 
# # df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
# #   geom_point(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y),
# #                 x=marg_TN_removal_kgN_y_1,label="a1"),pch=16,col="blue")+
# #   geom_point(aes(y=EMBAY_NAME,
# #                 x=marg_TN_removal_kgN_y_2,label="a2"),pch=17,col="red")+
# #   geom_point(aes(y=EMBAY_NAME,
# #                 x=marg_TN_removal_kgN_y_3,label="a3"),pch=18,col="green")+
# #   geom_point(aes(y=EMBAY_NAME,
# #                 x=marg_TN_removal_kgN_y_1_MEP,label="b1"),pch=1,col="blue")+
# #   geom_point(aes(y=EMBAY_NAME,
# #                 x=marg_TN_removal_kgN_y_2_MEP,label="b2"),pch=2,col="red")+
# #   log_x_scale+
# #   theme(panel.grid.minor = element_blank())+
# #   annotation_logticks(sides="tb")
# 
# ggplot(df)+
#   geom_col(aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha),
#                x=100*marg_bogshed_area_ha/embayshed_area_ha),fill="grey90",color="black")+
#   geom_col(aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha),
#                x=100*marg_bogshed_area_ha/embayshed_area_ha),fill="grey90",color="black")+
#   xlim(c(0,100))
# #ggsave("plots_fig5_a")
# 
# g <-df %>% filter(embFID!=6) %>% filter(embFID!=29) %>%
#   ggplot(aes(x=reorder(paste0(EMBAY_NAME," (",embFID,")"),marg_bogshed_area_ha/embayshed_area_ha)))
# g_a <- g +
#   geom_col(aes(y=LR_kghay_MEP*embayshed_area_ha),fill="grey90",color="black")+
#   geom_point(aes(y=marg_TN_Load_kgN_y),
#              pch=21,col="black",fill="magenta",size=3,alpha=0.75)+
#   geom_point(aes(y=marg_TN_Load_kgN_y_MEP),
#              pch=23,col="white",fill="darkblue",size=2,stroke=1,alpha=0.75)
# #ggsave("plots_fig6_a")
# 
# g_a <- g+
#   geom_point(aes(y=100*marg_TN_Load_kgN_y/(LR_kghay_MEP*embayshed_area_ha)),
#              pch=21,col="black",fill="magenta",size=3,alpha=0.75)+
#   geom_point(aes(y=100*marg_TN_Load_kgN_y_MEP/(LR_kghay_MEP*embayshed_area_ha)),
#              pch=23,col="white",fill="darkblue",size=2,stroke=1,alpha=0.75)
# g_a + ylim(0,80)+ ggpubr::rotate()
# ggsave("images/fig6_a_0t80.png",height=6,width=5)
# g_a + ylim(0,220)+ ggpubr::rotate()
# ggsave("images/fig6_a_220.png",height=6,width=8)


# ggplot(df)+
#   geom_point(aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha),
#                  x=100*marg_TN_removal_kgN_y_1/(LR_kghay_MEP*embayshed_area_ha)),
#              pch=16,col="magenta",alpha=0.75)+
#   geom_point(aes(y=EMBAY_NAME,
#                  x=100*marg_TN_removal_kgN_y_2/(LR_kghay_MEP*embayshed_area_ha)),
#              pch=17,col="magenta",alpha=0.75)+
#   geom_point(aes(y=EMBAY_NAME,
#                  x=100*marg_TN_removal_kgN_y_3/(LR_kghay_MEP*embayshed_area_ha)),
#              pch=4,stroke=1.1,col="magenta",alpha=0.75)+
#   geom_point(aes(y=EMBAY_NAME,
#                  x=100*marg_TN_removal_kgN_y_1_MEP/(LR_kghay_MEP*embayshed_area_ha)),
#              pch=1,col="darkblue")+
#   geom_point(aes(y=EMBAY_NAME,
#                  x=100*marg_TN_removal_kgN_y_2_MEP/(LR_kghay_MEP*embayshed_area_ha)),
#              pch=2,col="darkblue")+
#   geom_point(aes(y=EMBAY_NAME,
#                  x=100*marg_TN_removal_kgN_y_3_MEP/(LR_kghay_MEP*embayshed_area_ha)),
#              pch=4,stroke=1,col="darkblue",alpha=0.75)+
#   
#   xlim(c(0,100))

# df_LW$RE_pct %>% quantile(c(5,25,50,75,95)/100)
# 5%   25%   50%   75%   95% 
# 5.17 23.37 38.30 53.00 81.49 





gL1 <- ggplot(d.,aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha)))+
  geom_linerange(aes(xmin=L1_pctR_p25,xmax=L1_pctR_p75),col="magenta",linewidth=2)+
  geom_linerange(aes(xmin=L1_pctR_p05,xmax=L1_pctR_p95),col="magenta",linewidth=1)+
  geom_point(aes(x=L1_pctR_p50),
             pch=21,fill="white",col="magenta",alpha=0.75,size=3)
gL2 <- ggplot(d.,aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha)))+
  geom_linerange(aes(xmin=L2_pctR_p25,xmax=L2_pctR_p75),col="darkblue",linewidth=2)+
  geom_linerange(aes(xmin=L2_pctR_p05,xmax=L2_pctR_p95),col="darkblue",linewidth=1)+
  geom_point(aes(x=L2_pctR_p50),
             pch=21,fill="white",col="darkblue",alpha=0.75,size=3)+
  theme_classic()

ggplot(df_L1L2,aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha),
                   color=method),alpha=0.75)+
  geom_linerange(aes(xmin=L_pctR_p25,xmax=L_pctR_p75),linewidth=2)+
  geom_linerange(aes(xmin=L_pctR_p05,xmax=L_pctR_p95),linewidth=1)+
  geom_point(aes(x=L_pctR_p50),
             pch=21,fill="white",alpha=0.75,size=3)



gL2 <- ggplot(d.,aes(y=reorder(EMBAY_NAME,marg_bogshed_area_ha/embayshed_area_ha)))+
  geom_linerange(aes(xmin=L2_pctR_p25,xmax=L2_pctR_p75),col="darkblue",linewidth=2)+
  geom_linerange(aes(xmin=L2_pctR_p05,xmax=L2_pctR_p95),col="darkblue",linewidth=1)+
  geom_point(aes(x=L2_pctR_p50),
             pch=21,fill="white",col="darkblue",alpha=0.75,size=3)+
  theme_classic()
gL1+geom_linerange(aes(xmin=L2_pctR_p25,xmax=L2_pctR_p75),col="darkblue",linewidth=2)+
  geom_linerange(aes(xmin=L2_pctR_p05,xmax=L2_pctR_p95),col="darkblue",linewidth=1)+
  geom_point(aes(x=L2_pctR_p50),
             pch=21,fill="white",col="darkblue",alpha=0.75,size=3)+
  theme_classic()+
  position_dodge2(width = NULL, preserve = "total")
ggsave("")

ggplot(df)+
  geom_point(aes(y=reorder(EMBAY_NAME,marg_TN_removal_kgN_y_3/(LR_kghay_MEP*embayshed_area_ha)),
                 x=marg_TN_removal_kgN_y_1),
             pch=16,col="magenta",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_2),
             pch=17,col="magenta",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=marg_TN_removal_kgN_y_3/(LR_kghay_MEP*embayshed_area_ha)),
             pch=4,stroke=1.1,col="magenta",alpha=0.75)+
  geom_point(aes(y=EMBAY_NAME,
                 x=100*marg_TN_removal_kgN_y_1_MEP/(LR_kghay_MEP*embayshed_area_ha)),
             pch=1,col="darkblue")+
  geom_point(aes(y=EMBAY_NAME,
                 x=100*marg_TN_removal_kgN_y_2_MEP/(LR_kghay_MEP*embayshed_area_ha)),
             pch=2,col="darkblue")+
  geom_point(aes(y=EMBAY_NAME,
                 x=100*marg_TN_removal_kgN_y_3_MEP/(LR_kghay_MEP*embayshed_area_ha)),
             pch=4,stroke=1,col="darkblue",alpha=0.75)+
  scale_x_log10()
  

df %>% ggplot(aes(y=reorder(EMBAY_NAME,marg_TN_Load_kgN_y)))+
  geom_linerange(aes(xmin=marg_TN_removal_kgN_y_2_MEP,xmax=marg_TN_removal_kgN_y_1_MEP))
