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
                  size=5)
g + 
  #xlim(c(0,5000))
  #ylim(c(0,5000))+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())
ggsave("images/fig_L1_vs_L2_all.png",height=4,width=4)

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
ggsave("images/fig_L1_vs_L2_lt9000.png",height=3,width=3)
ggplotly()

df <- df_EMBAY_bog_summary %>% 
  filter(!is.na(bog_count)) %>% 
  filter(EMBAY_NAME != "ButtermilkBay") %>%
  filter(EMBAY_NAME != "PocassetHarbor") %>%
  mutate(resid=(marg_TN_Load_kgN_y - marg_TN_Load_kgN_y_MEP))
df %>% write_csv("3_post/df_EMBAY_bog_summary_TN_p50.csv")

dL1 <- df %>% 
  mutate(
    method = "L1",
    L_pct = 100*TN_Load_kgN_y/(LR_kghay_MEP*embayshed_area_ha),
    #L_pctR_cheng = 100*TN_removal_kgN_y_1/(LR_kghay_MEP*embayshed_area_ha),
    L_pctR_p05 = L_pct*0.05,
    L_pctR_p25 = L_pct*0.23,
    L_pctR_p50 = L_pct*0.38,
    L_pctR_p75 = L_pct*0.53,
    L_pctR_p95 = L_pct*0.81)

dL2 <- df %>% 
  mutate(
    method = "L2",
    L_pct = 100*TN_Load_kgN_y_MEP/(LR_kghay_MEP*embayshed_area_ha),
    #L_pctR_cheng = 100*TN_removal_kgN_y_1_MEP/(LR_kghay_MEP*embayshed_area_ha),
    L_pctR_p05 = L_pct*0.05,
    L_pctR_p25 = L_pct*0.23,
    L_pctR_p50 = L_pct*0.38,
    L_pctR_p75 = L_pct*0.53,
    L_pctR_p95 = L_pct*0.81)

df_L1L2 <- rbind(dL2,dL1)

my_fills <- scale_fill_manual(values=c("magenta","darkblue"))
my_colors <- scale_color_manual(values=c("magenta","darkblue"))
g_b = ggplot(df_L1L2 %>% filter(embFID!=6) %>% filter(embFID!=29),
           aes(x=reorder(paste0(EMBAY_NAME," (",embFID,")"),bogshed_area_ha/embayshed_area_ha),
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
ggsave("images/fig6_b_S2_full.png",height=6,width=8)
