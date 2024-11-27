# Metadata =====================
# author: Adrian Wiegman
#
# description:
# compare concentration verses loading rate
# in this script embayment level averages 
# of predicted nitrogen concentrations at cranberry bogs
# are compared to embayment level watershed nitrogen loading rates
# this is used to verify the use of concentration predictions
# from the empirical bayesian krigging model

# Main Program ====================
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
rstatix,
GGally
" 

package_names |>
  str_split_1(",") |> 
  str_extract("\\w+") |>
  (\(x) x[!is.na(x)])() |>
  (\(x) try(pacman::p_load(char=x,character.only=T),F))()

# load data
theme_set(theme_bw())
theme_update(panel.grid.minor=element_blank())
load("3_post/data/_model_load_reduction.Rdata")


df <- df_EMBAY_bog_summary %>% filter(!is.na(bog_count))
df2 <-  df_EMBAY_merged_summary %>% filter(!is.na(bog_count))
df3 <- df_bogs_processed


# VERIFICATION OF CONCENTRATION DATA VS MEP LOADING RATES

m_TN_LR<- lm(log(TN_mgl_p50) ~ log(LR_kghay_MEP),data=df2)
m <- m_TN_LR
m %>% summary()
write_lines(m%>%summary(),file="lm_TN_f_LR_all")
png("images/lm_TN_mgl_from_LR_MEP.png",width=800,height=500)
par(mfrow=c(2,2))
m %>% plot()
par(mfrow=c(1,1))
dev.off()

# generate Pearson correlation matrix
df2%>%select(LR_kghay_MEP,TN_mgl_p50,NO3_mgl_p50) %>% 
  cor_mat(method="pearson") %>%
  tibble() 

# generate Spearman correlation matrix
df2%>%select(LR_kghay_MEP,TN_mgl_p50,NO3_mgl_p50) %>% 
  cor_mat(method="spearman") %>%
  tibble() %>%
  write_csv("3_post/cor_mat_spearman_LR_NO3_TN_df_EMBAY_merged_summary.csv")
# p-values for spearman matrix
df2%>%select(LR_kghay_MEP,TN_mgl_p50,NO3_mgl_p50) %>% 
  cor_pmat(method="pearson") %>%
  tibble() %>%
  write_csv("3_post/cor_pmat_pearson_LR_NO3_TN_df_EMBAY_merged_summary.csv")

df2%>%select(LR_kghay_MEP,TN_mgl_p50,NO3_mgl_p50) %>%
  ggpairs() + theme_bw()
df2$LR_kghay_MEP %>% min()
df2$LR_kghay_MEP %>% median()
df2$LR_kghay_MEP %>% mean()
df2$LR_kghay_MEP %>% max()
ggsave("images/ggpairs_LR_TN_NO3_pearson.png",
       width=6,height=3)

df2 %>% ggplot(aes(y=TN_mgl_p50,x=LR_kghay_MEP)) + 
  geom_point()+
  geom_errorbar(aes(ymin=TN_mgl_p25,ymax=TN_mgl_p75)) + 
  geom_smooth(method="lm")+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  labs(title="MEP Watershed N Loading Rates vs Interpolated Concentrations",
       subtitle="Aggregated to the Embayment Level (n=24)")
ggsave("images/LR_MEP_vs_TN_p50_agg_embayment.png",
       width=6,height=3)

save.image("3_post/data/compare_concentration_and_loadingrate.Rdata")
