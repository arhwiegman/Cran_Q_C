# Comparison of Population density "pop" impervious cover "imp" and nitrogen concentrations


library(readxl)
df_NO3_TN_imputed_not_PSW_TableToExcel <- read_excel("2_gis/df_NO3_TN_imputed_not_PSW_domain.xls")


df_Basins_ZS <- read_excel("2_gis/Basins_ZS.xls") 
df_Basins_ZS %>% na.omit %>% nrow()

# this layer the mean of observations is calculated for each basin
df_Basins_ZS_SummarizeWithin <- read_excel("2_gis/Basins_ZS_SummarizeWithin.xls") 
df_Basins_ZS_SummarizeWithin %>% nrow()

m <- lm(lnNO3~imp,data=df_NO3_TN_imputed_not_PSW_TableToExcel)
summary(m)
plot(m)
plot(df_NO3_TN_imputed_not_PSW_TableToExcel)
library(tidyverse)


df <- df_NO3_TN_imputed_not_PSW_TableToExcel %>% 
  select_if(is.numeric) %>% 
  select(TN,NO3,NO3_TN_ratio,pop,imp) %>%
  na.omit() 

df %>% nrow()

library(rstatix)

# generate spearman correlation matrix
df %>%
  cor_mat(method="spearman") %>%
  write_csv("3_post/cor_mat_spearman_TN_NO3_pop_imp_observed.csv")

# generate p values for spearman correlation matrix
df %>%
  cor_pmat(method="spearman") %>%
  mutate(across(-1,~.<0.001)) %>%
  write_csv("3_post/cor_pmat_spearman_TN_NO3_pop_imp_observed.csv")

# generate pearson correlation matrix
df %>%
  cor_mat(method="pearson") %>%
  write_csv("3_post/cor_mat_pearson_TN_NO3_pop_imp_observed.csv")

# generate p values for pearson correlation matrix
df %>%
  cor_pmat(method="pearson") %>%
  write_csv("3_post/cor_pmat_pearson_TN_NO3_pop_imp_observed.csv")

library(GGally)
ggpairs(df)

# BASINS_ZS ========================

df <- df_Basins_ZS %>% 
  select_if(is.numeric) %>% 
  select(TN_p50_MEAN,NO3_p50_MEAN,NO3_TN_ratio_p50_MEAN,pop_dens,imp_dens) %>%
  filter(!is.na(pop_dens))

ggpairs(df)

df %>% nrow()

library(rstatix)

# generate spearman correlation matrix
df %>%
  cor_mat(method="spearman") %>%
  write_csv("3_post/cor_mat_spearman_TN_NO3_pop_imp_Basins_ZS.csv")

# generate p values for spearman correlation matrix
df %>%
  cor_pmat(method="spearman") %>% 
  write_csv("3_post/cor_pmat_spearman_TN_NO3_pop_imp_Basins_ZS.csv")

df %>%
  cor_pmat

ggpairs(df)
