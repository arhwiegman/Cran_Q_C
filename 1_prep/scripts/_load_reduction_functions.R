# prepare load reduction functions
# datasets:
# Walton, C. R., Zak, D., Audet, J., Petersen, R. J., Lange, J., Oehmke, C., ... & Hoffmann, C. C. (2020). Wetland buffer zones for nitrogen and phosphorus retention: Impacts of soil type, hydrology and vegetation. Science of the Total Environment, 727, 138709. https://doi.org/10.1016/j.scitotenv.2020.138709
# Land, M., Granéli, W., Grimvall, A. et al. How effective are created or restored freshwater wetlands for nitrogen and phosphorus removal? A systematic review. Environ Evid 5, 9 (2016). https://doi.org/10.1186/s13750-016-0060-0

# SET UP ENVIRONMENT -----------------

# clear objects from environment 
rm(list=ls()) 

# make sure pacman and tidyverse are installed
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all") # detach all non-base R packages
pacman::p_load(tidyverse) # load tidyverse

# type package names in the string below, use comma as separator
package_names <- "
car,
caret,
GGally,
" 

package_names |>
   str_split_1(",") |> 
   str_extract("\\w+") |>
   (\(x) x[!is.na(x)])() |>
   (\(x) try(pacman::p_load(char=x,character.only=T),F))()

theme_set(theme_bw())

# LOAD DATA ----------------------
walton_2020_all <- readxl::read_xlsx("0_raw/walton_2020_1-s2.0-S0048969720322269-mmc1.xlsx",
                                     sheet="sites and nutrient fluxes  (NA)") %>%
  # filter out precipitation fed systems
  filter(`Main source/flow path in`!="Precipitation") %>%
  mutate(HLR_my = `HLR [mm]`/1000,
         HLR_md = `HLR [mm]`/1000/365.25,
         Q_m3y = `Qin [m3/yr]`,
         Q_m3d = Q_m3y/365.25,
         A_m2 = `Area [m2]`,
         RR_kgNhay = `Retention (kg N, P/ha/yr)`,
         LR_kgNhay = `Load (kg N, P/ha/yr)`,
         C_mgl = 1000 * LR_kgNhay / 10000 * A_m2 / Q_m3y,
         RE_pct = as.numeric(`Efficiency (%)`)) %>% 
  filter(RE_pct > 0) %>%
  filter(LR_kgNhay > 0)

land_2016_TN <- read_csv("0_raw/land_2016_TN.csv",
                         na = c("", "NA","#N/A"),) %>%
  mutate(LR_kgNhay = `TN loading rate (g/m2/yr)`*10000/1000,
         RR_kgNhay = `TN removal rate (g/m2/yr)`*10000/1000,
         RE_pct = `TN removal efficieny (%)`,
         TN_mgl = `TN in (mg/l)`,
         HLR_md = `HLR (m/yr)`) %>%
  # filter out precipitation fed systems
  filter(`Water regime` != "Precipitation-driven",
         `Wetland type`== "Free Water Surface",
         #!str_detect(`Inflow type`,"Waste|waste"), 
         Lat >= 30,
         Lat < 60)
# 
# walton_2020_NO3 <- walton_2020_all %>% 
#   # select nitrate
#   filter(Nutrient == "nitrate")
# 
# walton_2020_NO3 %>% lm(data=.,`Efficiency (%)`~HLR_md) %>% summary
# walton_2020_NO3 %>% select(RE_pct,HLR_my,A_m2,LR_kgNhay,Q_m3y,C_mgl) %>% plot
# 
# walton_2020_NO3 %>% lm(data=.,RE_pct~LR_kgNhay) %>% summary
# walton_2020_NO3 %>% lm(data=.,RE_pct~log(LR_kgNhay)) 
# m <- walton_2020_NO3 %>% lm(data=.,RE_pct~log10(LR_kgNhay))
# m %>% summary()
# m %>% plot()

## Combine TN datasets

df_L <- land_2016_TN %>% 
  select(RE_pct,LR_kgNhay,RR_kgNhay,TN_mgl,HLR_md,area_m2 = `Area (m2)`,
         Reference = ID_label,
         `Wetland type`,
         `Inflow type`,
         `Water regime`,
         `Air temp (oC)`,
         `Prec. (mm/yr)`) %>% 
  filter(RE_pct > 0,
         !is.na(RE_pct),
         !is.na(HLR_md),
         !is.na(TN_mgl),
         !is.na(area_m2),
         !is.na(LR_kgNhay)) %>%
  mutate(study="Land")
df_W <- walton_2020_all %>% 
  filter(Nutrient=="total nitrogen") %>% 
  select(RE_pct,LR_kgNhay,RR_kgNhay,HLR_md,TN_mgl = C_mgl,
         area_m2 = `Area [m2]`,
         Reference,`WBZ-type`,`Country`) %>%
  filter(RE_pct > 0,
         !is.na(RE_pct),
         !is.na(HLR_md),
         !is.na(TN_mgl),
         !is.na(area_m2),
         !is.na(LR_kgNhay)) %>%
  mutate(study = "Walton")

df_LW <- full_join(df_L,df_W) %>% 
  mutate(Q_m3d = HLR_md * area_m2) %>%
  mutate(RE_prp=RE_pct/100) %>%
  mutate(L_kgy = LR_kgNhay * area_m2/10000) %>%
  mutate(R_kgy = RR_kgNhay * area_m2/10000) %>%
  arrange(by=Reference) 

save(df_LW,file="1_prep/data/df_LW.Rdata")
write_csv(df_LW,"1_prep/data/df_LandWaltlon.csv")
df <- df_LW %>% 
  select(HLR_md,area_m2,LR_kgNhay,Q_m3d,L_kgy,R_kgy,RE_prp) %>% 
  mutate(across(-RE_prp,log10))
df %>%  ggpairs(alpha=0.6)
## MAKE EXPLORATORY PLOTS --------------------------
psych::describeBy(df_LW~study)
df_LW$RE_pct %>% quantile(c(0.05,.25,0.5,.75,.95))

# 2024-07-10
df_LW %>% lm(RE_pct~log10(HLR_md)+study,data=.) %>% summary()
m1 <- df_LW %>% lm(log10(RR_kgNhay)~log10(area_m2)+log10(LR_kgNhay)+log10(HLR_md)+0,data=.)
m1 %>% summary()
m1 %>% plot

df_LW %>%
  ggplot(aes(y=RE_pct,x=log10(HLR_md)))+
  geom_smooth(method="lm",aes(color=study),linetype="dashed")+
  geom_point(aes(color=study,shape=study,size=LR_kgNhay))

df_LW %>%
  ggplot(aes(y=log(RR_kgNhay),x=log(LR_kgNhay)))+
  geom_smooth(method="lm",aes(color=study),linetype="dashed")+
  geom_point(aes(color=study,shape=study,size=log10(area_m2)))

df_LW %>% filter(study=="Land") %>%
  select(HLR_md,TN_mgl,area_m2,LR_kgNhay,RR_kgNhay,L_kgy,RE_pct) %>%
  ggpairs()
  
df_LW %>% filter(study=="Walton") %>%
  select(HLR_md,TN_mgl,area_m2,LR_kgNhay,RR_kgNhay,L_kgy,RE_pct) %>%
    ggpairs()

df_LW %>%
  select(HLR_md,TN_mgl,area_m2,LR_kgNhay,RR_kgNhay,L_kgy,RE_pct,study) %>%
  mutate(across(is.numeric,log10)) %>%
  #mutate(RE_pct = 100-RE_pct)%>%
  ggpairs(aes(color=study,alpha=0.6))

ggplot(df_LW) + 
  geom_point(aes(log(HLR_md*area_m2),log(RR_kgNhay*area_m2)))+
  geom_point(aes(log(HLR_md*area_m2),log(LR_kgNhay*area_m2)),pch=0)
ggplot(df_LW) + 
  geom_point(aes(log(L_kgy),log(R_kgy)))
ggplot(df_LW) + 
  geom_point(aes(log(L_kgy),log(R_kgy),color=log(HLR_md)))+
  geom_abline(aes(intercept=0,slope=1))

m <- lm(log(R_kgy)~log(L_kgy),
         data=df_LW) 
mq1 <- quantreg::rq(log(R_kgy)~log(L_kgy),tau=0.05,
        data=df_LW) 
mq2 <- quantreg::rq(log(R_kgy)~log(L_kgy),tau=0.25,
                   data=df_LW)
mq3 <- quantreg::rq(log(R_kgy)~log(L_kgy),tau=0.95,
                   data=df_LW)
mq2 %>% summary()
plot(log(R_kgy)~log(L_kgy),data=df_LW)
mq %>% plot()
abline(m,col="black",lty=2)
abline(mq1,col="red")
abline(mq2,col="green")
abline(mq3,col="blue")
points(log(mq3$model$L_kgy),log(mq3$fitted.values),col="red")
m <- lm(R_kgy~0+L_kgy,
        data=df_LW) 
m %>% summary()
mq1 <- quantreg::rq(R_kgy~0+L_kgy,tau=0.25,
                    data=df_LW) 
mq2 <- quantreg::rq(R_kgy~0+L_kgy,tau=0.5,
                    data=df_LW)
mq3 <- quantreg::rq(R_kgy~0+L_kgy,tau=0.75,
                    data=df_LW)
mq3 %>% summary()
mq2 %>% summary()
mq1 %>% summary()
plot(R_kgy~L_kgy,data=df_LW)
abline(m,col="black",lty=2)
abline(mq1,col="red")
abline(mq2,col="green")
abline(mq3,col="blue")

plot(RE_pct~log(L_kgy),data=df_LW)
mb <- betareg::betareg(I(RE_pct/100)~log(area_m2),data=df_LW)
plot(mb)
mb <- betareg::betareg(I(RE_pct/100)~log(area_m2),data=df_LW)
mb <- betareg::betareg(I(RE_pct/100)~log(area_m2),data=df_LW)

mb <- betareg::betareg(I(RE_pct/100)~log(HLR_md)*log(area_m2),
                       data=df_LW)
mb %>% summary
mb %>% AIC()
plot(I(RE_pct/100)~log(area_m2),data=df_LW)
plot(I(RE_pct/100)~log(HLR_md)*log(area_m2)),data=df_LW)
plot(I(RE_pct/100)~I(log(LR_kgNhay)),data=df_LW)
mb$model$fitted <- mb$fitted.values
plot(fitted~.,mb$model)

aggplot(df_LW,aes(LR_kgNhay,RE_pct)) +
  geom_point(aes(color=study,shape=study))+
  geom_smooth(method="lm",formula=y~x)+
  scale_x_log10()

ggplot(df_LW,aes(log(LR_kgNhay),RE_pct)) +
  geom_point(aes(color=study,shape=study))+
  geom_smooth(method="lm",formula=y~x)

ggplot(df_LW,aes(LR_kgNhay*area_m2/10000,RE_pct)) +
  geom_point(aes(color=study,shape=study))+
  geom_smooth(method="lm",formula=y~x)+
  scale_x_log10()

ggplot(df_LW,aes(LR_kgNhay*area_m2/10000,RE_pct)) +
  geom_point(aes(color=study,shape=study))+
  geom_smooth(method="lm",formula=y~x)+
  scale_x_log10()+
  scale_y_sqrt()

ggplot(df_LW,aes(TN_mgl*HLR_md,RE_pct)) +
  geom_point(aes(color=study,shape=study))+
  geom_smooth(method="lm",formula=y~x)+
  scale_x_log10()+
  scale_y_sqrt()

ggplot(df_LW,aes(LR_kgNhay,RE_pct,color=study))+
  geom_point(aes(shape=study))+
  geom_smooth(method="lm",formula=y~x)+
  #scale_x_log10()
  scale_y_log10()

ggplot(df_LW,aes(log(area_m2),RE_pct,color=study,shape=study)) +
  geom_point()+
  geom_smooth(method="lm",formula=y~x)


## FIT MODELS ---------------------------------------

### dependancies -----
require(caret)
require(lmtest)
require(bestglm)
require(betareg)
require(fitdistrplus)
require(logspline)

### subroutines ----
# subroutine for exploring probability distributions
sub_univariate_analysis <- expression({
  require(fitdistrplus)
  require(logspline)
  y %>% summary %>% print
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 16.00   28.27   40.22   42.78   57.14   86.97 
  y %>% shapiro.test  %>% print 
  # Shapiro-Wilk normality test
  # 
  # data:  .
  # W = 0.95124, p-value = 0.06119
  # null hypothesis, that data are normal cannot be rejected.
  # examine kurtosis vs skewness of variable compared to other distributions
  y %>% descdist(discrete = FALSE) %>% print
  
  # fit norm distribution
  y.norm <- fitdist(y,"norm")
  plot(y.norm)
  
  # fit a log-norm distribution (y must be between zero and inf)
  y.lnorm <- fitdist(y,"lnorm")
  plot(y.lnorm)
  
  # fit a log-norm distribution (y must be between zero and inf)
  y.gamma<- fitdist(y,"gamma")
  plot(y.gamma)
  
  # fit a beta distribution (y must be between zero and 1)
  y.beta <- fitdist(y,"beta")
  plot(y.beta)
  # the beta distribution is near perfect fit to the data
  
  # fit a beta distribution (y must be between zero and 1)
  y.binom <- fitdist(y,"binom")
  plot(y.binom)
  # the beta distribution is near perfect fit to the data
  
  # compare AIC for distributions
  cat("AIC of beta:",y.beta$aic,"\n")
  cat("AIC of norm:",y.norm$aic,"\n")
  cat("AIC of lnorm:",y.lnorm$aic,"\n")
  cat("AIC of gamma:",y.gamma$aic,"\n")
  # its not even close the data is beta distributed.
})

sub_model_summary_stats <- expression({
  m %>% summary %>% print
  # Call:
  #   NULL
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  5.50273    0.48796  11.277  5.4e-14 ***
  #   HLR_md      -0.39990    0.11675  -3.425 0.001432 ** 
  #   LR_kgNhay   -0.54614    0.11257  -4.852  1.9e-05 ***
  #   area_m2     -0.11095    0.02667  -4.160 0.000164 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # (Dispersion parameter for Gamma family taken to be 0.08380525)
  # 
  # Null deviance: 8.5975  on 43  degrees of freedom
  # Residual deviance: 3.4737  on 40  degrees of freedom
  # AIC: 345.67
  # 
  # Number of Fisher Scoring iterations: 5
  . <- (m$null.deviance - m$deviance)/m$null.deviance
  cat("Pseudo R2:",.,"\n")
  
  # Assess multicollinearity
  cat("\nVariance inflation factor: (<2 is good, <3 is okay):\n")
  m %>% vif %>% print
  
  # Diagnostics and Plots
  require(lmtest)
  bptest(m) %>% print
  cat("(p > 0.05 indicates the errors are not heteroskedastic)\n")
})

# subroutine for plotting model diagnostics
sub_model_diagnostic_plots <- expression({
  m %>% summary
  plot.new()
  par(mfrow=c(2,2))
  m %>% plot
  plot(m$fitted.values,
       m$model[,1],
       main="Obs. vs Fit",
       xlab="predicted",
       ylab="observed")
  abline(0,1,col="red")
  j = 1
  for (i in colnames(m$model)[-1]){
    j = j + 1
    plot(m$model[,i],
         m$model[,1],
         main="red = fitted, black = obs.",
         ylab="y",
         xlab=colnames(m$model)[j])
    points(m$model[,i],m$fitted.values,col="red")
  }
})

sub_find_bestglm_aic <- expression({
  best <- bestglm(df,IC="AIC",family=gaussian(link=log))
  print(best$BestModels)
  best$BestModel %>% summary
  # Pseudo R-sq: (null - model)/(null) 
  (best$BestModel$null.deviance - best$BestModel$deviance)/best$BestModel$null.deviance
  
  # save data frame of best models with formulas
  df_bestmodels <- best$BestModels %>%
    # remove last column
    dplyr::select(-Criterion) %>%
    mutate(row = row_number()) %>%
    gather(values, key, -row) %>%
    group_by(row) %>%
    mutate(formula = paste0(values[key==1], collapse = "+")) %>%
    spread(values, key) %>%
    ungroup() %>%
    rename(rank=row) %>%
    cbind(best$BestModels %>% dplyr::select(Criterion))
  print(df_bestmodels)
  
  best_formula <- formula(paste(colnames(df)[length(colnames(df))],
                                "~",df_bestmodels$formula[1]))
})

sub_fit_best_model_with_LOOCV <- expression({
  #specify the cross-validation method
  ### LEAVE ONE OUT CROSS VALIDATION
  ctrl <- trainControl(method = "LOOCV")
  #fit a regression model and use LOOCV to evaluate performance
  model <- train(best_formula, 
                 data = df, 
                 method = "glm",
                 family=gaussian(link=log), # gamma constrains y between 0 and inf
                 trControl = ctrl)
  print(model)
})

## prepare data for model -----
# Land only data
df_L_reg = df_LW %>%
  filter(study=="Land") %>%
  dplyr::select(HLR_md,TN_mgl,area_m2,LR_kgNhay,L_kgy,Q_m3d,RE_prp) %>%
  mutate(across(-RE_prp,log10)) %>% 
  as.data.frame

# Walton only data
df_W_reg = df_LW %>% 
  filter(study=="Walton") %>%
  dplyr::select(HLR_md,TN_mgl,area_m2,LR_kgNhay,L_kgy,Q_m3d,RE_prp) %>%
  mutate(across(-RE_prp,log10)) %>% 
  as.data.frame

# Land and Walton combined data
df_LW_reg = df_LW %>% 
  dplyr::select(HLR_md,TN_mgl,area_m2,LR_kgNhay,L_kgy,Q_m3d,RE_prp) %>%
  mutate(across(-RE_prp,log10)) %>% 
  as.data.frame

### LAND MODEL ========================
save_pdfs = FALSE
if (save_pdfs) pdf("LAND_MODEL.pdf")

df <- df_L_reg

df %>% ggpairs()

#### analysis of y variable -----
y = df$RE_prp

eval(sub_univariate_analysis)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 16.00   28.27   40.22   42.78   57.14   86.97 
# 
# Shapiro-Wilk normality test
# 
# data:  .
# W = 0.95124, p-value = 0.06119
# 
# summary statistics
# 
# min:  16   max:  86.96622 
# median:  40.22472 
# mean:  42.78039 
# estimated sd:  18.59712 
# estimated skewness:  0.5289265 
# estimated kurtosis:  2.638349 
# AIC of beta: -23.50189 
# AIC of norm: -20.17535 
# AIC of lnorm: -24.16344 
# AIC of gamma: -24.88138 

# beta lnorm and gamma are potential options
# lnorm is most straight forward to interpret so we will use that

# variable selection criteria do not perform well for beta regression (Espinheira et al. 2019)
# so we will search for the best linear model using AIC and LOOCV cross-validation
# then we fit the best linear model with a beta regression
# Espinheira, P. L., da Silva, L. C. M., Silva, A. D. O., & Ospina, R. (2019). Model selection criteria on beta regression for machine learning. Machine Learning and Knowledge Extraction, 1(1), 26.

#### feature selection -------------
# find the best fitting linear model based on AIC
eval(sub_find_bestglm_aic)
# rank                         formula area_m2 HLR_md LR_kgNhay TN_mgl Criterion
# 1    1        HLR_md+LR_kgNhay+area_m2    TRUE   TRUE      TRUE  FALSE    -57.36
# 2    2           HLR_md+TN_mgl+area_m2    TRUE   TRUE     FALSE   TRUE    -55.77
# 3    3 HLR_md+TN_mgl+LR_kgNhay+area_m2    TRUE   TRUE      TRUE   TRUE    -55.70
# 4    4        TN_mgl+LR_kgNhay+area_m2    TRUE  FALSE      TRUE   TRUE    -52.34
# 5    5                HLR_md+LR_kgNhay   FALSE   TRUE      TRUE  FALSE    -50.75

# if AIC is within 2% then use the model with highest CV rsq
# fit best model with cross LOOCV validation

#### fit best linear model with gamma distribution
eval(sub_fit_best_model_with_LOOCV)
# Generalized Linear Model 
# 
# 44 samples
# 3 predictor
# 
# No pre-processing
# Resampling: Leave-One-Out Cross-Validation 
# Summary of sample sizes: 43, 43, 43, 43, 43, 43, ... 
# Resampling results:
#   
#   RMSE    Rsquared  MAE  
# 0.1311  0.4946    0.102

# Tuning parameter 'intercept' was held constant at a value of TRUE
m <- model$finalModel
model_RE_prp_Land_glm_lnorm = m
save(model_RE_prp_Land_glm_lnorm,file="1_prep/data/model_RE_prp_Land_glm_lnorm.Rdata")

eval(sub_model_summary_stats)
# Call:
#   NULL
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.3807     0.4553    0.84  0.40806    
# HLR_md       -0.4348     0.1111   -3.92  0.00034 ***
#   LR_kgNhay    -0.4343     0.1060   -4.10  0.00020 ***
#   area_m2      -0.0756     0.0269   -2.81  0.00771 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.01526)
# 
# Null deviance: 1.4872  on 43  degrees of freedom
# Residual deviance: 0.6104  on 40  degrees of freedom
# AIC: -53.36
# 
# Number of Fisher Scoring iterations: 6
# 
# Pseudo R2: 0.5896 
# 
# Variance inflation factor: (<2 is good, <3 is okay):
#   HLR_md LR_kgNhay   area_m2 
# 1.322     1.585     1.327 
# 
# studentized Breusch-Pagan test
# 
# data:  m
# BP = 4.1, df = 3, p-value = 0.2
# 
# (p > 0.05 indicates the errors are not heteroskedastic)

eval(sub_model_diagnostic_plots)

mbeta <- betareg(RE_prp ~ HLR_md+LR_kgNhay+area_m2 |HLR_md + area_m2 ,
                 #link = "log",
                 data = df)

m <- mbeta
model_RE_prp_Land_betareg_log = m
save(model_RE_prp_Land_betareg_log,
     file="1_prep/data/model_RE_prp_Land_betareg_log.Rdata")
eval(sub_model_summary_stats)
# Call:
#   betareg(formula = RE_prp ~ HLR_md + LR_kgNhay + area_m2 | HLR_md + area_m2, data = df %>% mutate(RE_prp = RE_pct/100), 
#           link = "log")
# 
# Standardized weighted residuals 2:
#   Min     1Q Median     3Q    Max 
# -2.657 -0.726  0.055  0.842  2.080 
# 
# Coefficients (mean model with log link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -3.6235     0.4185   -8.66  < 2e-16 ***
#   HLR_md       -0.3595     0.1091   -3.29  0.00099 ***
#   LR_kgNhay    -0.5664     0.0977   -5.80  6.8e-09 ***
#   area_m2      -0.1009     0.0261   -3.86  0.00011 ***
#   
#   Phi coefficients (precision model with log link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    8.472      0.573   14.78   <2e-16 ***
#   HLR_md         0.809      0.508    1.59     0.11    
# area_m2        0.167      0.125    1.33     0.18    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Type of estimator: ML (maximum likelihood)
# Log-likelihood:  238 on 7 Df
# Pseudo R-squared: 0.601
# Number of iterations: 15 (BFGS) + 1 (Fisher scoring) 
# Pseudo R2:  
#   
#   Variance inflation factor: (<2 is good, <3 is okay):
#   HLR_md LR_kgNhay   area_m2 
# 1.488     1.509     1.310 
# 
# studentized Breusch-Pagan test
# 
# data:  m
# BP = 4.1, df = 3, p-value = 0.2
# 
# (p > 0.05 indicates the errors are not heteroskedastic)

eval(sub_model_diagnostic_plots)

dev.off()

### WALTON MODEL =================================
# prepare data
df = df_W_reg 

df %>% ggpairs()

y = df$RE_prp

eval(sub_univariate_analysis)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0100  0.1312  0.3395  0.3468  0.4340  0.9500 
# 
# Shapiro-Wilk normality test
# 
# data:  .
# W = 0.92887, p-value = 0.02901
# 
# summary statistics
# ------
#   min:  0.01   max:  0.95 
# median:  0.3395 
# mean:  0.3467941 
# estimated sd:  0.2518874 
# estimated skewness:  0.7338678 
# estimated kurtosis:  3.032614 
# AIC of beta: -5.590538 
# AIC of norm: 5.716249 
# AIC of lnorm: 6.178649 
# AIC of gamma: -2.23475 

# beta lnorm and gamma are potentially viable options
# gamma is marginally better than lnorm so we will use that to start

# variable selection criteria do not perform well for beta regression (Espinheira et al. 2019)
# so we will search for the best linear model using AIC and LOOCV cross-validation
# then we fit the best linear model with a beta regression
# Espinheira, P. L., da Silva, L. C. M., Silva, A. D. O., & Ospina, R. (2019). Model selection criteria on beta regression for machine learning. Machine Learning and Knowledge Extraction, 1(1), 26.

#### feature selection -------------
# find the best fitting linear model based on AIC
eval(sub_find_bestglm_aic)
# rank                  formula area_m2 HLR_md LR_kgNhay Criterion
# 1    1        LR_kgNhay+area_m2    TRUE  FALSE      TRUE -12.39740
# 2    2           HLR_md+area_m2    TRUE   TRUE     FALSE -11.74256
# 3    3                LR_kgNhay   FALSE  FALSE      TRUE -11.52880
# 4    4                   HLR_md   FALSE   TRUE     FALSE -11.49063
# 5    5 LR_kgNhay+HLR_md+area_m2    TRUE   TRUE      TRUE -10.96235

# if AIC is within 2% then use the model with highest CV rsq
# fit best model with cross LOOCV validation

#### fit best linear model
eval(sub_fit_best_model_with_LOOCV)
# Generalized Linear Model 
# 
# 44 samples
# 3 predictor
# 
# No pre-processing
# Resampling: Leave-One-Out Cross-Validation 
# Summary of sample sizes: 43, 43, 43, 43, 43, 43, ... 
# Resampling results:
#   
#   RMSE      Rsquared   MAE     
# 13.28991  0.4977424  10.42144

# Tuning parameter 'intercept' was held constant at a value of TRUE
m <- model$finalModel

eval(sub_model_summary_stats)

# Call:
#   NULL
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.62129    0.57260   2.831 0.008066 ** 
#   LR_kgNhay   -0.68462    0.18819  -3.638 0.000988 ***
#   area_m2     -0.21891    0.09143  -2.394 0.022894 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Gamma family taken to be 0.3234659)
# 
# Null deviance: 26.864  on 33  degrees of freedom
# Residual deviance: 20.399  on 31  degrees of freedom
# AIC: -8.3974
# 
# Number of Fisher Scoring iterations: 6
# 
# Pseudo R2: 0.2406622 
# 
# Variance inflation factor: (<2 is good, <3 is okay):
#   LR_kgNhay   area_m2 
# 1.056183  1.056183 
# 
# studentized Breusch-Pagan test
# 
# data:  m
# BP = 12.916, df = 2, p-value = 0.001568
# 
# (p > 0.05 indicates the errors are not heteroskedastic)

eval(sub_model_diagnostic_plots)


## COMBINED MODEL =============

df <- df_LW_reg %>% as.data.frame


# find the best fitting model based on AIC
require(bestglm)
best <- bestglm(df,family=quasibinomial())
print(best$BestModels)
best$BestModel %>% plot()
# HLR_md TN_mgl LR_kgNhay area_m2 Criterion
# 1  FALSE  FALSE      TRUE    TRUE  469.1250
# 2   TRUE   TRUE     FALSE    TRUE  469.5927
# 3   TRUE  FALSE     FALSE    TRUE  470.1289
# 4   TRUE  FALSE      TRUE    TRUE  470.2360
# 5  FALSE   TRUE      TRUE    TRUE  471.7788

# fit best model with cross LOOCV validation
model <- train(RE_prp ~ LR_kgNhay+area_m2, data = df, 
               method = "glm",
               family = Gamma(link=log),
               trControl = ctrl)
print(model)
model <- train(RE_pct ~ HLR_md + area_m2, data = df, 
               method = "lm",
               trControl = ctrl)
print(model)
# Linear Regression 
# 
# 78 samples
# 2 predictor
# 
# No pre-processing
# Resampling: Leave-One-Out Cross-Validation 
# Summary of sample sizes: 77, 77, 77, 77, 77, 77, ... 
# Resampling results:
#   
#   RMSE      Rsquared   MAE     
# 20.08932  0.1586315  15.17536
# 
# Tuning parameter 'intercept' was held constant at a value of TRUE

model$finalModel %>% summary()
# Call:
#   lm(formula = .outcome ~ ., data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -53.854 -11.616   0.696  11.134  47.908 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  106.059     14.485   7.322 2.30e-10 ***
#   LR_kgNhay    -15.451      3.663  -4.218 6.82e-05 ***
#   area_m2       -5.325      1.389  -3.833 0.000261 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 19.51 on 75 degrees of freedom
# Multiple R-squared:  0.2298,	Adjusted R-squared:  0.2092 
# F-statistic: 11.19 on 2 and 75 DF,  p-value: 5.597e-05

# Assess multicollinearity
model$finalModel %>% vif()
# LR_kgNhay   area_m2 
# 1.264475  1.264475 

# Diagnostic Plots
par(mfrow=c(2,2))
plot(model$finalModel)
plot(model$finalModel$fitted.values,
     model$trainingData$.outcome,
     main="Obs. vs Fit",
     xlab="predicted",
     ylab="observed")
abline(0,1,col="red")
plot(df$area_m2,
     df$RE_pct,
     main="red = fitted, black = obs.",
     ylab="y")
points(df$area_m2,model$finalModel$fitted.values,col="red")
plot(df$HLR_md,
     df$RE_pct,
     main="red = fitted, black = obs.",
     ylab="y")
points(df$HLR_md,model$finalModel$fitted.values,col="red")
plot(df$LR_kgNhay,
     df$RE_pct,
     main="red = fitted, black = obs.",
     ylab="y")
points(df$LR_kgNhay,model$finalModel$fitted.values,col="red")

## SAVE FITTED MODELS TO FUNCTIONS ====================

fn_pred_RE_pct_from_Walton <- function(area_m2,LR_kgNhay){
  # Linear Regression 
  # 
  # 34 samples
  # 2 predictor
  # 
  # No pre-processing
  # Resampling: Leave-One-Out Cross-Validation 
  # Summary of sample sizes: 33, 33, 33, 33, 33, 33, ... 
  # Resampling results:
  #   
  #   RMSE      Rsquared   MAE     
  # 23.40384  0.1551559  17.11702
  # 
  # Tuning parameter 'intercept' was held constant at a value of TRUE
  
  # Call:
  #   lm(formula = .outcome ~ ., data = dat)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -51.805 -11.676   0.897  12.337  41.392 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  116.109     21.501   5.400 6.82e-06 ***
  #   log10(LR_kgNhay)    -20.825      7.067  -2.947  0.00605 ** 
  #   log10(area_m2)      -5.999      3.433  -1.747  0.09050 .  
  # ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 21.36 on 31 degrees of freedom
  # Multiple R-squared:  0.3247,	Adjusted R-squared:  0.2811 
  # F-statistic: 7.453 on 2 and 31 DF,  p-value: 0.002276
  RE_pct = 116.109 + 
    -20.825*log10(LR_kgNhay) +
    -5.999*log10(area_m2)
  ifelse(RE_pct>95,95,ifelse(RE_pct<1,1,RE_pct))
  return(RE_pct)
}
save(fn_pred_RE_pct_from_Walton,file="3_post/data/fn_pred_RE_pct_from_Walton.Rdata")

fn_pred_RE_pct_from_Land <- function(area_m2,HLR_md,LR_kgNhay){
  # Linear Regression 
  # 
  # 44 samples
  # 3 predictor
  # 
  # No pre-processing
  # Resampling: Leave-One-Out Cross-Validation 
  # Summary of sample sizes: 43, 43, 43, 43, 43, 43, ... 
  # Resampling results:
  #   
  #   RMSE      Rsquared   MAE     
  # 12.90044  0.5104942  10.06257
  
  # Tuning parameter 'intercept' was held constant at a value of TRUE
  # Call:
  #   lm(formula = .outcome ~ ., data = dat)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -23.299  -7.058  -1.065   7.693  32.885 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  108.225     20.814   5.200 6.26e-06 ***
  #   log10(HLR_md)       -17.357      4.980  -3.485  0.00121 ** 
  #   log10(area_m2)       -3.743      1.138  -3.290  0.00209 ** 
  #   log10(LR_kgNhay)    -20.859      4.802  -4.344 9.30e-05 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 12.35 on 40 degrees of freedom
  # Multiple R-squared:  0.5899,	Adjusted R-squared:  0.5591 
  # F-statistic: 19.18 on 3 and 40 DF,  p-value: 7.275e-08
  RE_pct = 108.225 +
    -17.357*log10(HLR_md) + 
    -3.743* log10(area_m2)+ 
    -20.859*log10(LR_kgNhay)
  # constrain predicted values between the minimum and maximum observarations. 
  ifelse(RE_pct>86,86,ifelse(RE_pct<15,15,RE_pct))
  return(RE_pct)
}
save(fn_pred_RE_pct_from_Land,file="3_post/data/fn_pred_RE_pct_from_Land.Rdata")


fn_pred_RE_pct_from_LandWalton <- function(area_m2,LR_kgNhay){
  # Linear Regression 
  # 
  # 78 samples
  # 2 predictor
  # 
  # No pre-processing
  # Resampling: Leave-One-Out Cross-Validation 
  # Summary of sample sizes: 77, 77, 77, 77, 77, 77, ... 
  # Resampling results:
  #   
  #   RMSE      Rsquared   MAE     
  # 20.08932  0.1586315  15.17536
  # 
  # Tuning parameter 'intercept' was held constant at a value of TRUE

  # Call:
  #   lm(formula = .outcome ~ ., data = dat)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -53.854 -11.616   0.696  11.134  47.908 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  106.059     14.485   7.322 2.30e-10 ***
  #   log10(LR_kgNhay)    -15.451      3.663  -4.218 6.82e-05 ***
  #   log10(area_m2)      -5.325      1.389  -3.833 0.000261 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 19.51 on 75 degrees of freedom
  # Multiple R-squared:  0.2298,	Adjusted R-squared:  0.2092 
  # F-statistic: 11.19 on 2 and 75 DF,  p-value: 5.597e-05
  RE_pct = 106.059 + 
  -15.451*log10(LR_kgNhay)+  
  -5.325*log10(area_m2)  
  ifelse(RE_pct>95,95,ifelse(RE_pct<1,1,RE_pct))
  return(RE_pct)
}
save(fn_pred_RE_pct_from_LandWalton,file="3_post/data/fn_pred_RE_pct_from_LandWalton.Rdata")

fn_pred_RE_pct_from_Area_HLR_Conc <- function(HLR_md,area_m2,TN_mgl){
  # df = df_LW %>%
  #   filter(study=="Walton") %>%
  #   mutate(y = RE_pct/100,
  #          x1 = log10(HLR_md),
  #          x2 = log10(area_m2),
  #          x3 = log10(LR_kgNhay),
  #          x4 = log10(LR_kgNhay*area_m2/10000))
  # Linear Regression 
  # 
  # 34 samples
  # 2 predictor
  # 
  # No pre-processing
  # Resampling: Leave-One-Out Cross-Validation 
  # Summary of sample sizes: 33, 33, 33, 33, 33, 33, ... 
  # Resampling results:
  #   
  #   RMSE       Rsquared   MAE      
  # 0.2340384  0.1551559  0.1711702
  # 
  # Tuning parameter 'intercept' was held constant at a value of TRUE
  # > model2$finalModel %>% summary
  # 
  # Call:
  #   lm(formula = .outcome ~ ., data = dat)
  # 
  # Residuals:
  #   Min       1Q   Median       3Q      Max 
  # -0.51805 -0.11676  0.00897  0.12337  0.41392 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  1.16109    0.21501   5.400 6.82e-06 ***
  #   x2          -0.05999    0.03433  -1.747  0.09050 .  
  #   x3          -0.20825    0.07067  -2.947  0.00605 ** 
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 0.2136 on 31 degrees of freedom
  # Multiple R-squared:  0.3247,	Adjusted R-squared:  0.2811 
  # F-statistic: 7.453 on 2 and 31 DF,  p-value: 0.002276
  
  b0 = 42.238 # intercept
  b1 = -10.516 # HLR_md slope
  b2 = -2.0956 # area_m2 slope
  b3 = -6.5828 # TN_mgl slope
  RE_pct = b0 + b1*log(HLR_md) + b2*log(area_m2) + b3*log(TN_mgl)
  ifelse(RE_pct>100,100,ifelse(RE_pct<0,0,RE_pct))
  return(RE_pct)
}
save(fn_pred_RE_pct_from_Area_HLR_Conc,file="3_post/data/fn_pred_RE_pct_from_Area_HLR_Conc.Rdata")

fn_pred_RE_pct_from_Area_NLR <- function(area_m2,LR_kgNhay){
  # Call:
  #   lm(formula = RE_pct ~ log(area_m2) + log(LR_kgNhay), data = .)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -52.992 -11.156  -0.031  13.014  42.011 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)   110.6192    13.3481   8.287 3.71e-12 ***
  #   log(area_m2)   -2.5607     0.6135  -4.174 8.08e-05 ***
  #   log(LR_kgNhay)  -7.5072     1.4482  -5.184 1.82e-06 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 18.64 on 74 degrees of freedom
  # Multiple R-squared:  0.303,	Adjusted R-squared:  0.2841 
  # F-statistic: 16.08 on 2 and 74 DF,  p-value: 1.585e-06
  # AIC: 673.9129
  # VIF:  log(area_m2) log(LR_kgNhay) 
  #       1.188208      1.188208
  # 
  # Resampling: Leave-One-Out Cross-Validation 
  # Summary of sample sizes: 76, 76, 76, 76, 76, 76, ... 
  # Resampling results:
  #   
  #   RMSE      Rsquared   MAE     
  # 19.20513  0.2349216  14.91744

  
  b0 = 110.6192
  b1 = -2.56
  b2 = -7.5072
  RE_pct = b0 + b1*log(area_m2) + b2*log(LR_kgNhay)
  ifelse(RE_pct>100,100,ifelse(RE_pct<0,0,RE_pct))
  return(RE_pct)
}
save(fn_pred_RE_pct_from_Area_NLR,file="3_post/data/fn_pred_RE_pct_from_Area_NLR.Rdata")

# improvement could be bootstrapping of the regression parameters

fn_TIS_Kadlec_HLR <- function(
  # TANKS IN SERIES MODEL FROM KADLEC 2012 
    # variables
  HLR=0.1, # m/day, hydraulic loading rate, or residence time / depth
  TempC = 14, # degrees Celsius, load weighted average temperature
  # coefficients
  k20 = 0.46, # m/day first order removal rate coefficient 
  theta = 1.1, # degC-1, exponential temperature scaling factor
  N = 1.6, # Number of tanks in series
  stochastic = F, #sample from random distribution
  n=1 # number of random samples
){
  # > m <- nls(RE~1-(1+k/(N*HLR))^(-N),
  #            +          data=df,
  #            +          start=list(N=2.9,k=0.04),
  #            +          algorithm = "plinear")
  # > summary(m)
  # Formula: RE ~ 1 - (1 + k/(N * HLR))^(-N)
  # 
  # Parameters:
  #   Estimate Std. Error t value Pr(>|t|)    
  # N    1.602e+00  2.323e+00   0.690 0.492864    
  # k    4.621e-02  9.708e-03   4.760 1.07e-05 ***
  #   .lin 1.080e+02  2.970e+01   3.638 0.000535 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 7.304 on 67 degrees of freedom
  # 
  # Number of iterations to convergence: 9 
  # Achieved convergence tolerance: 8.961e-06
  if(stochastic){
    set.seed(3)
    # from Kadlec 2012 10.1080/10643389.2010.534711
    #95% CI for a Gamma distributed 
    N = rgamma(n, shape = 4.6) # runif(n,1,10) 
    k20 = rgamma(n,shape=5,scale=10)/365 #runif(n,10/365,100/365)
    theta = rnorm(n,1.1,0.01) # 
  }
  k = k20*theta^(TempC-20)
  RE=1-(1+k/(N*HLR))^(-N)
  return(list(N=N,k=k,k20=k20,theta=theta,HLR=HLR,TempC=TempC,RE=RE))
}
# 
xtest <- fn_TIS_Kadlec_HLR(1,14,stochastic=T,n=100)
xtest <- fn_TIS_Kadlec_HLR(1,14,N=0.1,stochastic=F,n=100)
xtest$RE %>% min()
xtest$RE %>% median()
xtest$RE %>% quantile(0.05)
xtest$RE %>% quantile(0.95)

xtest <- fn_TIS_Kadlec_HLR(0.2,14,stochastic=T,n=100)
xtest$RE %>% min()
xtest$RE %>% median()
xtest$RE %>% quantile(0.05)
xtest$RE %>% quantile(0.95)


# CRUMPTON ET AL 2020 ================

fn_pred_RE_pct_from_Crumpto <- function(HLR_md,tempC=14){
  # nitrate removal from Crumpton.
  k20 = 0.102 # hydraulic load coefficent adjusted for temperature
  theta = 1.10 # temperature coefficient
  N = 1.62 # number of tanks fitted parameter
  NRE = 1 - (1+k20*theta^(tempC-20)/(N*HLR_md))^(-N)
  return(NRE)
}
fn_pred_RE_pct_from_Crumpton(HLR_md=0.2,tempC=12.6)

# CRUMPTON ET AL 2020 ================

fn_pred_RE_pct_from_Crumpton <- function(HLR_md){
  k = 0.043 # hydraulic load coefficent adjusted for temperature
  N = 1.7 # number of tanks fitted parameter
  NRE = 1 - (1+k/(N*HLR_md))^(-N)
  return(NRE)
}
fn_pred_RE_pct_from_Crumpton(HLR_md=0.1)


# CHENG AND BASU 2017==================== 

# Cheng, F. Y., Van Meter, K. J., Byrnes, D. K., & Basu, N. B. (2020). Maximizing US nitrate removal through wetland protection and restoration. Nature, 588(7839), 625–630. https://doi.org/10.1038/s41586-020-03042-5
fn_p_k_tau_Cheng <- function(area_m2,
                           stochastic=F, # stochastic parameters
                           n=1 # number of random samples
                           ){
  # variable	description	equation or source or value	units
  # p	removal efficiency percent	p = 100*[1 - e ^ (-k*tau)]	%
  # k	effective removal rate constant	k = c*tau^d	d-1
  # tau	hydraulic residence time	tau = a(SA)^b	d
  # SA	surface area of wetland	input variable	m2
  # a	intercept parameter for power law tau ~ SA relationship	1.48 to 1.62	
  # b	slope parameter for power law tau ~ SA relationship	0.21 to 0.25	
  # c	intercept parameter for power law k~tau relationship	0.31 to 0.45
  # d	slope parameter for power law k~tau relationship	-0.86 to -0.7	
  tau = 1.51 * area_m2 ^ 0.23
  k = 0.38 * tau ^ -0.78
  if(stochastic){
    a = runif(n,1.48,1.86)
    b = runif(n,0.21,0.25)
    c = runif(n,0.31,0.45)
    d = runif(n,-0.86,-0.7)
    tau = a * area_m2 ^ b
    k = c * tau ^ d
  }
  p = (1-exp(-k*tau))*100
  return(list(k=k,tau=tau,p=p))
}

fn_pred_RE_pct_from_Cheng <- function(area_m2){
  # variable	description	equation or source or value	units
  # p	removal efficiency percent	p = 100*[1 - e ^ (-k*tau)]	%
  # k	effective removal rate constant	k = c*tau^d	d-1
  # tau	hydraulic residence time	tau = a(SA)^b	d
  # SA	surface area of wetland	input variable	m2
  # a	intercept parameter for power law k ~ tau relationship	1.48 to 1.62	
  # b	slope parameter for power law k ~ tau relationship	0.21 to 0.25	
  # c	intercept parameter for power law tau~SA relationship	0.31 to 0.45	
  # d	slope parameter for power law tau~SA relationship	-0.86 to -0.7	
  tau = 1.51 *area_m2^ 0.23	
  k = 0.38 * tau ^ -0.78	
  p = (1-exp(-k*tau)) *100
  return(p)
}

save(fn_pred_RE_pct_from_Cheng,file="3_post/data/fn_pred_RE_pct_from_Cheng.Rdata")

# BETA REGRESSION ---------------------------------------

# TEST THE FUNCTIONS -----------------------------------

df_LW %>% summary()
X <- data.frame( area_m2 = runif(min=1,max=8,n=100), 
                 LR_kgNhay = runif(min=1,max=5,n=100), 
                 HLR_md = runif(min=-5,max=2,n=100)) %>%
  mutate(RE_pct_Land_lm = fn_pred_RE_pct_from_Land(10^area_m2,10^HLR_md,10^LR_kgNhay)/100,
         RE_pct_Land_beta = predict(model_RE_prp_Land_betareg_log,newdata=.,type="response"),
         RE_pct_Land_lnorm = predict(model_RE_prp_Land_glm_lnorm,newdata=.,type="response")) %>%
  mutate(
    RE_pct_Land_lm = ifelse(RE_pct_Land_lm>(95/100),
                            95/100,RE_pct_Land_lm),
    RE_pct_Land_lm = ifelse(RE_pct_Land_lm<(1/100),
                            1/100,RE_pct_Land_lm),
    RE_pct_Land_lnorm = ifelse(RE_pct_Land_lnorm>(95/100),
                            95/100,RE_pct_Land_lm),
    RE_pct_Land_lnorm = ifelse(RE_pct_Land_lnorm<(1/100),
                            1/100,RE_pct_Land_lm),
  ) %>%
  bi_class(.,HLR_md,LR_kgNhay,style="jenks")
print(X)



ggplot(X) + geom_point(aes(X$area_m2,X$RE_pct_Land_beta,
                           color=HLR_md))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)

ggplot(X) + geom_point(aes(X$RE_pct_Land_lm,X$RE_pct_Land_beta,
                           color=HLR_md,size=LR_kgNhay))+
  geom_abline(aes(slope=1,intercept=0))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)
ggplot(X) + geom_point(aes(X$RE_pct_Land_lm,X$RE_pct_Land_lnorm,color=HLR_md,
                           color=HLR_md))+
  geom_abline(aes(slope=1,intercept=0))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)
ggplot(X) + geom_point(aes(RE_pct_Land_beta,
                           RE_pct_Land_lnorm,
                           fill=bi_class),pch=21)+
  bi_scale_fill(pal = "GrPink", dim = 3)+
  geom_abline(aes(slope=1,intercept=0))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)
fn_pred_RE_pct_from_Cheng(10^seq(1,5,by=1))



## APPENDIX ============================

# ## VALIDATION PLOT FOR FINAL MODEL --------------------------
# df_LW$pred_RE_pct_m2 <- predict(m2)
# model2$CVrsq = model2$results$Rsquared
# model2$CVRMSE = model2$results$RMSE
# 
# ggplot(df_LW,aes(pred_RE_pct_m2,RE_pct))+
#   geom_point(aes(color=study,shape=study))+
#   geom_abline(slope=1,intercept = 0)+
#   stat_smooth(aes(color=study),se=T,method="lm")+
#   mycolors+
#   labs(caption=paste("LOOCV_rsq:",signif(model2$CVrsq,3),
#                      "LOOCV_RMSE:",signif(model2$CVRMSE,3),
#                      "\nRE_pct~b0 + b1*log(HLR_md)+b2*log(area_m2)+b3*TN_mgl",
#                      "\n(Intercept)  log(HLR_md) log(area_m2)       TN_mgl 
#                      32.62394156  -9.55721186  -2.29744910  -0.02894636"))
# ggsave("images/fig_lm_RE_pct_land_walton.png")
# 
# mycolors = scale_color_manual(values=c("red","blue4"))
# df_LW$pred_RE_pct_m1 <- predict(m1)
# model1$CVrsq = model1$results$Rsquared
# model1$CVRMSE = model1$results$RMSE
# 
# ggplot(df_LW,aes(pred_RE_pct_m1,RE_pct))+
#   geom_point(aes(color=study,shape=study))+
#   geom_abline(slope=1,intercept = 0)+
#   stat_smooth(aes(color=study),se=T,method="lm")+
#   mycolors+
#   labs(caption=paste("LOOCV_rsq:",signif(CVrsq,3),
#                      "\nRE_pct~b0 + b1*log(HLR_md)+b2*log(area_m2)+b3*TN_mgl",
#                      "\n(Intercept)  log(HLR_md) log(area_m2)       TN_mgl 
#                      32.62394156  -9.55721186  -2.29744910  -0.02894636"))
# ggsave("images/fig_lm_RE_pct_land_walton.png")
# 
# fn_3d_plotly <- function (df,xlab=NULL,ylab=NULL,zlab=NULL,clrs=NULL){
#   if (!require("pacman")) install.packages("pacman"); library(pacman)
#   p_load("plotly")
#   if (is.null(xlab)) xlab = colnames(df)[1]
#   if (is.null(ylab)) ylab = colnames(df)[2]
#   if (is.null(zlab)) zlab = colnames(df)[3]
#   if (is.null(clrs)) clrs = palette.colors()
#   fig <- plot_ly(df, 
#                  x = ~df[,1], 
#                  y = ~df[,2], 
#                  z = ~df[,3], 
#                  color = ~df[,4], 
#                  colors = clrs)
#   fig <- fig %>% add_markers()
#   fig <- fig %>% layout(scene = list(xaxis = list(title = xlab),
#                                      yaxis = list(title = ylab),
#                                      zaxis = list(title = zlab)))
#   return(fig)
# }
# 
# 
# # Land model
# m <- df_LW %>% filter(study=="Land") %>%
#   lm(RE_pct~log(HLR_md)+log(area_m2)+log(TN_mgl)+log(LR_kgNhay),data=.)
# m %>% summary
# m %>% AIC
# m %>% car::vif()
# par(mfrow=c(2,2))
# plot(m)
# par(mfrow=c(1,1))
# plot(predict(m),m$model$RE_pct)
# abline(0,1)
# # VIF is for HLR and LR_kgNhay ~>5 which is problematic, remove loading rate. 
# 
# m1 <- df_LW %>% filter(study=="Land") %>%
#   glm(RE_pct~log(HLR_md)+log(area_m2)+log(TN_mgl),
#       family = gaussian(link="identity"),data=.)
# m <- m1
# m %>% summary
# m %>% AIC
# m  %>% car::vif()
# m  %>% anova(test = "Chisq")
# png("images/lm_TN_RE_pct_from_HLR_area_conc.png")
# par(mfrow=c(2,2))
# plot(m)
# dev.off()
# plot(predict(m),m$model$RE_pct)
# abline(0,1)
# lm(m$fitted.values~m$model[,1]) %>% summary()
# 
# # Call:
# #   lm(formula = RE_pct ~ log(HLR_md) + log(area_m2) + log(TN_mgl), 
# #      data = .)
# # 
# # Residuals:
# #   Min      1Q  Median      3Q     Max 
# # -35.181 -10.315  -0.575   9.565  30.159 
# # 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)   44.6825     6.8987   6.477 1.13e-07 ***
# #   log(HLR_md)  -11.2725     2.0033  -5.627 1.71e-06 ***
# #   log(area_m2)  -1.7830     0.6364  -2.802  0.00787 ** 
# #   log(TN_mgl)   -9.7630     3.0017  -3.253  0.00236 ** 
# #   ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 
# # Residual standard error: 14.65 on 39 degrees of freedom
# # Multiple R-squared:  0.4712,	Adjusted R-squared:  0.4305 
# # F-statistic: 11.58 on 3 and 39 DF,  p-value: 1.438e-05
# # VIF:
# # log(HLR_md) log(area_m2)  log(TN_mgl) 
# # 1.236181     1.079134     1.174650 
# 
# predict(m,se.fit=T,intveral="prediction")
# 
# glm(RE_pct~LR_kgNhay,data=walton_2020_NO3,family = gaussian(link="log")) %>% summary()
# exp(4.248)
# exp(0.1440856)
# exp(0.000942)
# exp(0.0001924)
# 
# lm(log(RE_pct)~LR_kgNhay,data=walton_2020_NO3) %>% summary()
# exp(4.12)
# exp(0.0009565)
# 
# ggplot(walton_2020_all,aes(LR_kgNhay,log(RE_pct))) +
#   facet_wrap(~Nutrient,scales="free")+
#   geom_point()+
#   geom_smooth(method="lm",formula=y~x)
# 
# ggplot(land_2016_TN,aes(`LR_kgNhay`,`RE_pct`)) +
#   geom_point()+
#   geom_smooth(method="lm",formula=y~x)

## LAND et al. 

# ggplot(land_2016_TN,aes(`HLR (m/yr)`,`RE_pct`)) +
#   geom_point()+
# #   geom_smooth(method="lm",formula=y~x)
# 
# land_2016_TN %>%
#   lm(LR_kgNhay~HLR_md+TN_mgl,data=.) %>%
#   summary()
# 
# land_2016_TN %>%
#   lm(log(RE_pct+1)~log(HLR_md)*TN_mgl,data=.) %>%
#   summary()
# land_2016_TN %>%
#   lm(RE_pct~TN_mgl*log(HLR_md),data=.) %>%
#   summary()
# land_2016_TN %>%
#   lm(RE_pct~log(LR_kgNhay),data=.) %>%
#   summary()
# plot(land_2016_TN %>% select(RE_pct,LR_kgNhay,TN_mgl,HLR_md))


# # Pseudo R-sq: (null - model)/(null) 
# (best$BestModel$null.deviance - best$BestModel$deviance)/best$BestModel$null.deviance
