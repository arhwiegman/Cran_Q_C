# prepare tanks-in-series model
# Author: Adrian Wiegman arhwiegman@github.io
# Date Modified: 2024-09-04 19:10:07

# crumpton
#library(digitize)
# Nutrient removal efficiency of TN vs HLR using relaxed TIS model
# digitize("0_raw\\Crumpton\\jeq220061-fig-0005-m.jpg")
data <- "
n HLR RE
1 0.01299137 92.198582
2 0.01299137 85.638298
3 0.01598087 87.234043
4 0.02195987 88.829787
5 0.03511367 90.780142
6 0.01837247 81.914894
7 0.02853677 75.886525
8 0.02554727 70.921986
9 0.04049476 62.588652
10 0.04348426 60.106383
11 0.04826746 49.645390
12 0.07397715 51.063830
13 0.07397715 51.063830
14 0.06201916 44.148936
15 0.06979185 45.035461
16 0.06680235 41.134752
17 0.05723596 39.007092
18 0.07636875 42.553191
19 0.07696665 43.439716
20 0.08533725 42.553191
21 0.07696665 38.297872
22 0.08294565 31.737589
23 0.09012045 26.418440
24 0.10267634 35.283688
25 0.10447004 44.858156
26 0.11343854 37.056738
27 0.10985114 31.205674
28 0.12001543 35.638298
29 0.11762384 31.560284
30 0.11702594 29.255319
31 0.12539653 18.262411
32 0.12539653 30.496454
33 0.12958183 28.191489
34 0.13197343 17.375887
35 0.13974613 20.390071
36 0.15050832 23.404255
37 0.15110622 31.205674
38 0.15768312 22.695035
39 0.16186842 37.056738
40 0.16844532 25.354610
41 0.16784742 18.617021
42 0.17382641 27.127660
43 0.17621801 34.929078
44 0.17801171 22.695035
45 0.18100121 20.035461
46 0.19594871 17.021277
47 0.20013401 21.099291
48 0.19893821 9.929078
49 0.21029830 16.312057
50 0.21448360 19.326241
51 0.21747310 18.971631
52 0.22106050 25.000000
53 0.21926680 11.524823
54 0.23481219 29.609929
55 0.23780169 32.269504
56 0.23780169 29.078014
57 0.24437859 29.787234
58 0.23959539 26.773050
59 0.23839959 24.290780
60 0.23839959 22.695035
61 0.23839959 9.751773
62 0.24557439 14.184397
63 0.25035759 20.212766
64 0.26231558 16.489362
65 0.27965468 9.929078
66 0.29340637 11.879433
67 0.29639587 28.368794
68 0.32031186 11.524823
69 0.33824885 16.666667
70 0.42494432 10.460993"
library(tidyverse)
df <- readr::read_delim(data,skip=1)
str(df)



# NONLINEAR MODEL FIT OF TIS for Prairie Pothole Sites From IOWA
m <- nls(RE~1-(1+k/(N*HLR))^(-N),
         data=df,
         start=list(N=2.9,k=0.04),
         algorithm = "plinear")
summary(m)
plot(m)


# 
fn_TIS <- function(
    # variables
    HLR=0.1, # m/day, hydraulic loading rate, or residence time / depth
    TempC = 14, # degrees celsius, load weighted average temperature
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

x <- fn_TIS(1,T,100)
x$RE %>% min()
x$RE %>% median()
x$RE %>% quantile(0.05)
x$RE %>% quantile(0.95)



n = 100000
HLR=10^seq(-4,3,length.out=n)
TempC = runif(n,11,14)
N %>% quantile(0.95)
N %>% quantile(0.5)
N %>% quantile(0.05)
N = rgamma(n, shape = 4.6) # runif(n,1,10) 
k20 = rgamma(n,shape=4,scale=12)/365 #runif(n,10/365,100/365)
k20 %>% quantile(0.95)
k20  %>% quantile(0.5)
k20 %>% quantile(0.05)
theta = rnorm(n,1.1,0.01)
df_pred = as.data.frame(fn_TIS(HLR,TempC,k20,theta,N,F))

seq(-4,3,by=0.25)
df_pred_summary <- df_pred %>% mutate(HLR_bin=round(log10(HLR)*10,0)/10) %>% 
  group_by(HLR_bin) %>%
  summarize(
    HLR_p05 = quantile(HLR,0.05),
    HLR_p25 = quantile(HLR,0.25),
    HLR_p50 = quantile(HLR,0.5),
    HLR_p75 = quantile(HLR,0.75),
    HLR_p95 = quantile(HLR,0.95),
    RE_p05 = quantile(RE,0.05),
    RE_p25 = quantile(RE,0.25),
    RE_p50 = quantile(RE,0.5),
    RE_p75 = quantile(RE,0.75),
    RE_p95 = quantile(RE,0.95))
df_pred_summary
fn_TIS_RE_p05_from_HLR <- approxfun(df_pred_summary$HLR_p50,df_pred_summary$RE_p05)
fn_TIS_RE_p25_from_HLR <- approxfun(df_pred_summary$HLR_p50,df_pred_summary$RE_p25)
fn_TIS_RE_p50_from_HLR <- approxfun(df_pred_summary$HLR_p50,df_pred_summary$RE_p50)
fn_TIS_RE_p75_from_HLR <- approxfun(df_pred_summary$HLR_p50,df_pred_summary$RE_p75)
fn_TIS_RE_p95_from_HLR <- approxfun(df_pred_summary$HLR_p50,df_pred_summary$RE_p95)
10126/(5.4*10000)
10126/(30*10000)

fn_TIS_RE_p50_from_HLR(0.19)
fn_TIS_RE_p50_from_HLR(0.03)

theme_set(theme_bw())
ggplot(data=df_pred,aes(log10(HLR),RE))+geom_point(aes(color=TempC))+
  geom_smooth(color="red")
library(plotly)
ggplotly()


print(df_pred_summary)

# MAKE PLOT WITH CUTOFF AT 1 m/d
g <- ggplot(data=df_pred_summary,aes(x=10^(HLR_bin)))+
  geom_point(data=df_pred,aes(x=HLR,y=RE),pch=21,alpha=0.1)+
  geom_line(aes(y=RE_p50),color="red",size=2)+
  geom_line(aes(y=RE_p95),color="blue",size=1,linetype="dashed")+
  geom_line(aes(y=RE_p05),color="blue",size=1,linetype="dashed")

g + 
  scale_x_log10()+
  annotation_logticks(sides = "b") 
ggplotly()
# 
ggplot(data=df_pred_summary,aes(x=10^HLR_bin))+
  geom_point(data=df_pred,aes(x=HLR,y=RE),pch=21,alpha=0.1)+
  geom_line(aes(y=RE_p50),color="red",size=1.5)+
  geom_line(aes(y=RE_p95),color="blue",size=1,linetype="dashed")+
  geom_line(aes(y=RE_p05),color="blue",size=1,linetype="dashed")+
  
  #xlim(c(0,1))
ggplotly()

                      