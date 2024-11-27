# clear environment
rm(list=ls ())

load("1_prep/data/prep_NO3_data.Rdata")
library(tidyverse)
library(sp)
library(spgwr)

df <- readr::read_csv("1_prep/df_NO3_TN_all.csv") %>% 
  filter(dataset!="PSW") %>% 
  select(dataset,Long,Lat,NO3,TN) %>%
  filter(Long>=-71.5,Lat<=42.25)%>% 
  na.omit()# %>% 
  #

nrow(df %>% na.omit())
m <- lm(TN~NO3,data=df)
summary(m)
m_BBC <- lm(TN~NO3,data=df %>% filter(dataset=="BBC"))
summary(m_BBC)
m_MEP <- lm(TN~NO3,data=df %>% filter(dataset=="MEP"))
summary(m_MEP)
m_WPP <- lm(TN~NO3,data=df %>% filter(dataset=="WPP"))
summary(m_WPP)

mycolors = scale_color_manual(values=c("magenta","cyan","yellow","grey10"))
myfills = scale_fill_manual(values=c("magenta","yellow","cyan","grey10"))
myshapes = scale_shape_manual(values=c(24,21,3,5))
#mycolors_bw = scale_fill_manual(values=c("white","grey70","grey10"))
#myshapes_bw = scale_shape_manual(values=c(24,21,22))
set.seed(3)
TN_med = df$TN %>% median
print(TN_med)
NO3_med = df$NO3 %>% median
print(NO3_med)

df %>% sample_frac(1L) %>%
  ggplot(aes(x=NO3,y=TN)) + 
  geom_vline(aes(xintercept=NO3_med),color="blue",size=1,linetype="dotted")+
  geom_hline(aes(yintercept=TN_med),color="blue",size=1,linetype="dotted")+
  geom_abline(slope=1,intercept=0)+
  geom_smooth(method="lm",color="red",linetype="dashed",size=1)+
  geom_point(aes(shape=dataset),color="black",fill="transparent",stroke=0.5)+
  geom_point(aes(fill=dataset,shape=dataset),stroke=0.5,alpha=0.75)+
  myfills+
  myshapes+
  theme_bw()+
  labs(subtitle="TN = b1*NO3 + b0\nb1 = 1.05 +/- 0.03\nb0 = 0.55 +/- 0.02\nR^2 = 0.77\nn = 284\np<2e-16")
ggsave("images/fig_NO3_TN_dataset_scatter.png")

df <- df_NO3_TN_merged_SE_imputed %>% 
  filter(dataset!="PSW") %>% 
  filter(Long>=-71.5,Lat<=42.25)# %>% 
summary(df$NO3)
summary(df$TN)

df %>%
  ggplot(aes(x=Long,y=Lat,color=TN))+geom_point()+
  geom_point()

df %>%
  ggplot(aes(x=Long,y=Lat,color=NO3))+geom_point()+
  geom_point()

mycolors = scale_fill_manual(values=c("magenta","grey88","yellow","cyan"))
myshapes = scale_shape_manual(values=c(24,21,3,5))
#mycolors_bw = scale_fill_manual(values=c("white","grey70","grey10"))
#myshapes_bw = scale_shape_manual(values=c(24,21,22))
set.seed(3)
TN_med = df$TN %>% median
print(TN_med)
NO3_med = df$NO3 %>% median
print(NO3_med)

df %>% sample_frac(1L) %>%
  ggplot(aes(x=NO3,y=TN)) + 
  geom_vline(aes(xintercept=NO3_med),linetype="dotted")+
  geom_hline(aes(yintercept=TN_med),linetype="dotted")+
  geom_abline(slope=1,intercept=0)+
  geom_smooth(method="lm",color="black",linetype="dashed",size=0.6)+
  geom_point(aes(shape=dataset),fill="transparent",stroke=0.5)+
  geom_point(aes(color=dataset,shape=dataset),stroke=0.5,alpha=0.5)+
  mycolors+
  myshapes+
  theme_bw()+
  labs(subtitle="TN = b1*NO3 + b0\nb1 = 1.03 +/- 0.04\nb0 = 0.56 +/- 0.02\nR^2 = 0.6507\nn = 245\np<2e-16")

ggplot()+
  geom_point(data=df %>% filter(dataset=="WPP"),
             aes(x=NO3,y=TN),
             pch=22,fill="yellow",color="black") +
  geom_point(data=df %>% filter(dataset=="MEP"),
             aes(x=NO3,y=TN),
             pch=21,fill="lightgrey",alpha=0.75)+
  geom_point(data=df %>% filter(dataset=="BBC"),
             aes(x=NO3,y=TN),
             pch=24,fill="magenta2",alpha=0.75) +
  geom_vline(aes(xintercept=NO3_med),linetype="dotted")+
  geom_hline(aes(yintercept=TN_med),linetype="dotted")+
  geom_abline(slope=1,intercept=0)+
  geom_smooth(data=df,aes(x=NO3,y=TN),method="lm",color="black",linetype="dashed",size=0.6)+
  mycolors+
  myshapes+
  theme_bw()
ggsave("images/fig_TN_vs_NO3_all_SW_BW.png",height=3.5,width=3.5)

nrow(df %>% na.omit())
m <- lm(TN~NO3,data=df)
summary(m)
m_BBC <- lm(TN~NO3,data=df %>% filter(dataset=="BBC"))
summary(m_BBC)
m_MEP <- lm(TN~NO3,data=df %>% filter(dataset=="MEP"))
summary(m_MEP)
m_WPP <- lm(TN~NO3,data=df %>% filter(dataset=="WPP"))
summary(m_WPP)


df %>% ggplot(aes(x=log(NO3),y=log(TN/NO3)))+ 
  geom_point(aes(color=dataset))+geom_smooth(method="lm")
m <- lm(log(TN/NO3)~log(NO3),data=df)
summary(m)

X = seq(-4,1,length.out=20)
exp(X)
exp(0.16122*exp(X)^(-0.80587))

df$preda_logTN2NO3 <- predict(m)
df$preda_TN2NO3 <- exp(df$preda_logTN2NO3)
df$pred_v_TN2NO3 <- exp(0.16122*exp(X)^(-0.80587))
df$preda_TN <- df$preda_TN2NO3 * df$NO3 

df %>% ggplot(aes(x=TN,y=df$preda_TN2NO3,color=dataset)) + 
  geom_point()+geom_smooth(method="lm") + geom_abline(slope=1,intercept=0)

View(df)
df %>% ggplot(aes(x=NO3,y=TN,color=dataset)) + 
  geom_point()+geom_smooth(method="lm")
mb <- lm(TN~NO3,data=df)
summary(mb)
df$predb_TN <- predict(mb)

df %>% ggplot(aes(x=TN,y=predb_TN,color=dataset)) + 
  geom_point()+geom_smooth(method="lm") + geom_abline(slope=1,intercept=0)

x = df$Long
y = df$Lat
spdf <- sp::SpatialPointsDataFrame(df %>% select(Long,Lat), df %>% select(dataset,NO3,TN))

model1 <- lm(TN ~ NO3, data = df)
summary(model1)
plot(model1,which=3)

resids<-residuals(model1)
colours <- c("dark blue", "blue", "red", "dark red") 
#here it is assumed that your eastings and northings coordinates are stored in columns called x and y in your dataframe
map.resids <- SpatialPointsDataFrame(coords=cbind(x,y),data=data.frame(resids)) 
#for speed we are just going to use the quick sp plot function, but you could alternatively store your residuals back in your LondonWards dataframe and plot using geom_point in ggplot2
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(TN~NO3, data=df, coords=cbind(x,y),adapt=T) 

#run the gwr model
gwr.model = gwr(TN~NO3, data=df, coords=cbind(x,y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
#print the results of the model
gwr.model

results<-as.data.frame(gwr.model$SDF)
head(results)


#attach coefficients to original dataframe
df$NO3_beta<-results$NO3

#now plot the various GWR coefficients                       
gwr.point1 <- ggplot(df, aes(x=x,y=y))+geom_point(aes(colour=df$NO3_beta))+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
#gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()