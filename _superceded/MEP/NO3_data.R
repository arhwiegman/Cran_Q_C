library(readxl)
library(tidyverse)


# cape cod rivers observatory
df_CCRO <- read_excel("data/CCRO_Summary_wiegman.xlsx") %>%
  mutate(dataset="CCRO") 

# mass estuaries program
df_MEP <- read_csv("data/MEP_SummaryData_Coords.csv") %>%
  rename(NO3=`NOX (mg/L)`,TN=`TN (mg/L)`,Long=Lon) %>%
  select(Region_MEP,Lat,Long,NO3,TN) %>%
  na.omit() %>%
  mutate(dataset="MEP") %>%
  mutate(Long = as.double(Long))
  

# public supply wells on the cape
df_PSW <- read_excel("data/Yearly_Nitrate_Concentration_Averages_in_Public_Supply_Wells_TableToExcel.xlsx") %>% 
  mutate(dataset="PSW") %>%
  rename(Lat = LATITUDE,
         Long = LONGITUDE,
         NO3 = YR_AVG)

# DEP Department of Watershed Protection, Watershed Planning Program (WPP)
df_WPP_all <- read_csv("df_DER_alldata_NO3_TN_summary.csv") %>% 
  mutate(dataset="DWMWPP")
# data from rivers and streams only
df_WPP <- read_csv("df_DER_streams_NO3_TN_summary.csv") %>% 
  mutate(dataset="DWMWPP")

df_NO3_merged <-  full_join(df_CCRO,df_MEP) %>% full_join(df_PSW) %>% full_join(df_WPP_all) 

df_NO3_merged_XY_streams <- df_NO3_merged %>% filter(dataset!="PSW") %>% filter(MLTYP_NAME=="River/Stream") %>% select(dataset,Lat,Long,NO3,TN) %>% write_csv("data/df_NO3_merged_XY.csv")

df_NO3_merged_XY_streams %>% write_csv("data/df_NO3_merged_XY_rivers_streams.csv")

df_NO3_merged %>% select(dataset,Lat,Long,NO3,TN) %>% write_csv("data/df_NO3_merged_XY_all.csv")


########################################

# Geographically Weighted Regression   #
# Total Nitrogen                       #

########################################

library (tidyverse)
library(sp)
library(spgwr)
library(maptools)

df <- readr::read_csv("data/df_NO3_merged_XY_all.csv") %>% 
  na.omit() %>% 
  filter(Long>=-71.25,Lat<=42.25)

summary(df$NO3)
summary(df$TN)

df %>% ggplot(aes(x=NO3,y=TN/NO3,color=dataset)) + 
  geom_point()+geom_smooth(method="lm")
m <- lm(I(TN/NO3)~log(1/NO3),data=df)
summary(m)

X = seq(-4,1,length.out=20)
exp(X)
exp(0.31493*exp(1/X)^(0.66561))

df$preda_logTN2NO3 <- predict(m)
df$preda_TN2NO3 <- exp(df$preda_logTN2NO3)
df$pred_v_TN2NO3 <- exp(0.31493*1/df$NO3^(0.66561))
df$preda_TN <- df$preda_TN2NO3 * df$NO3 

df %>% ggplot(aes(x=TN,y=preda_TN,color=dataset)) + 
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