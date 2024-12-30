# Verify Flow

# SET UP ENVIRONMENT -----------------

# clear objects from environment 
rm(list=ls()) 

# make sure pacman and tidyverse are installed
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all") # detach all non-base R packages
pacman::p_load(tidyverse) # load tidyverse

# type package names in the string below, use comma as separator
package_names <- "tidyverse,readxl" 

# convert string of package names to a vector
packages <- str_split_1(package_names,",") %>%
  str_extract("\\w+") %>%
  (\(x) x[!is.na(x)])()

# load packages
packages  %>%
   (\(x) try(pacman::p_load(char=x,character.only=T),F))()

# read in data ------------
# USGS gauge discharge
df_Q_bogs_streams_XYTableToPoint_TableToExcel <- read_excel("data/df_Q_bogs_streams_XYTableToPoint_TableToExcel.xls")

df <- df_Q_bogs_streams_XYTableToPoint_TableToExcel %>% na.omit

ggplot(df)+ 
  geom_point(aes(x=Q_m3d,y=Q_m3d_FA1,shape=source,color=source),alpha=0.5,stroke=1.5,size=2)+
  xlab("Modeled")+
  ylab("Measured")+
  geom_smooth(aes(x=Q_m3d,y=Q_m3d_FA1),method="lm")+
  theme_bw()