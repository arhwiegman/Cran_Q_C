# map flow verification.

# SET UP ENVIRONMENT -----------------

# clear objects from environment 
rm(list=ls()) 

# make sure pacman and tidyverse are installed
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all") # detach all non-base R packages
pacman::p_load(tidyverse) # load tidyverse

packages <- c("tidyverse","readxl","cowplot", "googleway", "ggplot2", "ggrepel", 
                        "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata")

# load packages
packages  %>%
  (\(x) try(pacman::p_load(char=x,character.only=T),F))()

# Load data and Set Mapping Parameters -------------
theme_set(theme_bw())
load("3_post/data/analysis_Verify_Discharge.Rdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
coasts <- ne_coastline(scale = 10,returnclass = "sf")
usa <- ne_states(country="united states of america",returnclass = "sf")
class(usa)

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name_sort)), " countries)"))

ggplot(data = usa) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

ggplot(data = usa) +
  geom_sf() +
  coord_sf(xlim = c(-74, -69), ylim = c(40, 44), expand = FALSE)

df <- df %>% mutate(pdiff = 100*(1-Q_m3d_Dinfgwebf/Q_m3d),
                    pmeas = Q_m3d_Dinfgwebf/Q_m3d)

df$pdiff %>% summary
g1 <- ggplot(data = usa) +
  geom_sf(fill="#FFEFDB") +
  coord_sf(xlim = c(-71.2, -69.9), ylim = c(41.5, 42.5), expand = FALSE)+
  geom_point(data=df,aes(x=Long,y=Lat,fill=log10(pmeas)),pch=21,alpha=0.75,size=1.5)+
  scale_fill_gradient2(high="green3",mid="#f7f7f7",low="purple")
ggsave("images/map_verify_Q_residual_pred_over_obs.png",dpi=600)


g2 <- ggplot(data = usa) +
  geom_sf(fill="#FFEFDB") +
  coord_sf(xlim = c(-71.2, -69.9), ylim = c(41.5, 42.5), expand = FALSE)+
  geom_point(data = df %>% filter(source == "MEP"),
             aes(y=Lat,x=Long),
             fill="grey95",shape=21,alpha=0.75,size=2.5)+
  geom_point(data = df %>% filter(source == "USGS"),
             aes(y=Lat,x=Long,shape=source),
             fill="blue",shape=24,alpha=0.75,size=2)+
  geom_point(data = df %>% filter(system == "Cranberry"),
             aes(y=Lat,x=Long),
             shape=22,fill="red",alpha=0.75,size=2)
ggsave("images/map_verify_Q_source.png",dpi=600)
