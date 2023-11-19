# map sample locations

library(tidyverse)
library(ggmap)

# load "cleaned" data
dat <- readRDS("./metadata/cleaned_metadata.RDS")

source("./R/googlemap_styling.R")
mapstyle <- rjson::fromJSON(file = "./R/mapstyle.json") %>% # from JSON file exported from snazzymaps.com
  googlemap_json_to_string(.)

# build simplified dataframe for lat/lon
lon <- dat$longitude
lat <- dat$latitude

loc <- data.frame(lon,lat,year=dat$collection_year) %>% 
  unique.data.frame()
dat[which(is.na(dat$latitude) | is.na(dat$longitude)),]

# build basemap
ggmap::register_google(Sys.getenv("APIKEY"))

basemap <- ggmap::get_googlemap(c(0,0),zoom = 1,style = mapstyle)

p <- 
ggmap(basemap) +
  geom_point(data=loc,
             aes(x=lon,y=lat),
             color='black',
             size=2,
             alpha=1) +
  geom_point(data=loc,
             aes(x=lon,y=lat),
             color='red3',
             size=1,
             alpha=1) 

p +
  lims(x=c(-180,180),y=c(-70,77)) 
ggsave(filename="./output/figs/sample_location.png",dpi=500,height = 10,width = 16)

# Might be interesting background for paper to download metadata for ALL marine metagenomes (just meatadata),
# and run through date cleanup from previous script. To show when marine MG data has been collected over time.

# find samples with locations that don't pass muster ####
# # what's up with mongolia!? # now removed in cleaning script
# ggmap(basemap) +
#   geom_point(data=loc,
#              aes(x=lon,y=lat),
#              color='black',size=.5) +
#   lims(x=c(60,75),y=c(45,55))
# 
# # isolate those 3 weird samples
# 
# dat %>% 
#   filter(latitude > 50 & latitude < 55 & longitude > 65 & longitude < 75) %>% 
#   pluck("bio_project") %>% 
#   unique()
# # "PRJEB37465" remove these from metadata in cleaning script








# check location variable
p2 <- ggmap(basemap) +
  geom_point(data=dat,
             aes(x=longitude,y=latitude,color=material,fill=bio_project),
             size=.5) +
  guides(fill = 'none')
p2

p2 +
  lims(x=c(-200,-150),y=c(50,75))
p3 <- 
p2 +
  lims(x=c(100,150),y=c(30,50)) 
p3

plotly::ggplotly(p3)
plotly::ggplotly(p2)
