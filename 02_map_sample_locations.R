# map sample locations

library(tidyverse)
library(ggmap)

# load "cleaned" data
dat <- readRDS("./metadata/cleaned_metadata.RDS")



# build simplified dataframe for lat/lon
lon <- dat$longitude
lat <- dat$latitude

loc <- data.frame(lon,lat)


# build basemap
ggmap::register_google(Sys.getenv("APIKEY"))

p <- 
ggmap(basemap) +
  geom_point(data=loc,
             aes(x=lon,y=lat),
             color='black',size=.5) +
  geom_point(data=loc,
             aes(x=lon,y=lat),
             color='red',
             size=.25,
             alpha=.25) 

p +
  lims(x=c(-180,180),y=c(-75,75))

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
  geom_point(data=loc,
             aes(x=lon,y=lat,color=dat$location,fill=dat$bio_project),
             size=.5) +
  guides(fill = 'none')
p2

p3 <- 
p2 +
  lims(x=c(100,150),y=c(30,50)) 
p3

plotly::ggplotly(p3)
plotly::ggplotly(p2)
