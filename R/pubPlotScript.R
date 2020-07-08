#################################################
####  Plotting for WinTor Publication        ####
####  Author: C. Reed Hranac                 ####
####  Tested                                 ####
#################################################

## This script provides all the plotting code for the wintor publication
##  Contents:
  ## Main text figures:
  ## figure 1: Data type and location
  ## figure 2: Predicted winter duration across North America
  ## figure 3: Predicted body mass and body fat
  ## figure 4: Predicted hibernation survival 
  ## figure 5: Increased energy expenditure due to WNS infection

  ## Si figures:

#### Set Up ####
env.prior <- ls()

## libraries
library(tidyverse); library(sf); library(raster); library(data.table)

## after these are all generated the first time you can comment out all the 
## previous lines

##creating North America political boundries 
# canada <- getData("GADM",country="CAN",level=1)
# usa <- getData("GADM",country="USA", level=1)
# mexico <- getData("GADM",country="MEX", level=1)
# North.America <- rbind(canada,usa,mexico)
# plot(North.America)
# writeOGR(North.America,
#          dsn = win.dat,
#          layer = "NorthAmerica.WGS",
#          driver = "ESRI Shapefile")

library(sf);library(rgdal);library(raster)
North.America <- st_read(win.dat, layer="NorthAmerica.WGS")
NA.utm <- st_transform(North.America, 2955)

##mylu distribution
mylu.dist <- st_read(file.path(win.dat, "shapeFiles"), 
                     layer = "myotis_lucifugus")

st_crs(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mylu.utm <- st_transform(mylu.dist, 2955)

## read the products back in 
NA.utm <- st_read(dsn = file.path(win.dat, "shapeFiles"),
                  layer = "NorthAmerica")
mylu.utm <- st_read(dsn = file.path(win.dat, "shapeFiles"),
                    layer = "MYLU_utm")
mylu.utm <- st_transform(mylu.utm,
                         "+proj=utm +zone=11 +ellps=GRS80 +units=m +no_defs")

####  Figure 1 ####
#### location x Data type ####
dur.raw <- fread("data/durationDataReferenced.csv")
mass.raw <- fread("data/massDataReferenced.csv")

dur.raw$type <- "Duration"
mass.raw$type <- "Mass"

full.dat <- full_join(dur.raw, mass.raw) %>%
  dplyr::select(Lat, Long, type)
colnames(full.dat)[[3]] <- "Data Type"

full.sf <- st_as_sf(full.dat,
                    coords = c("Long","Lat"))
st_crs(full.sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
full.utm <- st_transform(full.sf, 2955)

##further crop to the species extent 
NA.mylu <- st_crop(NA.utm, mylu.utm) ## this apparently wont work

## plot
(dat.map <- ggplot() +
    ##North American political boundaries
    geom_sf(data = NA.mylu,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA)+
    geom_sf(data = mylu.utm,
            aes(group = "SP_ID"),
            color="dodgerblue4",
            fill="lightblue1", 
            size = .7,
            alpha = .2)+
    geom_sf(data = full.utm,
            aes(shape = `Data Type`, color = `Data Type`),
            alpha = .7, #position = "dodge",
            size = 2,
            show.legend = "point")+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    scale_color_manual(values=c("black","red")) +
    theme_bw()
)


ggsave(filename = file.path(win.res, "fig", "dataLocations.png"),
       dat.map, device = "png",
       height = 4.875,
       width = 7.85,
       units = "in",
       dpi = 300)

## this refuses to work and I have no idea why. NA background wont crop to
## the mylu distribution despite this exact code working earlier and the
## error message for the crop is complete bs, they have the exact same crs