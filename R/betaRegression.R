#### Beta Regression Attempts ####
## Preliminary modeling for winTor

library(raster);library(tidyverse);library(AICcmodavg)
library(lubridate); library(betareg)

#link to external
win.link <- "D://Dropbox/winTor_aux/data"

dat <- read.csv("data/durationData.csv") #read data
dat$ID <- 1:nrow(dat)

dat$Start <- as.Date(dat$Start, "%d-%h") # Add time junk
dat$End<- as.Date(dat$End,"%d-%h")

for(i in 1:nrow(dat)){ # work around for duration 
  ifelse(is.na(dat$Duration[[i]]), 
         dat$Duration[[i]] <- dat$Start[[i]]- dat$End[[i]],
         dat$Duration[[i]] <- dat$Duration[[i]])  
}

dat$dur.prop <- dat$Duration/365
#Convert to spatial object
coordinates(dat) <- ~ lat + long

#load all the rasters
env <- stack( list.files(win.link, pattern = "Prop_", full.names = T))
dem <- raster(file.path(win.link, "NA_dem.tif"))
north <- raster(file.path(win.link, "NA_northing.tif"))
env <- stack(env,dem,north)
#extract estimations for locations
env.dat <- raster::extract(env, dat, cellnumber = T, df = T)

env.df <- left_join(as.data.frame(dat), env.dat, by = "ID")

####Single Variables ####
b1.ff <- betareg(dur.prop ~ Prop_frostFreeze, data = env.df)
summary(b1.ff)

b1.frz <- betareg(dur.prop ~ Prop_nFreezingDays, data = env.df)
summary(b1.frz)

b1.frst <- betareg(dur.prop ~ Prop_nFrostyDays, data = env.df)
summary(b1.frst)

b1.grow <- betareg(dur.prop ~ Prop_nonGrowingDays, data = env.df)
summary(b1.grow)

b1.dem <- betareg(dur.prop ~ NA_dem, data = env.df)
summary(b1.dem)

b1.north <- betareg(dur.prop ~ NA_northing, data = env.df)
summary(b1.north)

aictab(list(b1.ff, b1.frz, b1.frst, b1.grow, b1.dem, b1.north),
       modnames = c("FrostFreeze", "Freeze", "Frost", "Grow", "Dem", "Northing"))

####Add Northing ####
b2.ff <- betareg(dur.prop ~ NA_northing + Prop_frostFreeze, data = env.df)
summary(b2.ff)

b2.frz <- betareg(dur.prop ~ NA_northing + Prop_nFreezingDays, data = env.df)
summary(b2.frz)

b2.frst <- betareg(dur.prop ~ NA_northing + Prop_nFrostyDays, data = env.df)
summary(b2.frst)

b2.grow <- betareg(dur.prop ~ NA_northing + Prop_nonGrowingDays, data = env.df)
summary(b2.grow)

b2.dem <- betareg(dur.prop ~ NA_northing + NA_dem, data = env.df)
summary(b2.dem)

aictab(list(b2.ff, b2.frz, b2.frst, b2.grow, b2.dem),
       modnames = c("FrostFreeze", "Freeze", "Frost", "Grow", "Dem"))

#### Add Dem ####
b3.ff <- betareg(dur.prop ~ NA_northing + NA_dem + Prop_frostFreeze, data = env.df)
summary(b3.ff)

b3.frz <- betareg(dur.prop ~ NA_northing + NA_dem + Prop_nFreezingDays, data = env.df)
summary(b3.frz)

b3.frst <- betareg(dur.prop ~ NA_northing + NA_dem + Prop_nFrostyDays, data = env.df)
summary(b3.frst)

b3.grow <- betareg(dur.prop ~ NA_northing + NA_dem + Prop_nonGrowingDays, data = env.df)
summary(b3.grow)


aictab(list(b3.ff, b3.frz, b3.frst, b3.grow),
       modnames = c("FrostFreeze", "Freeze", "Frost", "Grow"))

#### Add interaction####
b2i <- betareg(dur.prop ~ NA_northing * NA_dem, data = env.df)
summary(b2i)

b3i.ff <- betareg(dur.prop ~ NA_northing * NA_dem + Prop_frostFreeze, data = env.df)
summary(b3i.ff)

b3i.frz <- betareg(dur.prop ~ NA_northing * NA_dem + Prop_nFreezingDays, data = env.df)
summary(b3i.frz)

b3i.frst <- betareg(dur.prop ~ NA_northing * NA_dem + Prop_nFrostyDays, data = env.df)
summary(b3i.frst)

b3i.grow <- betareg(dur.prop ~ NA_northing * NA_dem + Prop_nonGrowingDays, data = env.df)
summary(b3i.grow)


aictab(list(b2i, b3i.ff, b3i.frz, b3i.frst, b3i.grow),
       modnames = c("DemNorthing" ,"FrostFreeze", "Freeze", "Frost", "Grow"))

