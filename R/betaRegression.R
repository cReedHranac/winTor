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

