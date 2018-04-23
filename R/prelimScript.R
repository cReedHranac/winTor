## Preliminary modeling for winTor

library(raster);library(tidyverse)
library(lubridate)

#link to external
win.link <- "D://Dropbox/winTor_aux/data"

dat <- read.csv("data/durationData.csv") #read data
dat$ID <- 1:nrow(dat)

dat$Start <- as.Date(dat$Start, "%d-%h") # Add time junk
dat$End<- as.Date(dat$End,"%d-%h")

for(i in 1:nrow(dat)){ # work around for duration 
  ifelse(is.na(dat$Duration[[i]]), 
         dat$Duration[[i]] <- 365 - (dat$Start[[i]]- dat$End[[i]]),
         dat$Duration[[i]] <- dat$Duration[[i]])  
}

#Convert to spatial object
coordinates(dat) <- ~ lat + long

#load all the rasters
env <- stack(list.files(win.link, pattern = "NA_*", full.names = T))
#extract estimations for locations
env.dat <- raster::extract(env, dat, cellnumber = T, df = T)

env.df <- left_join(as.data.frame(dat), env.dat, by = "ID")

library(AICcmodavg)
## Single Variables
d1.frost <- lm(formula = Duration ~ NA_nFrostyDays, data = env.df)
summary(d1.frost)

d1.grow <- lm(formula = Duration ~ NA_nonGrowingDays, data = env.df)
summary(d1.grow)

d1.north <- lm(formula = Duration ~ NA_northing, data = env.df)
summary(d1.north)

d1.static <- lm(formula = Duration ~ NA_staticWinter, data = env.df)
summary(d1.static)

d1.FF <- lm(formula = Duration ~ NA_frostFreeze, data = env.df)
summary(d1.FF)

d1.freeze <- lm(formula = Duration ~ NA_nDaysFreeze, data = env.df)
summary(d1.freeze)

#aic Table
d1.res <- aictab(list(d1.frost, d1.grow, d1.north, d1.static, d1.FF, d1.freeze),
       modnames = c("frost", "grow", "north", "static", "frost.freeze", "freeze"))
write.csv(d1.res,file =  file.path("D://", "Dropbox", 'winTor_aux', "Results", 'd1AICtable.csv'), row.names = F)


## Two Variables
d2.north.frost <- lm(formula = Duration ~ NA_nFrostyDays + NA_northing, data = env.df)
summary(d2.north.frost)

d2.north.grow <- lm(formula = Duration ~ NA_nonGrowingDays + NA_northing, data = env.df)
summary(d2.north.grow)

d2.north.static <- lm(formula = Duration ~  NA_staticWinter + NA_northing, data = env.df)
summary(d2.north.static)

d2.north.dem <- lm(formula = Duration ~  NA_dem + NA_northing, data = env.df)
summary(d2.north.dem)

d2.north.ff <- lm(formula = Duration ~  NA_frostFreeze + NA_northing, data = env.df)
summary(d2.north.ff)

d2.north.freeze <- lm(formula = Duration ~  NA_nDaysFreeze + NA_northing, data = env.df)
summary(d2.north.freeze)

#AIC Table
d2.res <- aictab(list(d2.north.frost, d2.north.grow, d2.north.static, d2.north.dem, d2.north.ff, d2.north.freeze),
       modnames = c("frost", "grow", "static", "dem", "frost.freeze", "freeze"))
write.csv(d2.res, file = file.path("D://", "Dropbox", 'winTor_aux', "Results", "d2AICtable.csv"), row.names = F)

## Three Variables
d3.north.dem.frost <- lm(formula = Duration ~ NA_nFrostyDays + NA_northing + NA_dem, data = env.df)
summary(d3.north.dem.frost)

d3.north.dem.grow <- lm(formula = Duration ~ NA_nonGrowingDays + NA_northing + NA_dem, data = env.df)
summary(d3.north.dem.grow)

d3.north.dem.static <- lm(formula = Duration ~  NA_staticWinter + NA_northing + NA_dem, data = env.df)
summary(d3.north.dem.static)

d3.north.dem.ff <- lm(formula = Duration ~  NA_frostFreeze + NA_northing + NA_dem, data = env.df)
summary(d3.north.dem.ff)

d3.north.dem.freeze <- lm(formula = Duration ~  NA_nDaysFreeze + NA_northing + NA_dem, data = env.df)
summary(d3.north.dem.freeze)

aictab(list(d3.north.dem.frost, d3.north.dem.grow,d3.north.dem.static, d3.north.dem.ff, d3.north.dem.freeze),
       modnames = c("frost", "grow", "static", "frostFreeze", "freeze"))

## Interaction models
d2i.north.dem <- lm(formula = Duration ~  NA_dem * NA_northing, data = env.df)
summary(d2i.north.dem)

d3i.north.dem.ff <- lm(formula = Duration ~  NA_frostFreeze + NA_northing * NA_dem, data = env.df)
summary(d3i.north.dem.ff)

d3i.north.dem.frost <- lm(formula = Duration ~  NA_nFrostyDays + NA_northing * NA_dem, data = env.df)
summary(d3i.north.dem.frost)

d3i.north.dem.grow <- lm(formula = Duration ~  NA_nonGrowingDays + NA_northing * NA_dem, data = env.df)
summary(d3i.north.dem.grow)

d3i.north.dem.freeze <- lm(formula = Duration ~  NA_nDaysFreeze + NA_northing * NA_dem, data = env.df)
summary(d3i.north.dem.freeze)

aictab(list(d2i.north.dem, d3i.north.dem.ff, d3i.north.dem.frost, d3i.north.dem.grow, d3i.north.dem.freeze),
       modnames = c("dem","frostFreeze", "frost", "grow", "freeze"))

#### Predicting ####

##d1.freeze
frz.df <- as.data.frame(env$NA_nDaysFreeze) # make to dataframe
frz.df$cell <- 1:nrow(frz.df)
df.predict <- predict.lm(d1.freeze, newdata = frz.df, interval = "predict", type = "response")
summary(df.predict)
res.df <- cbind(frz.df, df.predict)


#lets get this back into a raster
empty.raster <- env$NA_dem
empty.raster[] <- NA_real_

empty.raster[res.df$cell] <- res.df$fit
plot(empty.raster)
