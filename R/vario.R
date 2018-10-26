#### variogram attempt

## libraries

## Extra Paths
if (!exists('base.path')) {
  if(.Platform$"OS.type" == "windows"){
    base.path = file.path("D:", "Dropbox", "wintor_aux")
  } else {
    base.path = "~/Dropbox/winTor_aux"
  }
}

win.dat <- file.path(base.path, "data")
win.res <- file.path(base.path, "Results")


#### Data ####

env.df <- read.csv("data/modelingDataFrame.csv")
library(sp); library(tidyverse); library(raster)
dat.mini <- env.df %>%
  dplyr::select(lat, long, winter.duration)
coordinates(dat.mini) <- ~long + lat


library(rgdal)
## Co-variates
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_OG1k")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)
dat.rast <- rasterize(dat.mini, y = env.stk)

mondo.stk <- stack(dat.rast$winter.duration, env.stk)

dat.mondo <- rasterToPoints(mondo.stk)

library(gstat)
# use best formula from linear selection? 
  # winter.duration ~ NA_dem + NA_northing + NA_nDaysFreeze
v <- variogram(winter.duration ~ NA_dem + NA_northing + NA_nDaysFreeze, data = dat.mondo)
fit.variogram(v, vgm("Exp", "Mat", "Sph"), fit.kappa = T)



library(sp)
demo(meuse, ask = FALSE, echo = FALSE)
head(meuse)
