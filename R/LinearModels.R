#######################################
### Liner modeling script for winTor###
#######################################

#### Extra Paths####
if (!exists('base.path')) {
  if(.Platform$"OS.type" == "windows"){
    base.path = file.path("D:", "Dropbox", "wintor_aux")
  } else {
    base.path = "~/Dropbox/winTor_aux"
  }
}

win.dat <- file.path(base.path, "data")
win.res <- file.path(base.path, "Results")
## The %!in% opperator 
'%!in%' <- function(x,y)!('%in%'(x,y))


#### Data ####
dur.df <- read.csv("data/modelingDataFrame.csv")
mass <- read.csv("data/massLocations.csv")

## Co-variates
library(raster)
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_OG1k")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)

## ammending mass to have co-variate data
coordinates(mass) <- ~ Long + Lat
proj4string(mass) <- proj4string(mass)
mass.df <- as.data.frame(cbind(mass, raster::extract(env.stk, dat.fat)))
