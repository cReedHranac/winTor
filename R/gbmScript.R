#######################################
### GMB modling script for winTor###
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

##packages
library(tidyverse);library(gbm); library(raster)

#### Data ####
dur <- read.csv("data/durationDataReferenced.csv")
mass <- read.csv("data/massDataReferenced.csv")

## Co-variates
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_OG1k")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)

## ammending to have co-variate data
coordinates(mass) <- ~ Long + Lat
proj4string(mass) <- proj4string(env.stk)
mass.df <- as.data.frame(cbind(mass, raster::extract(env.stk, mass)))

coordinates(dur) <- ~ Long + Lat
proj4string(dur) <- proj4string(env.stk)
dur.df <- as.data.frame(cbind(dur, raster::extract(env.stk, dur)))

##Duration

mass.df <- mass.df[-which(mass.df$avgMass==14.5),]

#### Linear model application ####

dur.mods <- lapply(mod.form("winter.duration",coVar = env.names),
                   FUN = lm, data = dur.df )

mass.mods <- lapply(mod.form("avgMass",coVar = env.names),
                    FUN = lm, data = mass.df )

##atmepting to solve the subscript out of bounds error
## Critical for model to operate apparently
dur.sub <- dur.df %>%
  dplyr::select(winter.duration,NA_dem, NA_northing, NA_nFrostyDays)


## set seed
set.seed(9)
## tain model
dur.fit <- gbm(
  formula = winter.duration ~ .,
  distribution = "gaussian",
  data = dur.sub,
  n.trees = 5000,
  interaction.depth = 3, 
  bag.fraction = 2,
  shrinkage = 0.05,
  cv.folds = 10
)

## print and examine
print(dur.fit)
sqrt(min(dur.fit$cv.error))
gbm.perf(dur.fit, method = "cv")
