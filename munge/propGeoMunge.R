## winTor length geoPropMunge ###
  ## create proportional layers for project attempt 2
  ## set to North American extent and 1k resolution

##Links
NA.Clim <- file.path("D:", "NA_NORM_8110_Bioclim_ASCII")
NA.Ref <- file.path("D:", "NA_Reference_files_ASCII")
data.dir <- file.path("D:/", "Dropbox","winTor_aux" , "data")
worldClim <- file.path("D:", "WorldClim", "bclim")

## libraries
library(raster); library(rgdal)

## pull in base layer for cropping and extent
mat <- raster(file.path(data.dir, "NA_mat.tif"))


## Oct - April Static layer
static <- mat
static[] <- 182/365
static.mask <- mask(static, mat)
names(static.mask) <- "staticWinter"
writeRaster(static.mask, file.path(data.dir, "Prop_staticWinter.tif"), format = "GTiff", overwrite = T)


eu.84 <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
## Number of frosty days (Inverse of the NFFD layer)
nffd <- raster(file.path(NA.Clim, "NFFD.asc"))
proj4string(nffd) <- eu.84
prop.frost.days <- calc(nffd, function(x) (365-x)/365) #flop and create proportion
names(prop.frost.days) <- "nFrostyDays"
rm(nffd)

## Non-growing days (inverse of dd5)
dd5 <- raster(file.path(NA.Clim, "DD5.asc"))
proj4string(dd5) <- eu.84
n.grow.days <- calc(dd5, fun = function(x) 1-x/10000) # inverse and correct scale
names(n.grow.days) <- "nonGrowingDays"
rm(dd5)

## Number freezing days
dd0 <- raster(file.path(NA.Clim, "DD_0.asc"))
proj4string(dd0) <- eu.84
freeze.days <- calc(dd0, function(x) x/10000)
names(freeze.days) <- "nFreezingDays"
rm(dd0)

## Frost freeze 
ff.stk <- stack(prop.frost.days, freeze.days)
frost.freeze <- calc(ff.stk, function(x) x[[2]]+ .5*(x[[1]] - x[[2]]))
names(frost.freeze) <- "frostFreeze"


#stack, reporject and wite
win.stk <- stack(prop.frost.days,n.grow.days,freeze.days,frost.freeze)
rm(prop.frost.days,n.grow.days,freeze.days,frost.freeze)
win.p <- projectRaster(win.stk, mat)
writeRaster(win.p, 
            filename = file.path(data.dir, "Prop"),
            format = "GTiff",
            bylayer = T, 
            suffix = "names")