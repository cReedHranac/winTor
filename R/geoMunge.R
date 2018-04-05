## wintorLength geoMunge ###
  ## Need to create some of the rasters for the testing.
  ## Set to North American extent

##Links
NA.Clim <- file.path("D:", "NA_NORM_8110_Bioclim_ASCII")
NA.Ref <- file.path("D:", "NA_Reference_files_ASCII")
data.dir <- file.path("data")

## libraries
library(raster); library(rgdal)

## October - April Layer
NA.raster <- raster(file.path(NA.Ref, "ClimateNA_ID.asc"))
  
#reclassify to static value
m <- c(-Inf,-1,NA,0,
       cellStats(NA.raster, max),182)
NA.182 <- reclassify(NA.raster,m, 
                     filename = file.path(data.dir, "staticWinter.tif"),
                     format = "GTiff")

## Number of frosty days (Inverse of the NFFD layer)
nffd <- raster(file.path(NA.Clim, "NFFD.asc"))
proj4string(nffd) <- proj4string(NA.raster)

nfd <- calc(nffd,
            fun = function(x) 365-x,
            filename = file.path(data.dir, "nFrostyDays.tif"),
            format = "GTiff")

## Non-growing days (inverse of dd5)
dd5 <- raster(file.path(NA.Clim, "DD5.asc"))

ngd <- calc(dd5,
            fun = function(x) 365-x,
            filename = file.path(data.dir, "nDayBelow5.tif"),
            format = "GTiff")

## Lattitude raster layer
northing <- init(NA.raster,
                 fun = "y")
northing.mask <- mask(northing, NA.182,
                      filename = file.path(data.dir, "norhting.tif"),
                      format = "GTiff")
## DEM
NA.dem <- raster(file.path(NA.Ref, "ClimateNA_DEM.asc"))
writeRaster(NA.dem,
            filename = file.path(data.dir, "dem.tif"),
            format = "GTiff")
