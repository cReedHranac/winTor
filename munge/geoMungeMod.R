## wintorLength geoMunge ###
## Create spatial layers for project
## Set to North American extent

##Links
## Extra Paths
if (!exists('base.path')) {
  if(.Platform$"OS.type" == "windows"){
    base.path = file.path("C:/Users/chran", "Dropbox", "wintor_aux")
  } else {
    base.path = "~/Dropbox/winTor_aux"
  }
}

win.dat <- file.path(base.path, "data")
win.res <- file.path(base.path, "Results")

NA.Clim <- file.path("C:/Users/chran", "Dropbox", "NA_NORM_8110_Bioclim_ASCII")
NA.Ref <- file.path("C:/Users/chran", "Dropbox","NA_Reference_files_ASCII")
data.dir <- file.path("C:/Users/chran", "Dropbox","winTor_aux" , "data")
worldClim <- file.path("C:/Users/chran","Dropbox", "WorldClim", "bclim")
spring <- file.path(file.path(win.dat,"FBI_FLI"))

## libraries
library(raster); library(rgdal)

## Create base layer for extent, resolution and projection (also temperature for later)
mat <- raster(file.path(worldClim, "bio_1.bil"))
mat.corrected <- calc(mat, function(x) x/10) #correcting the units from source
NA.extent <- c(-172.3, -52,23.5,66.5) #extent of NA from artic circle to tropics
mat.cropped <- crop(mat.corrected, NA.extent) #crop to NA extent
rm(mat, mat.corrected) # clean

## October - April Layer #Static layer
NA.raster <- raster(file.path(NA.Ref, "ClimateNA_ID.asc"))
lcc.proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
#reclassify to static value
m <- c(-Inf,-1,NA,0,
       cellStats(NA.raster, max),182)
NA.182 <- reclassify(NA.raster,m)
names(NA.182) <- "staticWinter"
rm(NA.raster, m)

## Number of frosty days (Inverse of the NFFD layer)
nffd <- raster(file.path(NA.Clim, "NFFD.asc"))
proj4string(nffd) <- lcc.proj
nfd <- calc(nffd, fun = function(x) 365-x) #switch to nfrostyDays
names(nfd) <-  "nFrostyDays"
rm(nffd)

## Frost period (Inverse of eFFp)
## This was added after the fact so procedure is slightly different
effp <- raster(file.path(NA.Clim, "eFFP.asc"))
proj4string(effp) <- lcc.proj
fp <- calc(effp, fun = function(x) 365-x) #switch to nfrostyDays
names(fp) <-  "periodFrost"
rm(effp)
form <- raster(file.path(win.dat, "NA_dem.tif"))
projectRaster(fp,
              form,
              filename = file.path(win.dat, "periodFrost.tif"),
              format = "GTiff",
              overwrite = T)


## Non-growing days (inverse of dd5)
dd5 <- raster(file.path(NA.Clim, "DD5.asc"))
proj4string(dd5) <- lcc.proj
ngd <- calc(dd5, fun = function(x) 365-(x/10000)*365) #Correct from % year and get inverse
names(ngd) <- "nonGrowingDays"
rm(dd5)

## Lattitude raster layer
northing <- init(mat.cropped,
                 fun = "y")
names(northing) <- "northing"

## DEM
NA.dem <- raster(file.path(NA.Ref, "ClimateNA_DEM.asc"))
names(NA.dem) <- "dem"

## relative humidity for downstream
rh <- raster(file.path(NA.Clim, "RH.asc"))
proj4string(rh) <- lcc.proj
names(rh) <- "relativeHumidity"


## stack all together, crop and read out
winTor <- stack(NA.182, nfd, NA.dem, rh, ngd)
wintor.p <- projectRaster(winTor, mat.cropped) 
writeRaster(wintor.p, filename = "data/NA", format = "GTiff", bylayer = T, suffix = "names", overwrite = T)
writeRaster(mat.cropped, filename = "data/NA_mat.tif", format = "GTiff", overwrite = T)
writeRaster(northing, filename = "data/NA_northing.tif", format = "GTiff", overwrite = T)


## Make a new one because why not?
frost <- raster(file.path(data.dir,"NA_nFrostyDays.tif"))
dd0 <- raster(file.path(NA.Clim, "DD_0.asc"))
proj4string(dd0) <- lcc.proj
dd0.proj <- projectRaster(dd0, frost )
dd0.days <- calc(dd0.proj,  fun = function(x) ((x/10000)*365))
names(dd0.days) <- "nDaysFreeze"
writeRaster(dd0.days, filename = file.path(data.dir,"NA_nDaysFreeze.tif"), format = "GTiff", overwrite = T)
frost.freeze <- stack(frost, dd0.days)

frostFreeze <- calc(frost.freeze,
                    function(x) x[[2]]+ .5*(x[[1]] - x[[2]]))
names(frostFreeze) <- "NA_frostFreeze"
writeRaster(frostFreeze, file.path(data.dir,"NA_frostFreeze.tif"), format = "GTiff", overwrite = T)

## Doesn't run as is but how I created the NA_OG1k layer
## Get old data layer to same res and such WATCH OUT RAM KILLER
# old.layer <- raster("D://Dropbox/WNS2/parameterFiles/wxnightsUS.asc")
# old.nights <- raster::shift(old.layer,x= -360)
# proj4string(old.nights) <- proj4string(t5.raw)
# old.year <- calc(old.nights, function(x) x*365)
# old.crop <- crop(old.year, t5.raw)
# ##
# old.1k <- projectRaster(old.crop, t5.raw)
# names(old.1k) <- "OG_Winter"
# ## crop and mask again 
# OG.1k <- mask(crop(old.1k, t5.raw),t5.raw)
# writeRaster(OG.1k, filename = file.path(win.res, "OG1k.tif"), format = "GTiff")

# ## spring metrics These guys are only for the Continental US...
# spring.stk <- stack(list.files(file.path(win.dat,"FBI_FLI"), recursive = T, pattern = "*.tif", full.names = T))
# spring.stk[[1]]
# plot(spring.stk[[2]])
