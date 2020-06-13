#### wintorLength geoMunge ####
#### author: C.Reed Hranac ####

## tested date: 13 June 2020

  ## Create spatial layers for project
  ## Set to North American extent and resolution
  ## write out into external data location


## libraries used within
library(raster); library(rgdal)

## take snapshot of whats in the environment before
env.prior <- ls()


## Create layer for extent, resolution and projection 
## this segment creates the data projection used throughout the project
## based on the standard projection and resolution used in the bioclim products
## available through the raster::getData function. For ease I have put a 
## template raster in the data directory for this project that has already 
## been cropped to the extent

# mat <- raster(file.path(worldClim, "bio_1.bil")) ## mean anual temperature
# mat.corrected <- calc(mat, function(x) x/10) #correcting the units from source
# NA.extent <- c(-172.3, -52,23.5,66.5) #extent of NA from artic circle to tropics
# mat.cropped <- crop(mat.corrected, NA.extent) #crop to NA extent
# rm(mat, mat.corrected) # clean

## template raster
mat <- raster("data/NA_mat.tif")

## Handling the NA Climate layers used
NA.raster <- raster(file.path(NA.Ref, "ClimateNA_ID.asc"))
lcc.proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
proj4string(NA.raster) <- lcc.proj


## Number of frosty days (Inverse of the NFFD layer)
nffd <- raster(file.path(NA.Clim, "NFFD.asc"))
proj4string(nffd) <- lcc.proj
nfd <- calc(nffd, fun = function(x) 365-x) #switch to nfrostyDays
names(nfd) <-  "nFrostyDays"
rm(nffd)

## Non-growing days (inverse of dd5)
dd5 <- raster(file.path(NA.Clim, "DD5.asc"))
proj4string(dd5) <- lcc.proj
ngd <- calc(dd5, fun = function(x) 365-(x/10000)*365) #Correct from % year and get inverse
names(ngd) <- "nonGrowingDays"
rm(dd5)

## Number of freezing days per year (dd0)
dd0 <- raster(file.path(NA.Clim, "DD_0.asc"))
proj4string(dd0) <- lcc.proj
names(dd0) <- "nFreezeDays"

## Latitude raster layer
northing <- init(mat,
                 fun = "y")
names(northing) <- "northing"

## DEM
NA.dem <- raster(file.path(NA.Ref, "ClimateNA_DEM.asc"))
proj4string(NA.dem) <- lcc.proj
names(NA.dem) <- "dem"



## stack all together
winTor <- stack(nfd, ngd, dd0)

## Match projection, and resolution then write out
wintor.p <- projectRaster(winTor, mat,
                          filename = file.path(win.dat, "NA_.tif"),
                          format = "GTiff",
                          bylayer = T,
                          suffix = "names",
                          overwrite = T) 

## the reference layers are a bit different so they need to be handled
## individually

## Northing layer
writeRaster(northing,
            file.path(win.dat, "NA_norhting.tif"),
            format = "GTiff", 
            overwrite = T)

## DEM layer
projectRaster(NA.dem, mat,
              file.path(win.dat, "NA_dem.tif"),
              format = "GTiff",
              overwrite = T)

## Doesn't run as is but how I created the NA_OG1k layer
## Get old data layer to same res and such WATCH OUT RAM KILLER
## "original" data layer as described in Hayman et al. 2016 and associated 
## git repo avaliable at github.com/dtsh2/
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
# writeRaster(OG.1k, filename = file.path(win.dat, "OG1k.tif"), format = "GTiff")

## Species distribution
## since Myotis lucifugus is the only species used in this work we can go ahead
## and create a subset of the IUCN species distributions and  stash it for
## later use

library(sf)
mam <- st_read(mam.dist, 
               layer = "TERRESTRIAL_MAMMALS")
mylu <- mam[mam$binomial == "Myotis lucifugus",]
st_write(obj = mylu,
         dsn = win.dat,
         layer = "myluDist",
         driver = "ESRI Shapefile")


#### Clean up script items ####
env.post <- ls()
to.remove <- env.post[env.post %!in% env.prior]
rm(list=to.remove); rm(env.post, to.remove)
