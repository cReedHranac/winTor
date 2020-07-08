#################################################
####  Plotting for WinTor Publication        ####
####  Author: C. Reed Hranac                 ####
####  Tested                                 ####
#################################################

## This script provides all the plotting code for the wintor publication
##  Contents:
  ## Main text figures:
  ## figure 1: Data type and location
  ## figure 2: Predicted winter duration across North America
  ## figure 3: Predicted body mass and body fat
  ## figure 4: Predicted hibernation survival 
  ## figure 5: Increased energry expenature due to WNS infection

  ## Si figures:

#### Set Up ####
env.prior <- ls()

## libraries
library(tidyverse); library(sf); library(raster)

## Creating North America Geo-political boundries for backround. 
# 
# ## creating North America political boundries 
# ## create these from the tools in the raster package
# canada <- getData("GADM",country="CAN",level=1)
# usa <- getData("GADM",country="USA", level=1)
# mexico <- getData("GADM",country="MEX", level=1)
# North.America <- rbind(canada,usa,mexico)
# 
# ## using the sp and rgdal packages is a bit simplier during this bit but I 
# ## generally will try to use the sf package for the rest
# 
# ## crop and mask to the study extent
# study.raster <- raster("data/NA_mat.tif")
# NA.crop <- crop(North.America, study.raster)
# 
# ## change to the UTM for astetic reasons
# NA.utm <- spTransform(North.America, CRS("+init=epsg:2955"))
# 
# ## write out
# library(rgdal)
# writeOGR(NA.utm,
#          dsn = file.path(win.dat, "shapeFiles"),
#          layer = "NorthAmerica",
#          driver = "ESRI Shapefile")
# 
# North.America <- st_read(win.dat, layer="NorthAmerica")
# 
# ## mylu distribution subset from the IUCN dataset
# mylu.dist <- st_read(file.path(win.dat, "shapeFiles"), 
#                      layer = "myotis_lucifugus")
# 
# ## assign the correct proj4
# st_crs(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# ## transform to the utm proj4
# mylu.utm <- st_transform(mylu.dist, 2955)
# 
# st_write(mylu.utm,
#          dsn = file.path(win.dat, "shapeFiles"),
#          layer = "MYLU_utm",
#          driver = "ESRI Shapefile")
# env.post <- ls()
# to.remove <- env.post[env.post %!in% env.prior]
# rm(list=to.remove); rm(env.post, to.remove)

## after these are all generated the first time you can comment out all the 
## previous lines