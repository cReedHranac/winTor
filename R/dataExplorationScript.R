#### Data exploration ####

#### Set Up ####
env.prior <- ls()

## libraries
library(tidyverse); library(raster); library(rgdal)
library(data.table); library(metR); library(gridExtra)
rasterOptions(memfrac = .3); rasterOptions(maxmemory = 1e+08) ## you'll need this


#### Data transformation ###
## read in all of the raw product layers, crop to the mylu species extent and transform to the UTM

## set up the cropping items 
## after these are all generated the first time you can comment out
## creating North America political boundries 

# canada <- getData("GADM",country="CAN",level=1)
# usa <- getData("GADM",country="USA", level=1)
# mexico <- getData("GADM",country="MEX", level=1)
# North.America <- rbind(canada,usa,mexico)
# library(rgdal)
# writeOGR(North.America,
#          dsn = file.path(win.dat, "shapeFiles"),
#          layer = "NorthAmerica.WGS",
#          driver = "ESRI Shapefile")

North.America <- readOGR(file.path(win.dat, "shapeFiles"),
                         layer="NorthAmerica.WGS")
NA.utm <- spTransform(North.America, 2955)

## mylu distribution
mylu.dist <- readOGR(file.path(win.dat, "shapeFiles"), 
                     layer = "myotis_lucifugus")

proj4string(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mylu.utm <- st_transform(mylu.dist, 2955)

## small clean
rm(North.America, mylu.dist);gc()


### Raster items
items <- c("duration_p.tif", "mass_p.tif", 
           "myluFatReq_fixed_fat.null.tif","myluFatReq_fixed_fat.inf.tif",
           "myluFatReq_best_fat.null.tif", "myluFatReq_fixed_fat.inf.tif")

i.stk <- stack(file.path(win.res, items))

## crop to the species extent
  ## done now mainly to save computational power
gc()

maskCrop <- function(x,y){
  a <- mask(crop(x,y),y)
  return(a)
}

beginCluster(2)
system.time(
ic.stk <- clusterR(i.stk, fun = maskCrop, args = list(y=mylu.dist))
)
endCluster()
  
beginCluster(2) ##can hand way too long depending on the number of cores
system.time({
  r.prob.Cluster<-clusterR(env.stk, fun=predict, args=list(model=mass_spatial,
                                                           fun=conffun,
                                                           progress='text',
                                                           index=1:3,
                                                           iter=1000))
})  
  
## Create the fat layer
## create the survival layers for fat and day
## Create the precent increase layers
## Tansform: use multicore application of function
## write out
## summary items <- aggregation stack for median


# duration is seperate due to it having a different extent
dur.rast <- raster(file.path(win.res, "duration_p.tif"))

## do a mylu cropped version too
dur.c <- raster(file.path(win.res, "myluCropped_win.tif"))

## remove values below 0 generated as an extrapolation error
dur.rast <- calc(dur.rast, function(x) ifelse(!is.na(x) & x < 0, 0, x))
dur.c <- calc(dur.c, function(x) ifelse(!is.na(x) & x < 0, 0, x))


projectRaster(dur.rast, crs = crs(NA.utm),
              filename = file.path(prod.utm, "duration_utm.tif"),
              format = "GTiff",
              overwrite = T)

projectRaster(dur.c, crs = crs(NA.utm),
              filename = file.path(prod.utm, "durationC_utm.tif"),
              format = "GTiff",
              overwrite = T)
rm(dur.rast, dur.c);gc()

m.cropped <- list.files(win.res,
                        pattern = "myluCropped_*",
                        full.names = T)

## do all the rest in a for loop to handel the memory requirements
for(i in 1:length(m.cropped)){
  ## read in
  a <- raster(m.cropped[[i]])
  names(a) <- paste0(strsplit(names(a), "_")[[1]][-c(1,2)], collapse = "_")
  a.repro <- projectRaster(a, crs = crs(mylu.utm),
                           filename = file.path(prod.utm, paste0(names(a),"_utm.tif")),
                           format = "GTiff",
                           overwrite = T)
  cat(names(a), "written at: ", timestamp(),"\n")
  rm(a, a.repro);gc()
}










## explore the best avaliable temperature layer
best.avail <- raster(file.path(win.dat, "Mylu_bestavailTF_NA.tif"))
## find the lower defended temperature in the data set
library(batwintor)
bat.params[1, "Ttormin"]
lower.than.best <- raster::calc(best.avail, function(x) x < 2)

rasterVis::levelplot(lower.than.best)
(length(lower.than.best[lower.than.best==F])/length(lower.than.best[is.na(lower.than.best)]))*100
## 

## same thing but crop to te species distribution first
library(rgdal)
mylu.dist <- readOGR(file.path(win.dat, "shapeFiles"), 
                     layer = "myotis_lucifugus")

proj4string(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
best.mylu <- mask(crop(best.avail, mylu.dist), mylu.dist)
mylu.best<- raster::calc(best.mylu, function(x) x < 2)

rasterVis::levelplot(mylu.best)
(length(mylu.best[mylu.best==T])/length(mylu.best[is.na(mylu.best)]))*100
## 4.379554

#### creating a table 
files <- c("win_utm.tif",
           "mass_utm.tif",
           "fat_utm.tif",
           "fat_null_utm.tif",
           "fat_inf_utm.tif",
           "fat_BEST_null_utm.tif",
           "fat_BEST_inf_utm.tif",
           "sFat_null_utm.tif",
           "sFat_inf_utm.tif",
           "sFat_Best_null_utm.tif",
           "sFat_Best_inf_utm.tif")

stk <- stack(file.path(win.res, "prodUTM", files))

##different extnet by one cell for some reason
other <- c("sDay_fixed_null_utm.tif",
           "sDay_fixed_inf_utm.tif",
           "sDay_best_null_utm.tif",
           "sDay_best_inf_utm.tif")
stk.o <- stack(file.path(win.res, "prodUTM", other))

inc.fixed <- (stk$fat_inf_utm/stk$fat_null_utm)*100

stk.odf <- cbind( med.ag, mean.stk, sd.stk, qu.stk, min.stk, max.stk)

## original stacks
org.files <- c(list.files(win.res, pattern = "survivalDays_fixed", full.names = T)[2:3],
               list.files(win.res, pattern = "survivalDays_best", full.names = T)[2:3])
org.stk <- stack(org.files)
org.crop <- mask(crop(org.stk, mylu.dist), mylu.dist)

org.crop <- projectRaster(org.stk, stk)

mean.stk <- cellStats(org.crop, mean)
qu.stk <- quantile(org.crop, 
                   probs = c(.05, .95))
sd.stk <- cellStats(org.crop, sd)
## to get the median it looks like we'll have to aggrate
stk.20 <- raster::aggregate(org.crop, 20)
med.ag <- cellStats(stk.20, median)
min.stk <- minValue(org.crop)
max.stk <- maxValue(org.crop)

odf <- cbind( med.ag, mean.stk, sd.stk, qu.stk, min.stk, max.stk)

death <- raster::calc(org.crop, function(x) x >= 0)
names(death) <- names(org.crop)
rasterVis::levelplot(death)
for(i in 1:4){
  a <- (length(death[[i]][death[[i]]==F])/29168764)*100  
  cat(paste(names(death[[i]]), "% = ", a, "\n"))
}

sDay_best <- org.crop[[3:4]]



stk.op <- projectRaster(stk.o, stk)


stk.full <- stack(stk, stk.op)
mean.stk <- cellStats(stk.full, mean)
qu.stk <- quantile(stk.full, 
                   probs = c(.05, .95))
sd.stk <- cellStats(stk.full, sd)
## to get the median it looks like we'll have to aggrate
stk.20 <- raster::aggregate(stk.full, 20)
med.ag <- cellStats(stk.20, median)
min.stk <- minValue(stk.full)
max.stk <- maxValue(stk.full)


Res.df <- cbind( med.ag, mean.stk, sd.stk, qu.stk, min.stk, max.stk)
write.csv(Res.df, file.path(win.res, "RasterSummaryTable.csv"))

#### Die/ No Die ####
sFat.fixed <- stack(list.files(prod.utm, "sFat_", full.names = T)[3:4])
rasterVis::levelplot(sFat.fixed)

## calculating precent that dies
fixed.bi <- raster::calc(sFat.fixed$sFat_inf_utm, function(x) x<=0)
rasterVis::levelplot(fixed.bi)
(length(fixed.bi[fixed.bi==F])/length(fixed.bi[is.na(fixed.bi)]))*100
## 0.9288195 die

fixed.bi <- raster::calc(sFat.fixed$sFat_inf_utm, function(x) x>0)
rasterVis::levelplot(fixed.bi)
length(fixed.bi[fixed.bi=F])/length(fixed.bi[!is.na(fixed.bi)]) * 100
###
###
sFat.best <- stack(list.files(prod.utm, "sFat_Best", full.names = T))
rasterVis::levelplot(sFat.best)
## calcing precent die
best.bi <- raster::calc(sFat.best$sFat_Best_inf_utm, function(x) x<=0)
rasterVis::levelplot(best.bi)
(length(best.bi[best.bi==F])/length(best.bi[is.na(best.bi)]))*100
## 1.019079



idea <- stack(fixed.bi,best.bi);names(idea) <- c("fix", "best")

idea.f <- Which(fixed.bi, cell=T)
idea.b <- Which(best.bi, cell = T)

win.f <- raster::extract(dur.rast, idea.f)
win.b <- raster::extract(dur.rast, idea.b)

min(win.f)
min(win.b)


