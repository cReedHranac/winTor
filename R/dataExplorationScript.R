#### Data exploration ####

#### Set Up ####
env.prior <- ls()

## libraries
library(tidyverse); library(raster); library(rgdal); library(sf)
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

North.America <- st_read(file.path(win.dat, "shapeFiles"),
                         layer="NorthAmerica.WGS")
NA.utm <- st_transform(North.America, 2955)

## mylu distribution
mylu.dist <- readOGR(file.path(win.dat, "shapeFiles"), 
                     layer = "myotis_lucifugus")

proj4string(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# ### Raster items
# items <- c("duration_p.tif", "mass_p.tif", 
#            "myluFatReq_fixed_fat.null.tif","myluFatReq_fixed_fat.inf.tif",
#            "myluFatReq_best_fat.null.tif", "myluFatReq_best_fat.inf.tif", 
#            list.files(win.res, "survivalDays"))
# 
# i.stk <- stack(file.path(win.res, items))
# 
# ## crop to the species extent
#   ## done now mainly to save computational power
# gc()
# 
# ic <- mask(crop(i.stk, mylu.dist), mylu.dist)
# 
# ## create the fat layer
# ic$fat <-  -2.83901 + 0.59675*ic$mass_p
# 
# ## create the survival layers for fat and day
# ic$sfat_fixed_null <- ic$fat - ic$myluFatReq_fixed_fat.null
# ic$sfat_fixed_inf <- ic$fat - ic$myluFatReq_fixed_fat.inf
# ic$sfat_best_null <- ic$fat - ic$myluFatReq_Best_fat.null
# ic$sfat_best_inf <- ic$fat - ic$myluFatReq_Best_fat.inf
# 
# ## Create the percent increase layers
# ic$percInc_fixed <- (ic$myluFatReq_fixed_fat.inf/ic$myluFatReq_fixed_fat.null)*100
# ic$percInc_best <- (ic$myluFatReq_Best_fat.inf/ic$myluFatReq_Best_fat.null)*100
# 
# names(ic) <- c("duration", "mass",
#                "fatReq_fixed_null", "fatReq_fixed_inf", "fatReq_best_null", "fatReq_best_inf",
#                "sDay_best_inf", "sDay_best_null", "sDay_fixed_inf", "sDay_fixed_null",
#                "fat",
#                "sfat_fixed_null", "sfat_fixed_inf",
#                "sfat_best_null","sfat_best_inf",
#                "percInc_fixed", "percInc_best")
# 
# 
# ## Tansform and write out
# ## out location
# prod.utm <- file.path(win.res, "prodUTM")
# ic.l <- unstack(ic)
# ## loop to project and write
# 
# for( i in 1:nlayers(ic)){
#   a<- projectRaster(from = ic.l[[i]],
#                 crs = CRS("+init=epsg:2955"),
#                 filename = file.path(prod.utm, paste0(names(ic.l[[i]]), "_utm.tif")),
#                 overwrite = T)
#   cat("layer ", names(ic.l[[i]]), " complete at: ", date(), "\n" )
#   rm(a);gc()
# }
# 
# rm(i.stk, ic, ic.l, items);gc()
## write out


## Read the layers back in and create the summary file
items <- c("duration_utm.tif", "mass_utm.tif", "fat_utm.tif",
           "fatReq_fixed_null_utm.tif", "fatReq_fixed_inf_utm.tif",
           "fatReq_best_null_utm.tif", "fatReq_best_inf_utm.tif",
           "sfat_fixed_null_utm.tif", "sfat_fixed_inf_utm.tif",
           "sfat_best_null_utm.tif", "sfat_best_inf_utm.tif",
           "sDay_fixed_null_utm.tif", "sDay_fixed_inf_utm.tif",
           "sDay_best_null_utm.tif", "sDay_best_inf_utm.tif",
           "percInc_fixed_utm.tif", "percInc_best_utm.tif")

prod.utm <- file.path(win.res, "prodUTM")
stk <- raster::stack(file.path(prod.utm, items))

## summary items <- aggregation stack for median

mean.stk <- cellStats(stk, mean)
qu.stk.1 <- quantile(stk[[1:10]], 
                   probs = c(.05, .95))
qu.stk.2 <- quantile(stk[[11:17]], 
                     probs = c(.05, .95))
qu.stk <- rbind(qu.stk.1, qu.stk.2)
sd.stk <- cellStats(stk, sd)
min.stk <- minValue(stk)
max.stk <- maxValue(stk)
## to get the median it looks like we'll have to aggrate
stk.20 <- raster::aggregate(stk, 25)
med.ag <- cellStats(stk.20, median)

stk.odf <- cbind( med.ag, mean.stk, sd.stk, qu.stk, min.stk, max.stk)
write.csv(stk.odf, 
          file.path(win.res, "rasterSummary.csv"))

rm(stk.20);gc()

#### examine the survival threshold values ####

## number of cells in the dist with values
n.cells <- length(stk$duration_utm[!is.na(stk$duration_utm)])

## proportion of cells with winter greater than 6 months
over6 <- raster::calc(stk$duration_utm, function(x) x < 181)

p.over6 <- (length(over6[over6==T])/n.cells)*100
## 51.30 % of cells

### below the survival threshold
fix.null <- raster::calc(stk$sDay_fixed_null_utm, function(x) x <= 0)
p.fix.null <- (length(fix.null[fix.null == T])/n.cells) *100
## 0

fix.inf <- raster::calc(stk$sDay_fixed_inf_utm, function(x) x <= 0)
p.fix.inf <- (length(fix.inf[fix.inf == T])/n.cells) *100
## 4.820619

best.null <- raster::calc(stk$sDay_best_null_utm, function(x) x <= 0)
p.best.null <- (length(best.inf[best.null == T])/n.cells) *100
## 0.5076172

best.inf <- raster::calc(stk$sDay_best_inf_utm, function(x) x <= 0)
p.best.inf <- (length(best.inf[best.inf == T])/n.cells) *100
## 4.74147

## stack for visualization
surv.stk <- raster::stack(fix.null, fix.inf, best.null, best.inf)
names(surv.stk) <- c("fixed null", "fixed inf", "best null", "best inf")
rasterVis::levelplot(surv.stk)


## examine for the best available temperature layers
best.avail <- raster(file.path(win.dat, "Mylu_bestavailTF_NA.tif"))
best.avail <- mask(crop(best.avail, mylu.dist), mylu.dist)
lower.than.fix <- raster::calc(best.avail, function(x) x < 4)
lower.than.tlc <- raster::calc(best.avail, function(x) x < 2)

p.lfix <- (length(lower.than.fix[lower.than.fix == T])/n.cells)*100
## 32.64112
p.ltlc <- (length(lower.than.tlc[lower.than.tlc == T])/n.cells)*100
## 6.440322
t.stk <- stack(lower.than.fix, lower.than.tlc); names(t.stk) <- c("lower than 4", "lower than 2")
rasterVis::levelplot(t.stk)

