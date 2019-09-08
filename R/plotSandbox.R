## Plotting script play area


library(tidyverse)

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

## Arguments going in
library(raster);library(rgdal);library(metR)
x <- raster("D:/Dropbox/wintor_aux/Results/myluCropped__surv_4_98_inf.tif")
c.string <- colorRampPalette(c(  "#e66101","#fdb863","#ffffff", "#b2abd2","#5e3c99"))
res.agg = 25
mylu.dist <- readOGR(dsn = "D:/Dropbox/batwintor_aux/paramFiles/ShapeFiles", 
                     layer = "myotis_lucifugus")
proj4string(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
dist.map = mylu.dist
North.America <- readOGR(dsn = win.dat,
                         layer = "NorthAmerica")

north.america = North.America
canada.focus = F
legend.key <- "THis is\n a test"
surv.countours = F
save.name = NULL
device.out = NULL


## Create DataFrame (aggragation is mainly for the dev period)
if(!is.null(res.agg)){ #aggratetion bits
  x.ag <- raster::aggregate(x, res.agg)
}
else{
  x.ag <- x}
## Crop and mask to distribution
if(!is.null(dist.map)){
  x.ag <- mask(crop(x.ag, dist.map), dist.map)
}

## Convert to df
x.pts <- rasterToPoints(x.ag) #to points
x.df <- data.frame(x.pts)
colnames(x.df) <- c("long", "lat", "winter")


ggplot(data = x.df, aes(x=long, y = lat, z = winter))+
  coord_fixed(xlim = extent(x.ag)[1:2], ylim = extent(x.ag)[3:4]) +
  geom_contour_fill(na.fill = -9999) +
  geom_contour_tanaka() +
  scale_fill_gradientn(legend.key,
                       colors = c.string(5),
                       # mid = "#ffffff",
                       # midpoint = 0, 
                       limits=  c(floor(minValue(x.ag)),
                                  ceiling(maxValue(x.ag)))) +
  scale_x_longitude() +
  scale_y_latitude()
  
  
  
  
  #Raster fill
  geom_raster(aes(fill = winter),  interpolate = T) +
  #oooohhhhh pretty colors
  scale_fill_gradientn(legend.key,
                       colors = col.string,
                       # mid = "#ffffff",
                       # midpoint = 0, 
                       limits=  c(floor(minValue(x.ag)),
                                  ceiling(maxValue(x.ag)))) 
    
    
    
    
    
    
    
    
    
    
    
  
###
mylu.mod %>%
    filter(Ta ==4, 
           pct.rh == 98,
           time > 4200)

test %>%
  filter(time>4200)




summary(mylu.mod$time)



a <- raster("D:/Dropbox/winTor_aux/Results/MYLU_fatRequired__4_98_fat.null.tif")
plot(a)
