### Plotting 

## libraries
library(raster);library(tidyverse)
library(lubridate); library(broom)
library(AICcmodavg); library(gridExtra)

## Extra Paths
if (!exists('base.path')) {
  if(.Platform$"OS.type" == "windows"){
    base.path = file.path("D:", "Dropbox", "wintor_aux")
  } else {
    base.path = "~/Dropbox/winTor_aux"
  }
}


win.dat <- file.path(base.path, "data")
win.res <- file.path(base.path, "Results")


#### Functions ####
wintTorHighRes <- function(x, save, id,  ...){
  ## Function for plotting high resolution maps of models results
  m <- rbind(c(-1000, 0, 0),
             c(365, 1000, 365))
  x.clean <- reclassify(x, rcl = m)
  x.pts <- data.frame(rasterToPoints(x.clean))
  colnames(x.pts) <- c("long", "lat", "Winter")
  
  win.plot <- ggplot(x.pts) +
    aes(x = long, y = lat) +
    coord_fixed(xlim = extent(x)[1:2],ylim=extent(x)[3:4])+
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(data = x.pts, aes(fill = Winter),  interpolate = T) +
    scale_fill_gradientn("Winter\nLength\n(days)",
                         colors = c("#e66101", "#fdb863","#ffffff", "#b2abd2", "#5e3c99")) + #purp low orange hi
    ggtitle(names(x)) + 
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  if(save == T){
    ggsave(filename = file.path(win.res, id),
           win.plot, ...)
    
    return(win.plot)
  }
  
}

wintorResid <- function(x, id, res.agg = 25,  save = F, ...){
  ## Function to plot the residuals across on the map
  
  ## Create DataFrame
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  
  x.class <- calc(x.ag, function(x) {x[is.na(x)] <- NA; as.factor(round(x/30))}) #make month factors 
  m <- rbind(c(-1000, 0, 0),
             c(0,2,2), 
             c(2,4,4),
             c(4,6,6),
             c(6,8,8),
             c(8,10,10),
             c(10,12,12),
             c(12, 1000, 12))
  
  rast.trimmed <- reclassify(x.class, m) #trim ends
  x.pts <- rasterToPoints(rast.trimmed) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "Winter")
  x.df$Winter <- as.factor(x.df$Winter)
  
  g.winTor <- ggplot() +
    aes(x = long, y = lat) +
    coord_fixed(xlim = extent(x)[1:2],ylim=extent(x)[3:4])+
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(data = x.df, aes(fill = Winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_brewer(palette="Spectral", na.value="white") +
    #general malarkey
    ggtitle(names(x)) + 
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  if(save == T){
    ggsave(filename = file.path(win.res, paste0(id, names(x))),
           g.winTor, ...)
  }
  
  return(g.winTor)
}

#### Plot data occurence ####
NA.extent <- c(-160,-52,23.5,66.5)
aspect.ratio <- (NA.extent[[2]] - NA.extent[[1]])/(NA.extent[[4]] - NA.extent[[3]])

env.df <- as_tibble(fread("data/modleingDataFrame.csv"))

lit.dat <- env.df[1:7,]
rae.dat <- env.df[8:nrow(env.df),]

##Lit points
lit.points <- ggplot() +
  aes(x = long, y = lat) +
  coord_fixed(xlim = extent(NA.extent)[1:2],ylim=extent(NA.extent)[3:4])+
 
  #border lines
  borders("world",
          xlim=extent(NA.extent)[1:2],ylim=extent(NA.extent)[3:4],
          colour = "grey20",
          fill = "grey80")+
  geom_text(data= lit.dat, aes(x=long, y=lat, label = winter.duration), size = 4) +
  theme_bw()+
  ggtitle("Literature Points")

ggsave(filename = file.path(win.res, "litPoints.png"),
       plot = lit.points, device = "png", 
       height = 7/aspect.ratio, width = 7, units = "in")

## Rae points

rae.wide <- ggplot() +
  aes(x = long, y = lat) +
  coord_cartesian(xlim = extent(NA.extent)[1:2],ylim=extent(NA.extent)[3:4])+
  #border lines
  borders("world",
          xlim=extent(NA.extent)[1:2],ylim=extent(NA.extent)[3:4],
          colour = "grey20",
          fill = "grey80")+
  geom_point(data= rae.dat, aes(x=long, y=lat)) +
  theme_bw()+
  ggtitle("WCS Points")

summary(rae.dat[,c("long", "lat")])
rae.ext <- c(-135, -110, 47.5,60)

rae.zoom <- ggplot() +
  aes(x = long, y = lat) +
  coord_cartesian(xlim = extent(rae.ext)[1:2],ylim=extent(rae.ext)[3:4])+
  #border lines
  borders("world",
          xlim=extent(rae.ext)[1:2],ylim=extent(rae.ext)[3:4],
          colour = "grey20",
          fill = "grey80")+
  geom_text(data= rae.dat, aes(x=long, y=lat, label = winter.duration), size = 2.5) +
  theme_bw()+
  ggtitle("WCS Points (Zoom)")

rae.points <- grid.arrange(rae.wide, rae.zoom)

ggsave(filename = file.path(win.res, "WCSPoints.png"),
      plot = rae.points, device = "png", 
      height = 2*(7/aspect.ratio), width = 7, units = "in")

#### Plot differences in top 5 predictions ####
top.5 <- list("f3Pred_frost",
              "f2Pred_frostFreeze",
              "f3Pred_frostFreeze",
              "f2Pred_freeze",
              "f2Pred_frost",
              "NA_OG1k")



t5.raw <- stack(file.path(win.res, paste0(top.5,".tif")))


## clean of values that are too loud
m <- rbind(c(-1000, 0, 0),
           c(365, 1000, 365))
t5.stk <- reclassify(t5.raw, rcl = m)
t5.agg <- aggregate(t5.stk, fact = 25)
rm(t5.raw, t5.stk)

## plot combine and display
t5.dat <- data.frame(rasterToPoints(t5.agg))
colnames(t5.dat) <- c( "long", "lat", top.5)
res.long <- tidyr::gather(data = t5.dat, key = "Model", value = "Winter.Length", -c(long, lat), factor_key = T)
NA.extent <- c( -172.3 ,-52,23.5,66.5)
aspect.ratio <- (NA.extent[[2]] - NA.extent[[1]])/(NA.extent[[4]] - NA.extent[[3]])

##Plot ### Needs more ram...
ggplot(res.long, aes(x = long, y = lat))+
  coord_cartesian(xlim = NA.extent[1:2], 
                  ylim = NA.extent[3:4]) +
  #border lines
  borders("world",
          xlim=NA.extent[1:2],ylim=NA.extent[3:4],
          colour = "grey20",
          fill = "grey80")+
  #Raster fill
  geom_raster( aes(fill = Winter.Length),  interpolate = T) +
  scale_fill_gradientn("Winter\nLength\n(days)",
                       colors = c("#e66101", "#fdb863","#ffffff", "#b2abd2", "#5e3c99")) + #purp low orange hi
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw() +
  facet_wrap(~ Model, ncol = 2)

rm(list = ls())
#### Infection Maps ####
## mylu dat
my.dat <- stack(file.path(win.res, c("myluR_max.inf.tif", "myluR_max.null.tif")))
top.5 <- list("f3Pred_frost",
              "f2Pred_frostFreeze",
              "f3Pred_frostFreeze",
              "f2Pred_freeze",
              "f2Pred_frost",
              "NA_OG1k")

t5.raw <- stack(file.path(win.res, paste0(top.5,".tif")))
m <- rbind(c(-1000, 0, 0),
           c(365, 1000, 365))
t5.stk <- reclassify(t5.raw, rcl = m)
names(t5.stk) <- top.5
rm(t5.raw)

surv.list <- list()

for(i in 1:2){
  for(j in 1:nlayers(t5.stk)){
    foo <- my.dat[[i]] - t5.stk[[j]]
    names(foo) <- paste0(names(t5.stk[[j]]),"_", substring(names(my.dat[[i]]), 11,13))
    surv.list <- append(surv.list, foo)
  }
}
surv.stk <- do.call(stack, surv.list)
rm(surv.list)
stk.agg <- aggregate(surv.stk, fact = 25)
names(stk.agg) <- names(surv.stk);rm(surv.stk)
surv.dat <- data.frame(rasterToPoints(stk.agg))
colnames(surv.dat) <- c( "long", "lat", names(stk.agg))


res.long <- tidyr::gather(data = surv.dat, key = "Model", value = "Survival", -c(long, lat), factor_key = T)
NA.extent <- c( -172.3 ,-52,23.5,66.5)
aspect.ratio <- (NA.extent[[2]] - NA.extent[[1]])/(NA.extent[[4]] - NA.extent[[3]])
res.long$Model <- as.string(res.long$Model)


ggplot(res.long, aes(x = long, y = lat))+
  coord_cartesian(xlim = NA.extent[1:2], 
                  ylim = NA.extent[3:4]) +
  #border lines
  borders("world",
          xlim=NA.extent[1:2],ylim=NA.extent[3:4],
          colour = "grey20",
          fill = "grey80")+
  #Raster fill
  geom_raster( aes(fill = Survival),  interpolate = T) +
  scale_fill_gradientn("Survial \nCapacity \n(days)",
                       colors = c("#e66101", "#fdb863","#ffffff", "#b2abd2", "#5e3c99")) + #purp low orange hi
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw() +
  facet_wrap(~ Model, ncol = 4)

#### Working on histograms ####






frosty <- lm(formula = winter.duration ~ NA_northing + NA_dem + NA_nFrostyDays, data = env.df)

env.df$Resid <- frosty$residuals #extracted f3 model object

## data layer
frosty.rast <- raster(file.path(win.res, "f3Pred_frost.tif"))
extent(frosty.rast)

frsty.pic <- wintorResid(x = frosty.rast, id = NULL)
frsty.pic + geom_text(data = env.df, aes(x= long, y = lat, label = round(Resid))) + 
  




wintTorHighRes(x = frosty.rast, save = T, id = "f3Frost", device = "pdf")
