#### Bringing things togather ####

#### Extra Paths ####
if (!exists('base.path')) {
  if(.Platform$"OS.type" == "windows"){
    base.path = file.path("D:", "Dropbox", "wintor_aux")
  } else {
    base.path = "~/Dropbox/winTor_aux"
  }
}

win.dat <- file.path(base.path, "data")
win.res <- file.path(base.path, "Results")


####Top body fat layer ####
library(raster)
mass.rast <- raster(file.path(win.res, "fat3Pred_freeze.tif"))
fat.rast <- calc(mass.rast, function(x) {-2.840036 + .59321*x})
library(rgdal)
mylu.dist <- readOGR(dsn = "D:/Dropbox/batwintor_aux/paramFiles/ShapeFiles", 
                     layer = "myotis_lucifugus")


## plots 
library(ggplot2)

mass.plot.resid <- function(x, resid.df = NULL, save.name = NULL, dist.map = NULL, res.agg = 25, save = F, ...){
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
  }
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "predMass")
  
  g.mass <- ggplot(data = x.df, aes(x = long, y = lat, z = predMass)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80") +
    #Raster fill
    geom_raster(aes(fill = predMass),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nBody\nMass (g)",
                         colors = c("#5e3c99","#b2abd2", "#ffffff","#fdb863", "#e66101" ),
                         limits=  c(floor(minValue(x.ag)),
                                    ceiling(maxValue(x.ag)))) + 
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    #ggtitle("Predicted Body Mass") + 
    theme_bw()
  
  ## distribution flag
  if(!is.null(dist.map)){
    g.mass <- g.mass +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(resid.df)){
    g.mass <- g.mass +
      geom_text(data = resid.df, aes(x = Long,
                                     y = Lat,
                                     label = round(resid, 2)),
                size = 2,
                inherit.aes = F,
                position=position_jitter(width=2,height=2))
    
  }
  
  if(!is.null(save.name)){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
           g.mass, width = 7, height = 7/aspect.ratio,
           ...)
  }
  return(g.mass)
}
fat.plot <- function(x, save.name = NULL, res.agg = 25,  dist.map = NULL, ...){
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
  colnames(x.df) <- c("long", "lat", "predFat")
  
  g.Fat <- ggplot(data = x.df, aes(x = long, y = lat, z = predFat)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = predFat),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nBody\nFat (g)",
                         colors = c("#5e3c99","#b2abd2", "#ffffff","#fdb863", "#e66101" ),
                         limits=  c(floor(minValue(x.ag)),
                                    ceiling(maxValue(x.ag)))) +
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    #ggtitle("Predicted Body Mass") + 
    theme_bw()
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    g.Fat <- g.Fat +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
    g.fat, width = 7, height = 7/aspect.ratio,
    ...)}
  
  return(g.Fat)
}
## If you want to look at the residuals on the mass plot you can create the df through:
## Note the item generation is contained within the fatLM.R script

# best <- fat3.list[[3]]
# mod <- rapid.lm.clean(best) ## 6 points are romoved. but whitch 6?
# 
# mod.df <- env.df %>%
#   left_join( cbind(resid = mod$residuals, mod$model), "NA_nDaysFreeze") %>%
#   dplyr::filter(!is.na(avgMass.y)) %>%
#   select(Long, Lat, resid)

m.test <- mass.plot.resid(x = mass.rast, dist.map = mylu.dist)
f.test <- fat.plot(x= fat.rast, dist.map = mylu.dist)


#### Look at predicted body fat for survival ####
null.fat <- raster(file.path(win.res, "MYLU_fat.null.tif"))
inf.fat <- raster(file.path(win.res, "MYLU_fat.inf.tif"))

requiredFat.plot <- function(x, save.name = NULL, dist.map = NULL,  inf, res.agg = 25,  ...){
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
  colnames(x.df) <- c("long", "lat", "predFat")
  
  ## title logic
  if( inf ==T ){
   Tstring <- "Predicted body fat required for surival with infection" 
  } else{
    Tstring <- "Predicted body fat required for surival without infection" 
  }
  
  g.Fat <- ggplot(data = x.df, aes(x = long, y = lat, z = predFat)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = predFat),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nBody Fat\nRequired (g)",
                         colors = c("#5e3c99","#b2abd2", "#ffffff","#fdb863", "#e66101" ),
                         limits=  c(floor(minValue(x.ag)),
                                    ceiling(maxValue(x.ag)))) +
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    ggtitle(Tstring) + 
    theme_bw()
  ## distribution flag
  if(!is.null(dist.map)){
    g.Fat <- g.Fat +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
    g.fat, width = 7, height = 7/aspect.ratio,
    ...)}
  return(g.Fat)
}

n.test <- requiredFat.plot(x = null.fat, dist.map = mylu.dist, inf = F)
i.test <- requiredFat.plot(x = inf.fat, dist.map = mylu.dist, inf = T)


## predicted fat reserves - fat required
null.survive <- fat.rast - null.fat
inf.survive <- fat.rast - inf.fat

survivalFat.plot <- function(x, save.name = NULL, dist.map = NULL,  inf, res.agg = 25,  ...){
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
  colnames(x.df) <- c("long", "lat", "predFat")
  
  ## title logic
  if( inf ==T ){
    Tstring <- "Predicted body fat remaining after winter with infection" 
  } else{
    Tstring <- "Predicted body fat remaining after winter without infection" 
  }
  
  g.Fat <- ggplot(data = x.df, aes(x = long, y = lat, z = predFat)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = predFat),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nBody Fat\nRemaining (g)",
                         colors = c("#5e3c99","#b2abd2", "#ffffff","#fdb863", "#e66101" ),
                         limits=  c(floor(minValue(x.ag)),
                                    ceiling(maxValue(x.ag)))) +
    geom_contour(aes(z = predFat,
                     color = factor(..level.. == 0,
                                      levels = c(F, T))),
                 breaks = -31:3) + 
    scale_color_manual(values = c(NA, "black"), guide =F)+
    labs("deathline")+
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    ggtitle(Tstring) + 
    theme_bw()
  
  ## distribution flag
  if(!is.null(dist.map)){
    g.Fat <- g.Fat +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
           g.fat, width = 7, height = 7/aspect.ratio,
           ...)}
  return(g.Fat)
}

ns.test <- survivalFat.plot(x = null.survive, dist.map = mylu.dist, inf = F)
is.test <- survivalFat.plot(x = inf.survive, dist.map = mylu.dist, inf = T)



## required fat as a precent of predicted body mass
pnull <- null.fat/mass.rast
pinf <- inf.fat/mass.rast

binPrecentFat  <- function(x, save.name = NULL, dist.map = NULL,  inf, res.agg = 25,  ...){
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
  
  ## create catigorical binning
  x.class <- calc(x.ag, function(x) {x[is.na(x)] <- NA; as.factor(round(x*100))})
  # Binning matrix
  control.sequence <- c(seq(from = 0,to = signif(ceiling(maxValue(x.ag*100)),2),by = 10))
  start.sequence <- control.sequence[1:(length(control.sequence) - 1)]
  end.sequence <- control.sequence[2:(length(control.sequence))]
  m <- cbind(start.sequence, end.sequence, end.sequence)
  # reclasify
  rast.reclass <- reclassify(x.class, m)
  
  ## Convert to df
  x.pts <- rasterToPoints(rast.reclass) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "predFat")
  
  ## title logic
  if( inf ==T ){
    Tstring <- "Body fat required for survival with infection as a precentage of predicted body mass" 
  } else{
    Tstring <- "Body fat required for survival without infection as a precentage of predicted body mass" 
  }
  
  g.Fat <- ggplot(data = x.df, aes(x = long, y = lat, z = predFat)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = predFat),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Precent\nBody Fat\nof Mass\nRequired",
                         colors = c("#5e3c99","#b2abd2", "#ffffff","#fdb863", "#e66101" ),
                         limits=  c(0,
                                    signif(ceiling(maxValue(x.ag*100)),2))) +
    # geom_contour(aes(z = predFat,
    #                  color = factor(..level.. == 0,
    #                                 levels = c(F, T))),
    #              breaks = -31:3) + 
    # scale_color_manual(values = c(NA, "black"), guide =F)+
    # labs("deathline")+
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    ggtitle(Tstring) + 
    theme_bw()
  
  ## distribution flag
  if(!is.null(dist.map)){
    g.Fat <- g.Fat +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
           g.fat, width = 7, height = 7/aspect.ratio,
           ...)}
  return(g.Fat)
}

pnull.test <- binPrecentFat(x = pnull, inf = F, dist.map = mylu.dist)
pinf.test <- binPrecentFat(x = pinf, inf = T, dist.map = mylu.dist)

