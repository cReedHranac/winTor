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

## plots 
library(ggplot2)

mass.plot <- function(x, save.name,  res.agg = 25, save = F, ...){
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  
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
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = predMass),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nBody\nMass (g)",
                         colors = c("#5e3c99","#b2abd2", "#ffffff","#fdb863", "#e66101" ),
                         limits=  c(5,16)) + 
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    #ggtitle("Predicted Body Mass") + 
    theme_bw()
  
  if(save == T){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
           g.mass, width = 7, height = 7/aspect.ratio,
           ...)
  }
  return(g.mass)
}
fat.plot <- function(x, save.name,  res.agg = 25, save = F, ...){
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  
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
                         limits=  c(0,6)) + 
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    #ggtitle("Predicted Body Mass") + 
    theme_bw()
  
  if(save == T){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
    g.fat, width = 7, height = 7/aspect.ratio,
    ...)}
  return(g.Fat)
}

m.test <- mass.plot(x = mass.rast)
f.test <- fat.plot(x= fat.rast)


#### Look at predicted body fat for survival ####
null.fat <- raster(file.path(win.res, "MYLU_fat.null.tif"))
inf.fat <- raster(file.path(win.res, "MYLU_fat.inf.tif"))

requiredFat.plot <- function(x, save.name, inf, res.agg = 25, save = F, ...){
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  
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
                         limits=  c(0,33)) + 
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    ggtitle(Tstring) + 
    theme_bw()
  
  if(save == T){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
    g.fat, width = 7, height = 7/aspect.ratio,
    ...)}
  return(g.Fat)
}

n.test <- requiredFat.plot(x = null.fat, inf = F)
i.test <- requiredFat.plot(x = inf.fat, inf = T)
## predicted fat reserves - fat required
null.survive <- fat.rast - null.fat
inf.survive <- fat.rast - inf.fat

survivalFat.plot <- function(x, save.name, inf, res.agg = 25, save = F, ...){
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  
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
                         limits=  c(-31,3)) + 
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    ggtitle(Tstring) + 
    theme_bw()
  
  if(save == T){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path(win.res, save.name),
           g.fat, width = 7, height = 7/aspect.ratio,
           ...)}
  return(g.Fat)
}

ns.test <- survivalFat.plot(x = null.survive, inf = F)
is.test <- survivalFat.plot(x = inf.survive, inf = T)
