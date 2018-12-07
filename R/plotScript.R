#### Plotting for WinTor ####

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
## The %!in% opperator 
'%!in%' <- function(x,y)!('%in%'(x,y))

#### Winter duration plots ####
library(tidyverse);library(raster)
#Winter duration raster
durationMean <- raster(file.path(win.res, "durationRaster_p.tif"))
winterColors <- colorRampPalette(c("#e0ecf4", "#9ebcda","#8856a7"))

winDuration.plot <- function(x, c.string, res.agg = 25, dist.map = NULL,
                             save.name = NULL,  device.out = NULL,  ...){
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
  
  g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nDuration\nWinter\n(Days)",
                         colors = c.string,
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
    g.win <- g.win +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  return(g.win)
}

(winMean.plot <- winDuration.plot(x = durationMean,
                                  c.string = winterColors(5)))

#### Body Mass and Fat Mass Plots####
massmean <- raster(file.path(win.res, "massRaster_p.tif"))
massColors <- colorRampPalette(c("#f7fcb9", "#31a354"))

mass.plot <- function(x, c.string, res.agg = 25, dist.map = NULL,
                             save.name = NULL,  device.out = NULL,  ...){
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
  
  g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nBody\nMass (g)",
                         colors = c.string,
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
    g.win <- g.win +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  return(g.win)
}
(massMean.plot <- mass.plot(x = massmean,
                            c.string = massColors(5)))

fatMean <- calc(massmean, fun = function(x){-2.84 + 0.593*x})
fat.plot <- function(x, c.string, res.agg = 25, dist.map = NULL,
                      save.name = NULL,  device.out = NULL,  ...){
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
  
  g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nBody\nFat (g)",
                         colors = c.string,
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
    g.win <- g.win +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  return(g.win)
}
fatColors <- colorRampPalette(c("#fff7bc","#fec44f", "#d95f0e"))

(fatMean.plot <- fat.plot(fatMean, fatColors(5)))

#### Survival mapping ####
##distribution
library(rgdal)
mylu.dist <- readOGR(dsn = "D:/Dropbox/batwintor_aux/paramFiles/ShapeFiles", 
                     layer = "myotis_lucifugus")
proj4string(mylu.dist) <- proj4string(massmean)

## Static conditions 
fatReq984.null <- raster(file.path(win.res, "MYLU_fatRequired_98_4_fat.null.tif"))
fatReq984.inf <- raster(file.path(win.res, "MYLU_fatRequired_98_4_fat.inf.tif"))

survStatic.null <- fatMean - fatReq984.null
survStatic.inf <- fatMean - fatReq984.inf

survColors <- colorRampPalette(c("#fdb863", "#e66101", "#ffffff", "#5e3c99","#b2abd2"))
Surv.plot <- function(x,  res.agg = 25, dist.map = NULL,
                      save.name = NULL,  device.out = NULL,  ...){
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
  
  g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradient2("Predicted\nBody\nFat\nRemaining (g)",
                         low = "#fdb863", mid = "#ffffff", high = "#b2abd2",
                         midpoint = 0,
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
    g.win <- g.win +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  return(g.win)
}

(staticNull.plot <- Surv.plot(survStatic.null))
(staticInf.plot <- Surv.plot(survStatic.inf))

fatSurvivalHistograms <- function(survNULL, survINF, dist.map, c.string,
                                 save.name = NULL, device.out = NULL){
  stk <- stack(survNULL, survINF); names(stk) <- c("null", "inf")
  stk.mask <- mask(crop(stk, dist.map), dist.map)
  stk.df <- na.omit(as.data.frame(values(stk.mask))) %>%
    gather("status", "fat", 1:2 ) 
  stk.med <- stk.df %>%
    group_by(status) %>%
    summarise(med = median(fat))
  
  dif.hist <- ggplot(stk.df, aes(x=fat, fill = status, color = status))+
    scale_color_manual(values = c.string) +
    scale_fill_manual(values = c.string) +
    geom_histogram(binwidth = .25, alpha = .5, position = "identity") +
    xlim(-3,3)+
    geom_vline(data = stk.med, aes(xintercept = med, color = status),
               linetype = "dashed", size = 1)+
    geom_vline(xintercept = 0)+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
           dif.hist, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  
  return(dif.hist)
  
}
survNULL <- survStatic.null
survINF <- survStatic.inf
dist.map <-mylu.dist

fatSurvivalHitograms(survStatic.null,survStatic.inf,mylu.dist, survColors(2))

####No Longer needed ####
# survColorsPOS <- colorRampPalette(c( "#ffffff", "#5e3c99","#b2abd2")) #"#fdb863", "#e66101"
# Surv.plotPOS <- function(x, c.string, res.agg = 25, dist.map = NULL,
#                      save.name = NULL,  device.out = NULL,  ...){
#   ## Create DataFrame (aggragation is mainly for the dev period)
#   if(!is.null(res.agg)){ #aggratetion bits
#     x.ag <- raster::aggregate(x, res.agg)
#   }
#   else{
#     x.ag <- x}
#   ## Crop and mask to distribution
#   if(!is.null(dist.map)){
#     x.ag <- mask(crop(x.ag, dist.map), dist.map)
#   }
#   
#   ## Convert to df
#   x.pts <- rasterToPoints(x.ag) #to points
#   x.df <- data.frame(x.pts)
#   colnames(x.df) <- c("long", "lat", "winter")
#   
#   g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
#     coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
#     #border lines
#     borders("world",
#             xlim=extent(x)[1:2],ylim=extent(x)[3:4],
#             colour = "grey20",
#             fill = "grey80")+
#     #Raster fill
#     geom_raster(aes(fill = winter),  interpolate = T) +
#     #oooohhhhh pretty colors
#     scale_fill_gradientn("Predicted\nBody\nFat\nRemaining (g)",
#                          colors = c.string,
#                          limits=  c(floor(minValue(x.ag)),
#                                     ceiling(maxValue(x.ag)))) +
#     
#     #general malarkey
#     scale_x_continuous(expand = c(0,0))+
#     scale_y_continuous(expand = c(0,0))+
#     theme(plot.title = element_text(hjust = .05))+
#     #ggtitle("Predicted Body Mass") + 
#     theme_bw()
#   
#   ##Distribution map flag
#   if(!is.null(dist.map)){
#     g.win <- g.win +
#       geom_polygon(data = fortify(dist.map),
#                    aes(long,lat, group = group),
#                    colour = "black",
#                    fill = NA,
#                    inherit.aes = F) 
#   }
#   
#   if(!is.null(save.name)){
#     if(device.out == "pdf"){
#       dev.ext <- cairo_pdf
#     } else if (device.out =="eps"){
#       dev.ext <- cairo_ps
#     } else {
#       dev.ext <- device.out
#     }
#     ex <- as.vector(extent(x))
#     aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
#     ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
#            g.win, 
#            width = 9, height = 9/aspect.ratio, unit = "in",
#            dpi = 300,
#            device = dev.ext,
#            ...)}
#   
#   return(g.win)
# }
# 
# (staticNull.plot <- Surv.plotPOS(survStatic.null, survColorsPOS(5)))
# (staticInf.plot <- Surv.plotPOS(survStatic.inf, survColorsPOS(5)))
# 

#### Surface conditions####
fatReq.null <- raster(file.path(win.res, "MYLU_fatRequired_fat.null.tif"))
fatReq.inf <- raster(file.path(win.res, "MYLU_fatRequired_fat.inf.tif"))

surfaceSurv.null <- fatMean - fatReq.null
surfaceSurv.inf <- fatMean - fatReq.inf
 #
(surfaceNull.plot <- Surv.plot(surfaceSurv.null))
(surfaceInf.plot <- Surv.plot(surfaceSurv.inf))

fatSurvivalHistSurface <- function(survNULL, survINF, dist.map, c.string,
                                 save.name = NULL, device.out = NULL){
  stk <- stack(survNULL, survINF); names(stk) <- c("null", "inf")
  stk.mask <- mask(crop(stk, dist.map), dist.map)
  stk.df <- na.omit(as.data.frame(values(stk.mask))) %>%
    gather("status", "fat", 1:2 ) 
  stk.med <- stk.df %>%
    group_by(status) %>%
    summarise(med = median(fat))
  
  dif.hist <- ggplot(stk.df, aes(x=fat, fill = status, color = status))+
    scale_color_manual(values = c.string) +
    scale_fill_manual(values = c.string) +
    geom_histogram(binwidth = .25, alpha = .5, position = "identity") +
    # xlim(-3,3)+
    geom_vline(data = stk.med, aes(xintercept = med, color = status),
               linetype = "dashed", size = 1)+
    geom_vline(xintercept = 0)+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
           dif.hist, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  
  return(dif.hist)
  
}

fatSurvivalHistSurface(surfaceSurv.null,surfaceSurv.inf,mylu.dist, survColors(2))

#### Onset and End of winter for Meredith) ####

onset <- calc(durationMean, fun = function(x){ifelse(x > 0, 365-(x/2-10), NA)})
writeRaster(onset, file.path(win.res, "winterOnset.tif"), format = "GTiff")
end <- calc(durationMean,fun = function(x){ifelse(x > 0, (x/2)+10, NA)})
writeRaster(end, file.path(win.res, "winterEnd.tif"), format = "GTiff")


writeRaster(surfaceSurv.null, file.path(win.res,"myluSurace_nul.tif"), format = "GTiff")
writeRaster(surfaceSurv.inf, file.path(win.res, "myluSurace_inf.tif"), format = "GTiff")
writeRaster(survStatic.null, file.path(win.res, "myluStatic_null.tif"), format = "GTiff")
writeRaster(survStatic.inf, file.path(win.res, "myluStatic_inf.tif"), format = "GTiff")
