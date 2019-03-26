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

dumb.fun <- function(x){
  if(x<0){
    y <- 1
  } else if(x==0){
    y <- 2
  } else if (x>0) {
    y <- 3
  } else{
    y <- NA
  }
  return(y)
}

##creating North America political boundries 
# can.ext <- c( -140,-104,41, 60)
# canada <- getData("GADM",country="CAN",level=1)
# usa <- getData("GADM",country="USA", level=1)
# mexico <- getData("GADM",country="MEX", level=1)
# North.America <- rbind(canada,usa,mexico)
# plot(North.America)
# writeOGR(North.America,
#          dsn = win.dat, 
#          layer = "NorthAmerica",
#          driver = "ESRI Shapefile")
library(rgdal)
North.America <- readOGR(dsn = win.dat,layer = "NorthAmerica")
mylu.dist <- readOGR(dsn = "D:/Dropbox/batwintor_aux/paramFiles/ShapeFiles", 
                     layer = "myotis_lucifugus")
proj4string(mylu.dist) <- proj4string(massmean)

#### Winter duration plots ####
library(tidyverse);library(raster);
library(gridExtra)
## Plot Function 
masterPlotter <- function(x, c.string, res.agg = 25, dist.map = NULL,
                          north.america = North.America, canada.focus = F,
                          legend.key, surv.countours = F,
                          save.name = NULL,  device.out = NULL,  ...){
  ##Function for plotting all  wintor spatil figurs   
  
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
    coord_fixed(xlim = extent(x.ag)[1:2], ylim = extent(x.ag)[3:4]) +
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn(legend.key,
                         colors = c.string,
                         limits=  c(floor(minValue(x.ag)),
                                    ceiling(maxValue(x.ag)))) +
    #border lines
    geom_polygon(data= fortify(North.America),
                 aes(long,lat,group=group),
                 color="grey20",
                 fill=NA,
                 inherit.aes = F) +
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(legend.position = c(0.1,0.40),
          legend.margin = margin(),
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank())
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    g.win <- g.win +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  ## Canada focus flag
  if(canada.focus==T){
    can.ext <- c(-140,-104,41,60)
    g.win <- g.win +
      coord_cartesian(xlim = can.ext[1:2],
                      ylim = can.ext[3:4]) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) 
  } 
  
  ## Contour flag
  if(surv.countours == T) {
    g.win <- g.win + 
      geom_contour(aes(z = winter,
                       color = factor(..level.. == 0 ,
                                      levels = c(T,F),
                                      labels = c(expression(fat=0),
                                                 expression(fat>0.5)))),
                   breaks=c(-0.5, 0,0.5)) +
      
      
      scale_colour_manual(values = c( "red","blue")) +
      labs(color = "Contours")
  }
  
  
  
  ## Save Flag  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    if(canada.focus==T){
      aspect.ratio <- (can.ext[[2]] - can.ext[[1]])/(can.ext[[4]] - can.ext[[3]])
    } else{
      aspect.ratio <-(ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])  
    }
    
    ggsave(filename = file.path(win.res,"fig", paste0(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext)}
  
  return(g.win)
}


#Winter duration raster
durationMean <- raster(file.path(win.res, "durationRaster_p.tif"))
winterColors <- colorRampPalette(c("#e0ecf4", "#9ebcda","#8856a7"))


(winMean.plot <- masterPlotter(x = durationMean,
                               c.string = winterColors(5),
                               canada.focus = F,
                               legend.key = "Predicted\nDuration\nWnter\n(Days)",
                               save.name = "winDuration_Mean",
                               device.out = "pdf"))
 #### Body Mass and Fat Mass Plots####
massmean <- raster(file.path(win.res, "massRaster_p.tif"))
massColors <- colorRampPalette(c("#f7fcb9", "#31a354"))

(massMean.plot <- masterPlotter(x = massmean,
                            c.string = massColors(5),
                            canada.focus = F,
                            dist.map = mylu.dist,
                            legend.key = "Predicted\nBody\nMass (g)",
                            save.name = "massMean_Dist",
                            device.out = "pdf"))

fatMean <- calc(massmean, fun = function(x){-2.84 + 0.593*x})
fatColors <- colorRampPalette(c("#fff7bc","#fec44f", "#d95f0e"))

(fatMean.plot <- masterPlotter(x = fatMean,
                          c.string = fatColors(5),
                          canada.focus = F,
                          legend.key = "Predicted\nBody\nFat (g)",
                          dist.map = mylu.dist,
                          save.name = "fatMean_Dist",
                          device.out = "pdf"))
## Single figure

fig3 <- grid.arrange(massMean.plot, 
                     fatMean.plot, 
                     ncol = 1)
ggsave(file.path(win.res, "fig", "Mass_Fat.pdf"),
       fig3,
       device = cairo_pdf,
       width = 9,
       height = 6.5, 
       units = "in")


#### Survival mapping ####


## Static conditions 
fatReq984.null <- raster(file.path(win.res, "MYLU_fatRequired_98_4_fat.null.tif"))
fatReq984.inf <- raster(file.path(win.res, "MYLU_fatRequired_98_4_fat.inf.tif"))

survStatic.null <- fatMean - fatReq984.null

survStatic.inf <- fatMean - fatReq984.inf

# writeRaster(survStatic.null,
#             filename = file.path(win.res, "myluSurvStativNull.tif"),
#             format = "GTiff")
# writeRaster(survStatic.inf,
#             filename = file.path(win.res, "myluSurvStativInf.tif"),
#             format = "GTiff")

survColors <- colorRampPalette(c( "#fdb863", "#e66101","#ffffff", "#b2abd2","#5e3c99"))
Surv.plot <- function(x,  res.agg = 25, dist.map = NULL,
                      north.america = North.America, canada.focus = F,
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
  if(canada.focus == T){
    can.ext <- extent(-140,-104,41,60)
    x.ag <-crop(x.ag, can.ext)
  }
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "winter")
  
  (g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
    coord_fixed(xlim = extent(x.ag)[1:2], ylim = extent(x.ag)[3:4]) +
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradient2("Predicted\nBody\nFat\nRemaining (g)",
                         low = "#fdb863", mid = "#ffffff", high = "#b2abd2",
                         midpoint = 0,
                         limits=  c(minValue(x.ag),
                                    maxValue(x.ag))) +
    #border lines
    geom_polygon(data= fortify(North.America),
                 aes(long,lat,group=group),
                 color="grey20",
                 fill=NA,
                 inherit.aes = F) +
    #contour lines
      geom_contour(aes(z = winter,
                       color = factor(..level.. == 0 ,
                                      levels = c(T,F),
                                      labels = c(expression(fat=0),
                                                 expression(fat>0.5)))),
                   breaks=c(-0.5, 0,0.5)) +

     
      scale_colour_manual(values = c( "red","blue")) +
      labs(color = "Contours")+
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    #ggtitle("Predicted Body Mass") + 
    theme_bw())
  
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
    if(canada.focus==T){
      aspect.ratio <- (can.ext[[2]] - can.ext[[1]])/(can.ext[[4]] - can.ext[[3]])
    } else{
      aspect.ratio <-(ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])  
    }
    ggsave(filename = file.path(win.res,"fig", paste0(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  return(g.win)
}
survColors.Pos <- colorRampPalette(c( "#ffffff", "#b2abd2","#5e3c99"))


(staticNull.plot <- masterPlotter(x = survStatic.null,
                              canada.focus = F,
                              dist.map = mylu.dist,
                              c.string = survColors.Pos(5),
                              legend.key = "Predicted\nBody Fat\nRemaining (g)",
                              save.name = "nullSurvive4_98_Dist",
                              device.out = "pdf"))
(staticInf.plot <- masterPlotter(x = survStatic.inf,
                             canada.focus = F,
                             dist.map = mylu.dist,
                             c.string = survColors(5),
                             legend.key = "Predicted\nBody Fat\nRemaining (g)",
                             surv.countours = T))
                             save.name = "infSurvuve4_98_Dist",
                             device.out = "pdf"))

fatSurvivalHistograms <- function(survNULL, survINF, dist.map, c.string,
                                  canada.focus = F, 
                                 save.name = NULL, device.out = NULL){
  stk <- stack(survNULL, survINF); names(stk) <- c("null", "inf")
  stk.ag <- aggregate(stk, 25)
  if(canada.focus == T){
    can.ext <- c(-140,-104,41,60)
    dist.map <- crop(dist.map, can.ext)
  }
  stk.mask <- mask(crop(stk.ag, dist.map), dist.map)
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
    
    ggsave(filename = file.path(win.res, "fig", paste0(save.name,".", device.out)),
           dif.hist, 
           width = 8, height = 5, unit = "in",
           dpi = 300,
           device = dev.ext)}
  
  
  return(dif.hist)
  
}

(staticHist <- fatSurvivalHistograms(survNULL = survStatic.null,
                                     survINF =  survStatic.inf,
                                     mylu.dist,
                                     survColors(2),
                                     canada.focus = T,
                                     save.name = "fatSurvivalHistStaticCanada",
                                     device.out = "pdf"))

######### Sand Box ##########
## highest relative change: largest differences between infection and not
surv.dif <-  fatReq984.inf - fatReq984.null
p.dif <- (surv.dif/fatMean)*100


plot(surv.dif)
fatDiffPlot <- function(x, c.string, res.agg = 25, dist.map = NULL,
                        north.america = North.America, canada.focus = F,
                        save.name = NULL,  device.out = NULL,  ...){
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg, fun=mean)
  }
  else{
    x.ag <- x}
  ## Crop and mask to distribution
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
  }
  if(canada.focus == T){
    can.ext <- extent(-140,-104,41,60)
    x.ag <-crop(x.ag, can.ext)
  }
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "winter")
  
  g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
    coord_fixed(xlim = extent(x.ag)[1:2], ylim = extent(x.ag)[3:4]) +
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\ndifference\nin body fat\nrequired (g)",
                         colors = c.string,
                         limits=  c(0,
                                    maxValue(x.ag))) +
    #border lines
    geom_polygon(data= fortify(North.America),
                 aes(long,lat,group=group),
                 color="grey20",
                 fill=NA,
                 inherit.aes = F) +
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
    if(canada.focus==T){
      aspect.ratio <- (can.ext[[2]] - can.ext[[1]])/(can.ext[[4]] - can.ext[[3]])
    } else{
      aspect.ratio <-(ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])  
    }
    ggsave(filename = file.path(win.res, "fig", paste0(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...
    )}
  
  return(g.win)
}
fdiffcolors <- colorRampPalette(c( "#ffffff", "#fdb863", "#e66101" ))

fdiff <- fatDiffPlot(surv.dif, fdiffcolors(3),
                     dist.map = mylu.dist)
thing


## Survival differential 
## infected survival capacity /fat to start
surv.0 <- mylu.survI
values(surv.0)[which(values(surv.0)>=0)] <- 0
b <- values(surv.0)[which(values(surv.0) != 0)]
summary(b)
surv.pct <- (abs(surv.0)/fat.mylu) *100
plot(surv.pct)
a <- values(surv.pct)[which(values(surv.pct) != 0)]
hist(a)
summary(a)

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
  stk.ag <- aggregate(stk,25)
  stk.mask <- mask(crop(stk.ag, dist.map), dist.map)
  stk.df <- na.omit(as.data.frame(values(stk.mask))) %>%
    gather("status", "fat", 1:2 ) 
  stk.med <- stk.df %>%
    group_by(status) %>%
    summarise(med = median(fat))
  
  dif.hist <- ggplot(stk.df, aes(x=fat, fill = status, color = status))+
    scale_color_manual(values = c.string) +
    scale_fill_manual(values = c.string) +
    geom_histogram(binwidth = .25, alpha = .5, position = "identity") +
    xlim(-10,2)+
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

(surfaceHist <- fatSurvivalHistSurface(surfaceSurv.null,surfaceSurv.inf,mylu.dist, survColors(2)))

#### Onset and End of winter for Meredith) ####

onset <- calc(durationMean, fun = function(x){ifelse(x > 0, 365-(x/2-10), NA)})
writeRaster(onset, file.path(win.res, "winterOnset.tif"), format = "GTiff")
end <- calc(durationMean,fun = function(x){ifelse(x > 0, (x/2)+10, NA)})
writeRaster(end, file.path(win.res, "winterEnd.tif"), format = "GTiff")


writeRaster(surfaceSurv.null, file.path(win.res,"myluSurace_nul.tif"), format = "GTiff")
writeRaster(surfaceSurv.inf, file.path(win.res, "myluSurace_inf.tif"), format = "GTiff")
writeRaster(survStatic.null, file.path(win.res, "myluStatic_null.tif"), format = "GTiff")
writeRaster(survStatic.inf, file.path(win.res, "myluStatic_inf.tif"), format = "GTiff")

#### Aranging and such ####
library(gridExtra)

## figure 2: Top Models 
  ## alternaticly may just be the winMean.plot
topMods <- grid.arrange(winMean.plot + theme(axis.text.x = element_blank(),
                                             axis.title.x = element_blank(),
                                             axis.ticks.x = element_blank(),
                                             axis.title.y = element_blank()), 
                        massMean.plot+ theme(axis.text.x = element_blank(),
                                             axis.ticks.x = element_blank(),
                                             axis.title.x = element_blank(),
                                             axis.title.y = element_blank()),
                        fatMean.plot + theme(axis.title.y = element_blank(),
                                             axis.title.x = element_blank()),
                        ncol = 1)
## Issue: don't know how to get things to be the same sizes (issues come from 
  ## dropping the x axis attributes in the first two instances)

#figure 3 (alt): Top mass and fat 
massMean.Dist <- mass.plot(x = massmean,
                           c.string = massColors(5),
                           dist.map = mylu.dist)
fatMean.Dist <- fat.plot(fatMean, 
                         c.string = fatColors(5),
                         dist.map = mylu.dist)
massFat <- grid.arrange(massMean.Dist, 
                        fatMean.Dist,
                        ncol = 1)

