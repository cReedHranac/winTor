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


#### Functions ####
masterPlotter <- function(x, c.string, res.agg = 25, dist.map = NULL,
                          north.america = North.America, canada.focus = F,
                          legend.key, surv.countours = F,
                          save.name = NULL,  device.out = NULL,  ...){
  ##Function for plotting all  wintor spatial figures   
  # x <- item plotting, 
  # res.agg <- unit of aggragation (generally for the dev period with large maps)
  # dist.map <- species distribution map of you want it to be included in the plotting
  # north.america <- geo-political boundries to plot on top of
  # canada.focus <- Logical of wheather or not to focus on Western Canada
  # legend.key <- string to place on the legend
  # surv.countour <- Logical,countours for the survival mapping functionality
  # save.name <- string for saving the figures
  # device.out <- choose which driver is used to write out figures
  # ... <- additional arguments to be passed to ggsave
  
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

#### Creating North America Geo-political boundries for backround. ####

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
North.America <- readOGR(dsn = win.dat,
                         layer = "NorthAmerica")
mylu.dist <- readOGR(dsn = "D:/Dropbox/batwintor_aux/paramFiles/ShapeFiles", 
                     layer = "myotis_lucifugus")
proj4string(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#### Load data ####
library(raster);library(tidyverse)
raw.layers <- stack(list.files(win.res, pattern = "_p", full.names = T))
## Create the fat layer
raw.layers$fat_p <- calc(raw.layers$massRaster_p, fun = function(x){-2.84 + 0.593*x})
## Changing names for simplicity
names(raw.layers) <- c("win", "mass", "fat")

## Survival layers
fatReq.layers <- stack(list.files(win.res, pattern = "MYLU_fat", full.names = T))
names(fatReq.layers) <- c("fatReq_2_100_inf", "fatReq_2_100_null",
                        "fatReq_4_98_inf", "fatReq_4_98_null")
##Creating survival capacities for each of the simulations
  ## Issues with running out of ram on my 32 Gb machine
surv.layers <- list()
for(i in 1:nlayers(fatReq.layers)){

  out <- raw.layers$fat - fatReq.layers[[i]]
  names(out) <- paste(c("surv",sapply(strsplit(names(fatReq.layers[[i]]), "_"),tail,3)),collapse = "_")
  surv.layers[[i]] <- out
}
surv.stk <- do.call(stack, surv.layers)

full.stk <- stack(raw.layers, fatReq.layers, surv.stk)
rm(raw.layers, fatReq.layers,surv.layers, surv.stk)

#### Create cropped estimates for mapping ####
crop.stk <- mask(crop(full.stk, mylu.dist), mylu.dist)

writeRaster(crop.stk,
            file.path(win.res, "myluCropped_.tif"),
            format = "GTiff",
            bylayer = T,
            suffix = "names",
            overwrite=T)

plotStk <- stack(list.files(win.res, pattern = "myluCropped_*", full.names = T))
names(plotStk) <- sub(".*?__", "", names(plotStk))

#### Winter duration plots ####

library(gridExtra)
## Plot Function 

#Winter duration raster
winterColors <- colorRampPalette(c("#e0ecf4", "#9ebcda","#8856a7"))

(winMean.plot <- masterPlotter(x = plotStk$win,
                               c.string = winterColors(5),
                               canada.focus = F,
                               legend.key = "Predicted\nDuration\nWnter\n(Days)",
                               save.name = "winDuration_Mean_MYLU",
                               device.out = "pdf"))
(winMean.plot.canada <- masterPlotter(x = plotStk$win,
                                      c.string = winterColors(5),
                                      canada.focus = T,
                                      legend.key = "Predicted\nDuration\nWnter\n(Days)",
                                      save.name = "winDuration_Mean_MYLU_Canada",
                                      device.out = "pdf"))


 #### Body Mass and Fat Mass Plots####
massColors <- colorRampPalette(c("#f7fcb9", "#31a354"))

(massMean.plot <- masterPlotter(x = plotStk$mass,
                            c.string = massColors(5),
                            canada.focus = F,
                            dist.map = mylu.dist,
                            legend.key = "Predicted\nBody\nMass (g)",
                            save.name = "massMean_MYLU",
                            device.out = "pdf"))
(massMean.plot.canada <- masterPlotter(x = plotStk$mass,
                                c.string = massColors(5),
                                canada.focus = T,
                                dist.map = mylu.dist,
                                legend.key = "Predicted\nBody\nMass (g)",
                                save.name = "massMean_MYLU_Canada",
                                device.out = "pdf"))

fatColors <- colorRampPalette(c("#fff7bc","#fec44f", "#d95f0e"))

(fatMean.plot <- masterPlotter(x = plotStk$fat,
                          c.string = fatColors(5),
                          canada.focus = F,
                          legend.key = "Predicted\nBody\nFat (g)",
                          dist.map = mylu.dist,
                          save.name = "fatMean_MYLU",
                          device.out = "pdf"))

(fatMean.plot.canada <- masterPlotter(x = plotStk$fat,
                               c.string = fatColors(5),
                               canada.focus = T,
                               legend.key = "Predicted\nBody\nFat (g)",
                               dist.map = mylu.dist,
                               save.name = "fatMean_MYLU_Canada",
                               device.out = "pdf"))
## Single figure
library(gridExtra)
fig3 <- grid.arrange(massMean.plot, 
                     fatMean.plot, 
                     ncol = 1)
ggsave(file.path(win.res, "fig", "Mass_Fat_MYLU.pdf"),
       fig3,
       device = cairo_pdf,
       width = 9,
       height = 6.5, 
       units = "in")

fig3Canada <- grid.arrange(massMean.plot, 
                           fatMean.plot, 
                           ncol = 1)
ggsave(file.path(win.res, "fig", "Mass_Fat_MYLU_Canda.pdf"),
       fig3Canada,
       device = cairo_pdf,
       width = 9,
       height = 6.5, 
       units = "in")


#### Survival mapping ####
survColors <- colorRampPalette(c(  "#e66101","#fdb863","#ffffff", "#b2abd2","#5e3c99"))
## Doesn't go below 0
survColors.Pos <- colorRampPalette(c("#ffffff", "#B2ABD2", "#8873B5", "#5E3C99"))
reqColors.Pos <- colorRampPalette(c("#ffffff", "#5E3C99"))

masterPlotter.Surv <- function(x,  res.agg = 25, dist.map = NULL,
                          north.america = North.America, canada.focus = F,
                          legend.key, surv.countours = F, col.string,
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
                         colors = col.string,
                         # mid = "#ffffff",
                         # midpoint = 0, 
                         limits=  c(floor(minValue(x.ag)),
                                    ceiling(maxValue(x.ag)))) +
    #border lines
    geom_polygon(data= fortify(North.America),
                 aes(long,lat,group=group),
                 color="grey90",
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

(fatreq.plot <- masterPlotter.Surv(x = plotStk$fatReq_4_98_null,
                              canada.focus = F,
                              dist.map = mylu.dist,
                              c.string = reqColors.Pos(5),
                              surv.countours = F,
                              legend.key = "Predicted\nBody Fat\nRequired (g)",
                              save.name = "fatRequired_4_98_Dist",
                              device.out = "pdf"))


(fatreq.plot <- masterPlotter.Surv(x = plotStk$fatReq_2_100_null,
                                   canada.focus = F,
                                   dist.map = mylu.dist,
                                   c.string = reqColors.Pos(5),
                                   surv.countours = F,
                                   legend.key = "Predicted\nBody Fat\nRequired (g)",
                                   save.name = "fatRequired_2_100_Dist",
                                   device.out = "pdf"))

## Fat req

(staticNull.plot <- masterPlotter.Surv(x = plotStk$surv_4_98_null,
                              canada.focus = F,
                              dist.map = mylu.dist,
                              surv.countours = T,
                              col.string = survColors.Pos(4),
                              legend.key = "Predicted\nBody Fat\nRemaining (g)"))#,
                              # save.name = "nullsurvive_4_98_Dist",
                              # device.out = "pdf"))
(staticInf.plot <- masterPlotter.Surv(x = plotStk$survInf,
                             canada.focus = F,
                             dist.map = mylu.dist,
                             legend.key = "Predicted\nBody Fat\nRemaining (g)",
                              surv.countours = T,
                             col.string = survColors(5)))#,
                             # save.name = "infSurvive4_98_Dist",
                             # device.out = "pdf"))

surFig <- grid.arrange(staticNull.plot, 
                       staticInf.plot, 
                       ncol = 1)
ggsave(file.path(win.res, "fig", "SurvFig.pdf"),
       surFig,
       device = cairo_pdf,
       width = 9,
       height = 6.5, 
       units = "in")


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

(staticHist <- fatSurvivalHistograms(survNULL = full.stk$surv_4_98_null,
                                     survINF =  full.stk$surv_4_98_inf,
                                     mylu.dist,
                                     survColors(2),
                                     canada.focus = F))
                                     # save.name = "fatSurvivalHistStatic",
                                     # device.out = "pdf"))

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
survDiff <- function(x, y = plotStk$fat){
  #x is survival capacity
  #y is predicted fat content
  if(x<0){
    val <- abs(x)/y
  } else{ val <- 0}
    
  return(val)
    
}

precNull <- calc(plotStk$survNull, survDiff) 
precInf <- 
  
# lil hack hack to get it working  
precNull <- plotStk$survNull  
values(precNull)[which(values(precNull)>=0)] <- 0
b <- values(precNull)[which(values(precNull) != 0)]
summary(b)
surv.pctNull <- (abs(precNull)/plotStk$fat) *100
plot(surv.pctNull)
a <- values(surv.pct)[which(values(surv.pct) != 0)]
hist(a)
summary(a)
writeRaster(surv.pct, filename = file.path(win.res, "survivalPrecentNull.tif"), 
            format = "GTiff", overwrite = T)

precNull <- plotStk$survInf  
values(precNull)[which(values(precNull)>=0)] <- 0
b <- values(precNull)[which(values(precNull) != 0)]
summary(b)
surv.pctInf <- (abs(precNull)/plotStk$fat) *100
plot(surv.pctInf)
a <- values(surv.pct)[which(values(surv.pct) != 0)]
hist(a)
summary(a)
writeRaster(surv.pct, filename = file.path(win.res, "survivalPrecentInf.tif"), 
            format = "GTiff")

## make plots for these?
 colpal <- colorRampPalette(C( "#f1948a" , "#ec7063", "#cb4335"))

 
 (precSurv.plot <- masterPlotter(x = surv.pctNull,
                                c.string = colpal,
                                canada.focus = F,
                                legend.key = "Precent\nfat\nShortfall (g)",
                                dist.map = mylu.dist,
                                save.name = "precSurvNull_Dist",
                                device.out = "pdf"))

 (precSurvInf.plot <- masterPlotter(x = surv.pctInf,
                                 c.string = fdiffcolors(3),
                                 canada.focus = F,
                                 legend.key = "Precent\nfat\nShortfall (g)",
                                 dist.map = mylu.dist,
                                 save.name = "fatShortfallPrecentInf",
                                 device.out = "pdf"))
                                  

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






















