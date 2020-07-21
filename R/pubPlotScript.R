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
  ## figure 5: Increased energy expenditure due to WNS infection

  ## Si figures:

#### Set Up ####
env.prior <- ls()

## libraries
library(tidyverse); library(sf); library(raster); library(data.table); library(metR)
rasterOptions(memfrac = .3); rasterOptions(maxmemory = 1e+08) ## you'll need this

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
mylu.dist <- st_read(file.path(win.dat, "shapeFiles"), 
                     layer = "myotis_lucifugus")

st_crs(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mylu.utm <- st_transform(mylu.dist, 2955)

## small clean
rm(North.America, mylu.dist);gc()

## create utm projections and crop to the mylu distribution for all the prods
prod.utm <- file.path(win.res, "prodUTM")

## duration is seperate due to it having a different extent
# dur.rast <- raster(file.path(win.res, "duration_p.tif"))
# ## do a mylu cropped version too
# dur.c <- raster(file.path(win.res, "myluCropped__win.tif"))
# 
# ## remove values below 0 since that doesn't make sence
# dur.rast <- calc(dur.rast, function(x) ifelse(!is.na(x) & x < 0, 0, x))
# dur.c <- calc(dur.c, function(x) ifelse(!is.na(x) & x < 0, 0, x))
# 
# 
# projectRaster(dur.rast, crs = crs(NA.utm),
#               filename = file.path(prod.utm, "duration_utm.tif"),
#               format = "GTiff",
#               overwrite = T)
# 
# projectRaster(dur.c, crs = crs(NA.utm),
#               filename = file.path(prod.utm, "durationC_utm.tif"),
#               format = "GTiff",
#               overwrite = T)
# rm(dur.rast, dur.c);gc()
# 
# m.cropped <- list.files(win.res,
#                         pattern = "myluCropped_*",
#                         full.names = T)
# 
# ## do all the rest in a for loop to handel the memory requirements
# for(i in 1:length(m.cropped)){
#   ## read in
#   a <- raster(m.cropped[[i]])
#   names(a) <- paste0(strsplit(names(a), "_")[[1]][-c(1,2)], collapse = "_")
#   a.repro <- projectRaster(a, crs = crs(mylu.utm),
#                            filename = file.path(prod.utm, paste0(names(a),"_utm.tif")),
#                            format = "GTiff",
#                            overwrite = T)
#   cat(names(a), "written at: ", timestamp(),"\n")
#   rm(a, a.repro);gc()
# }



#### Functions ####
masterPlotter <- function(x,
                          break.size,
                          c.string,
                          vis.break = NULL,
                          res.agg = 20,
                          dist.map = mylu.utm,
                          use.dist = F,
                          north.america = NA.utm,
                          legend.key, text.min =25,
                          save.name = NULL,  device.out = NULL,  ...){
  ##Function for plotting single feature wintor spatial figures   
  # x <- item plotting, 
  # c.string <- for colors
  # vis.break <- optional arg 2x1 vector for manual limitation of  legend colors
  # break.by <- for handling the number of break to appear in the data
  # res.agg <- unit of aggragation (generally for the dev period with large maps)
  # dist.map <- species distribution map of you want it to be included in the plotting
  # use.dist <- T/F arg for weather to use the map or not
  # north.america <- geo-political boundries to plot on top of
  # legend.key <- string to place on the legend
  # text.min <- minimum size for text appearance
  # save.name <- string for saving the figures
  # device.out <- choose which driver is used to write out figures
  # ... <- additional arguments to be passed to ggsave
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  
  ## dist map cropping, mainly for astetics
  north.america <- st_crop(north.america, dist.map)
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("Easting", "Northing", "winter")
  
  ## handing break for the visuals
  if(is.null(vis.break)){
    break.string <- seq(floor(min(x.df$winter, na.rm=T)),
                        ceiling(max(x.df$winter, na.rm=T)),
                        by = break.size)
  }
  else{
    break.string <- seq(vis.break[[1]],
                        vis.break[[2]],
                        by = break.size)
  }
  
  
  g.win <- ggplot() +
    ##Contouring
    geom_contour_fill(data = x.df,
                      aes(x= Easting, y = Northing, z = winter),
                      breaks = break.string,
                      na.fill = -9999)+
    #handling the NA 
    stat_subset(data = x.df, 
                aes(x= Easting, y = Northing, subset = is.na(winter)),
                geom = "raster",
                fill = "#ffffff") +
    
    #oooohhhhh pretty colors
    scale_fill_gradientn(legend.key,
                         colors = c.string,
                         limits=  c(min(break.string),
                                    max(break.string))) +
    
    ##North American political boundries
    geom_sf(data = north.america,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA) +
    
        #Lables
    geom_text_contour(data = x.df, 
                      aes(x= Easting, y = Northing, z = winter),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    
    theme_bw()+
    theme(#legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
    scale_x_continuous(limits =  c(extent(dist.map)[1],extent(dist.map)[2]))+
    scale_y_continuous(limits = c(extent(dist.map)[3],extent(dist.map)[4]))
    
  
  if(use.dist==T){
    g.win <- g.win +
      geom_sf(data = dist.map,
              aes(group = "SP_ID"),
              colour = "dodgerblue4",
              size = .7,
              fill = NA)
      
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
    
    ggsave(filename = file.path(win.res,"fig", paste0(save.name,".", device.out)),
           g.win, 
           dpi = 400,
           device = dev.ext,
           ...)}
  gc()
  
  return(g.win)
}

pairedPlotting <- function(x,
                           parent.data = plotStk,
                           vis.break = NULL,
                           res.agg = 20,
                           text.min = 25,
                           break.size,
                           north.america = NA.utm,
                           dist.map = mylu.utm,
                           c.string, 
                           legend.key = "Fill this in",
                           save.name = NULL, device.out = NULL,
                           ...){
  ## Subset out the paired layers
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  
  dist.crop <- dist.map
  north.america <- st_crop(north.america, dist.crop)
  
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df) <- c("Easting", "Northing", "Infected", "Uninfected")
  
  x.df <- x.df %>%
    gather(key = "Status", 
           value = "Value",
           c("Infected", "Uninfected"))
  ##reorder factor levels
  x.df$Status <- factor(x.df$Status, levels = c("Uninfected", "Infected"))
  ##break points for the legend
  ## handing break for the visuals
  
  break.string <- seq(vis.break[[1]],
                      vis.break[[2]],
                      by = break.size)
  
  
  g.win <- ggplot()+
    geom_contour_fill(data = x.df,
                      aes(x= Easting, y = Northing, z = Value),
                      breaks = break.string,
                      na.fill = -9999)+
    #handling the NA 
    stat_subset(data = x.df, 
                aes(x= Easting, y = Northing, subset = is.na(Value)),
                geom = "raster",
                fill = "#ffffff")+
    
    #oooohhhhh pretty colors
    scale_fill_gradientn(legend.key,
                         colors = c.string,
                         limits=  c(min(break.string),
                                    max(break.string))) +
    
    ##North American political boundries
    geom_sf(data = north.america,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA)+
    geom_sf(data = dist.crop,
            aes(group = "SP_ID"),
            colour = "dodgerblue4",
            fill = NA)   +
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= Easting, y = Northing, z = Value),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    theme_bw()+
    theme(#legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm"))+
    scale_x_continuous(limits =  c(extent(dist.crop)[1],extent(dist.crop)[2]))+
    scale_y_continuous(limits = c(extent(dist.crop)[3],extent(dist.crop)[4]))+
    facet_wrap( .~ Status,
                nrow = 1)
  
  ## Save Flag  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    
    ggsave(filename = file.path(win.res,"fig", paste0(save.name,".", device.out)),
           g.win, 
           dpi = 400,
           device = dev.ext,
           ...)}
  gc()
  return(g.win)
}

increasedExpendaturePlot <- function(x,
                                      parent.data = plotStk, 
                                      res.agg = 20,
                                      north.america =NA.utm,
                                      canada.focus = NULL,
                                      dist.map = mylu.utm,
                                      text.min = 35,
                                      legend.key = "Fill this in",
                                      save.name = NULL,
                                      device.out = NULL,
                                      ...){
  
  ## Subset out the paired layers
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
    north.america <- st_crop(north.america, dist.crop)
    
  }
  else{
    ## Crop background to distribution
    dist.crop <- dist.map
    north.america <- st_crop(north.america, dist.crop)
  }
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df) <- c("Easting", "Northing", "Infected", "Uninfected")
  
  x.df <- x.df %>%
    mutate(precIncrease = (Infected/Uninfected)*100)
  
  ## create breaks
  break.string <- seq(floor(min(x.df$precIncrease, na.rm = T)),
                      ceiling(max(x.df$precIncrease, na.rm = T)),
                      by = 25)
  colourCount = length(break.string)
  getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Spectral"))
  
  g.win <- ggplot() +
    ##Contouring
    geom_contour_fill(data = x.df,
                      aes(x= Easting, y = Northing, z = precIncrease),
                      breaks = break.string,
                      na.fill = -9999)+
    #handling the NA 
    stat_subset(data = x.df, 
                aes(x= Easting, y = Northing, subset = is.na(precIncrease)),
                geom = "raster",
                fill = "#ffffff")+
    
    scale_fill_gradientn(legend.key,
                         colors = rev(getPalette(colourCount)),
                         limits=  c(min(break.string),
                                    max(break.string))) +
    ##North American political boundries
    geom_sf(data = north.america,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA)+
    
    geom_sf(data = dist.crop,
            aes(group = "SP_ID"),
            colour = "dodgerblue4",
            size = .7,
            fill = NA)   +
    
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= Easting, y = Northing, z = precIncrease),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    
    theme_bw()+
    scale_x_continuous(limits =  c(extent(dist.crop)[1],extent(dist.crop)[2]))+
    scale_y_continuous(limits = c(extent(dist.crop)[3],extent(dist.crop)[4]))+
    theme(#legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank())
  
  ## Save Flag  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    
    ggsave(filename = file.path(win.res,"fig",
                                paste0(save.name,".", device.out)),
           g.win, 
           dpi = 400,
           device = dev.ext,
           ...)}
  gc()
  
  return(g.win)
}



#### Figure 1 ####
## predicted duration of winter

## prediction raster
dur.rast <- raster(file.path(prod.utm, "durationC_utm.tif"))

## color set
winterColors <- colorRampPalette(c("#e0ecf4", "#9ebcda","#8856a7"))

## plot and write
dur.plot <- masterPlotter(x = dur.rast, break.size = 30, c.string = winterColors(5),
                          use.dist = F,legend.key = "Predicted\nDuration\nWnter\n(Days)",
                          text.min = 50, save.name = "winDuration_Mean_MYLU",
                          device.out = "png", width = 6, unit = "in")
rm(dur.rast,winterColors,dur.plot);gc()

#### Figure 2 ####
## predicted mass and fat for M. lucifugus
## includes the state analysis and body mass/ body mass relationship

mass.p <- raster(file.path(prod.utm, "mass_utm.tif"))
fat.p <- raster(file.path(prod.utm, "fat_utm.tif"))

## colors
massColors <- colorRampPalette(c("#f7fcb9", "#31a354"))
fatColors <- colorRampPalette(c("#fff7bc","#fec44f", "#d95f0e"))

## mass
mass.plot <- masterPlotter(x = mass.p, break.size = .5, c.string = massColors(5),
                           vis.break = c(5,14), use.dist = T, legend.key = "Predicted\nBody\nMass (g)",
                           text.min = 50)
# , save.name = "massMean_MYLU", device.out = "png",
#                            width = 6, unit = "in")

## fat
fat.plot <- masterPlotter(x = fat.p, break.size = .5, c.string = fatColors(5),
                           vis.break = c(0,5.5), use.dist = T, legend.key = "Predicted\nBody\nFat (g)",
                           text.min = 50)
# , save.name = "fatMean_MYLU", device.out = "png", 
#                           width = 6, unit = "in")

## qMR analysis components
##data
dat.clean <- fread("data/qmrCleaned.csv")
## clean in the same fashion as in that script
dat.clean <- dat.clean %>%
  filter(fat > 0.85) %>%
  mutate(fl = fat + lean,
         flp = fl/mass)
dat.clean$sex <- as.factor(dat.clean$sex)
dat.clean$state <- as.factor(dat.clean$state)
## LM
fat.pred <- lm(fat ~ mass, dat.clean)

## State Lean mass plot
state.lean.plot <- ggplot(data = dat.clean) +
  geom_boxplot(aes(x = state, y = lean, color = state))+
  ylab("Lean Mass (g)") + xlab("State") +
  theme_bw()

## Linear relationship
fat.mass.plot <- ggplot(dat.clean) + 
  geom_point(aes(x = mass, y = fat, color = state), show.legend = F) + 
  geom_abline(aes(intercept = fat.pred$coefficients[[1]],
                  slope = fat.pred$coefficients[[2]])) +
  annotate("text",
           x=7.9, y = 3.5,
           label = paste0("Fat Mass = ",round(fat.pred$coefficients[[1]],2)," + ",
                          round(fat.pred$coefficients[[2]],2)," * Mass")) +
  xlab("Body Mass (g)") +
  ylab("Fat mass (g)") + 
  theme_bw()
## combine
library(gridExtra)
fig3 <- grid.arrange(state.lean.plot, mass.plot,
                     fat.mass.plot,fat.plot,
                     nrow = 2)
ggsave(file.path(win.res, "fig", "Mass_Fat_Superfig.png"),
       fig3,
       device = "png",
       width = 8,
       height = 6,
       units = "in")
rm(mass.p, fat.p, massColors, fatColors, mass.plot, fat.plot, fig3,
   dat.clean, fat.pred, state.lean.plot, fat.mass.plot);gc()

#### Figure 3 ####
## predicted hibernation survival
## and increased energy for the fixed simulation
sDay.fixed <- stack(list.files(prod.utm, "sDay_fixed", full.names = T))

## usefull to check the visable range of results for best plotting
# rasterVis::levelplot(sDay.fixed) 


below0 <- colorRampPalette(c( "#E08214", "#fdb863"))
above0 <- colorRampPalette(c("#E8E3F0", "#5E3C99"))
## figuring out color scheemes
## range; -65:371
## to get teh color to work correctly you need to get the vis break and color string to line up
## and use the break size to scale

fix.color <- c(below0(1), above0(5.75)) ## maybe divide each one by the break size?

surv <- pairedPlotting(x = "sDay_fixed", parent.data = sDay.fixed, vis.break = c(-56,322),
                       break.size = 14,text.min = 50,
                       c.string = fix.color,legend.key = "survival\n Capacity\n (days)" )
                       # ,save.name = "survDays_fixed", device.out = "png", width = 8, height = 4,
                       # unit = "in")

fat.stk <- stack(list.files(prod.utm, "fat", full.names = T)[3:4])

a <- increasedExpendaturePlot(x <- "fat", parent.data = fat.stk, res.agg = 20,
                              text.min = 50, north.america = NA.utm, canada.focus = NULL,
                              legend.key = "Precent\nIncreased\nFat\nRequired")
                              # save.name = "precIncrease_fixed", device.out = "png",
                              # width = 6, units = "in")
fixed.Survival <- grid.arrange(surv, a, layout_matrix = matrix(c(1,2,2,1,2,2), nrow = 3, ncol = 2))


ggsave(file.path(win.res, "fig", "fixedSurvival_super.png"),
       fixed.Survival,
       device = "png",
       width = 8,
       height = 6,
       units = "in")

#### Figure 5 ####
## best temperature condtions
sDay.best <- stack(list.files(prod.utm, "sDay_best", full.names = T))
rasterVis::levelplot(sDay.best)

surv.b <- pairedPlotting(x = "sDay_best", parent.data = sDay.best, vis.break = c(-56,322),
                       break.size = 14,text.min = 50,
                       c.string = fix.color,legend.key = "survival\n Capacity\n (days)")
                       # ,save.name = "survDays_best", device.out = "png", width = 8, height = 4,
                       # unit = "in")


fat.best <- stack(list.files(prod.utm, "fat", full.names = T)[1:2])

b <- increasedExpendaturePlot(x <- "fat_BEST",parent.data = fat.best, res.agg = 20,
                              text.min = 50, north.america = NA.utm,
                              canada.focus = NULL, legend.key = "Precent\nIncreased\nFat\nRequired")
                              # save.name = "precIncrease_best",
                              # device.out = "png",
                              # width = 6, 
                              # units = "in")
best.Survival <- grid.arrange(surv.b, b, layout_matrix = matrix(c(1,2,2,1,2,2), nrow = 3, ncol = 2))


ggsave(file.path(win.res, "fig", "bestSurvival_super.png"),
       fixed.Survival,
       device = "png",
       width = 8,
       height = 6,
       units = "in")

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



#### SI Figures ####
#### location x Data type 
dur.raw <- fread("data/durationDataReferenced.csv")
mass.raw <- fread("data/massDataReferenced.csv")

dur.raw$type <- "Duration"
mass.raw$type <- "Mass"

full.dat <- full_join(dur.raw, mass.raw) %>%
  dplyr::select(Lat, Long, type)
colnames(full.dat)[[3]] <- "Data Type"

full.sf <- st_as_sf(full.dat,
                    coords = c("Long","Lat"))
st_crs(full.sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
full.utm <- st_transform(full.sf, 2955)

##further crop to the species extent 
NA.mylu <- st_crop(NA.utm, mylu.utm) ## this apparently wont work

## plot
dat.map <- ggplot() +
  ##North American political boundaries
  geom_sf(data = NA.mylu,
          aes(group = "Name_1"),
          color="grey20",
          fill=NA)+
  geom_sf(data = mylu.utm,
          aes(group = "SP_ID"),
          color="grey10",
          fill="grey80", 
          size = .7,
          alpha = .2)+
  geom_sf(data = full.utm,
          aes(shape = `Data Type`),
          alpha = .7, #position = "dodge",
          size = 2,
          show.legend = "point")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()



ggsave(filename = file.path(win.res, "fig", "dataLocations.png"),
       dat.map, device = "png",
       height = 4.875,
       width = 7.85,
       units = "in",
       dpi = 300)