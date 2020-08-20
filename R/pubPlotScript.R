#################################################
####  Plotting for WinTor Publication        ####
####  Author: C. Reed Hranac                 ####
####  Tested                                 ####
#################################################

## This script provides all the plotting code for the wintor publication
##  Contents:
  ## Main text figures:
  ## figure 1: Predicted winter duration across North America
  ## figure 2: Predicted body mass and body fat multi pannel figure
  ## figure 4: Predicted hibernation survival and increased expendature for fixed conditions
  ## figure 5: Predicted hibernation survival and increased expendature for best avaliable conditions
  
  ## Si figures:
  ## figure 1: Data type and location
  ## Figure 2: hibernatin sensitivity to env conditions

#### Set Up ####
env.prior <- ls()

## libraries
library(tidyverse); library(sf); library(raster)
library(data.table); library(metR); library(gridExtra)
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

## Directory for all products created in the data exploration script
prod.utm <- file.path(win.res, "prodUTM")


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
    theme(legend.position = "bottom",
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
                      na.fill = -9999) +
      
    
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
                                      res.agg = 20,
                                      north.america =NA.utm,
                                      dist.map = mylu.utm,
                                      text.min = 50,
                                      legend.key = "Fill this in",
                                      save.name = NULL,
                                      device.out = NULL,
                                      ...){
  
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  ## Crop background to distribution
  dist.crop <- dist.map
  north.america <- st_crop(north.america, dist.crop)
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df) <- c("Easting", "Northing", "precIncrease")
  
  ## create breaks
  break.string <- seq(0,225,
                      by = 25)
  colourCount = length(break.string)
  getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Spectral"))
  
  g.win <- ggplot() +
    ##Contouring
    geom_contour_fill(data = x.df,
                      aes(x= Easting, y = Northing, z = precIncrease),
                      breaks = break.string,
                      na.fill = -9999)+
    
    
    scale_fill_gradientn(legend.key,
                         colors = rev(getPalette(colourCount)),
                         limits=  c(min(break.string),
                                    max(break.string))) +
    ##North American political boundaries
    geom_sf(data = north.america,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA)+
    
    geom_sf(data = dist.crop,
            aes(group = "SP_ID"),
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



#### predicted duration of winter  ####

## prediction raster
dur.rast <- raster(file.path(prod.utm, "duration_utm.tif"))
proj4string(dur.rast) <- crs(mylu.utm)

## color set
winterColors <- colorRampPalette(c("#e0ecf4", "#9ebcda","#8856a7"))

## plot and write
dur.plot <- masterPlotter(x = dur.rast, break.size = 30, c.string = winterColors(5),
                          use.dist = F,legend.key = "Predicted\nDuration\nWinter\n(Days)",text.min = 50,
                          save.name = "winDuration",
                          device.out = "png", width = 6, unit = "in")

## clean
rm(dur.rast,winterColors,dur.plot);gc()

#### predicted mass and fat for M. lucifugus ####
## 
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
  geom_boxplot(aes(x = state, y = lean, color = state),
               show.legend = F)+
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
fig3 <- grid.arrange(state.lean.plot, fat.mass.plot,
                     mass.plot, fat.plot,
                     nrow = 2,
                     heights = c(.4,.6))

fig3.2 <- cowplot::ggdraw(fig3) + 
  theme(plot.background = element_rect(fill=NA, color = NA))

# ggsave(file.path(win.res, "fig", "Mass_Fat_Superfig.png"),
#        fig3,
#        device = "png",
#        width = 7.5,
#        height = 5.5,
#        units = "in")
rm(mass.p, fat.p, massColors, fatColors, mass.plot, fat.plot, fig3,
   dat.clean, fat.pred, state.lean.plot, fat.mass.plot);gc()

#### Hibernation survival fixed conditions ####
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
                       c.string = fix.color,legend.key = "Survival\nCapacity\n(days)" )
                       # ,save.name = "survDays_fixed", device.out = "png", width = 8, height = 4,
                       # unit = "in")

inc.fix <- raster(file.path(prod.utm, "percInc_fixed_utm.tif"))

a <- increasedExpendaturePlot(x = inc.fix, res.agg = 20,
                              text.min = 40, north.america = NA.utm,
                              legend.key = "Precent\nIncrease\nin Fat\nExpended")
                              # save.name = "precIncrease_fixed", device.out = "png",
                              # width = 6, units = "in")
fixed.Survival <- grid.arrange(surv, a,
                               layout_matrix = matrix(c(1,2,2,1,2,2),
                                                      nrow = 3, ncol = 2),
                               heights = c(.37, 0.315, 0.315),
                               widths = c(1,1))


ggsave(file.path(win.res, "fig", "fixedSurvival_super.png"),
       fixed.Survival,
       device = "png",
       width = 7.5,
       height = 7.5,
       units = "in")
rm(sDay.fixed, surv, inc.best, a, fixed.Survival);gc()
#### Hibernation survival at best conditions ####
## best temperature condtions
sDay.best <- stack(list.files(prod.utm, "sDay_best", full.names = T))

surv.b <- pairedPlotting(x = "sDay_best", parent.data = sDay.best, vis.break = c(-56,322),
                       break.size = 14,text.min = 50,
                       c.string = fix.color,legend.key = "Survival\nCapacity\n(days)")
                       # ,save.name = "survDays_best", device.out = "png", width = 8, height = 4,
                       # unit = "in")


inc.best <- inc.fix <- raster(file.path(prod.utm, "percInc_best_utm.tif"))

b <- increasedExpendaturePlot(x = inc.best, res.agg = 20,
                              text.min = 45, north.america = NA.utm,
                              legend.key = "Precent\nIncrease\nin Fat\nExpended")
                              # save.name = "precIncrease_best",
                              # device.out = "png",
                              # width = 6, 
                              # units = "in")
best.Survival <- grid.arrange(surv.b, b,
                              layout_matrix = matrix(c(1,2,2,1,2,2), nrow = 3, ncol = 2),
                              heights = c(.37, 0.315, 0.315),
                              widths = c(1,1))


ggsave(file.path(win.res, "fig", "bestSurvival_super.png"),
       best.Survival,
       device = "png",
       width = 7.5,
       height = 7.5,
       units = "in")
rm(sDay.best, inc.best, surv.b, best.Survival); gc()
#### SI Figures ####
#### location x Data type ####
dur.raw <- fread(file.path(win.dat,"durationDataReferenced.csv"))
mass.raw <- fread(file.path(win.dat,"massDataReferenced.csv"))

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

#### Sensitivity figure ####
library(batwintor)
mylu.mod <- fread(file.path(win.dat, "myluDynamicModel.csv"))
time.v <- seq(1,240,by=14)

mylu.sub90 <- mylu.mod %>%
  filter(Ta %in% c(2, 4, 6, 8),
         pct.rh %in% seq(90, 100, by =2), 
         time %in% day.to.hour(time.v)) %>%
  mutate(DiffDate = hour.to.day(time),
         DiffMass_n = n.g.fat.consumed,
         DiffMass_i = g.fat.consumed,
         Temp = as.factor(Ta),
         RH = as.factor(pct.rh))

##try to change the data structure to get everything in one plot

sub90.n <- mylu.sub90 %>%
  dplyr::select(Temp, RH, DiffDate, DiffMass_n) %>%
  mutate(Infection = F)
colnames(sub90.n)[[4]] <- "DiffMass"
sub90.i <- mylu.sub90 %>%
  dplyr::select(Temp, RH, DiffDate, DiffMass_i) %>%
  mutate(Infection = T)
colnames(sub90.i)[[4]] <- "DiffMass"

sub.90 <- bind_rows(sub90.n, sub90.i)


null.plot <- ggplot() +
    ##model lines
    geom_line(data = sub.90,
              aes(x= DiffDate,
                  y = DiffMass,
                  color = RH,
                  linetype = Infection))+ 
  xlab("Time (days)")+
  ylab("Fat Consumed (g)")+
  labs(color = "Relative\nHumidity")+
    theme_bw() +
    facet_grid(~Temp)

ggsave(filename = file.path(win.res, "fig", "SI_sensitivityFig.png"),
       null.plot, device = "png",
       height = 4.5,
       width = 7,
       units = "in",
       dpi = 300)

#### Clean up script items ####
env.post <- ls()
to.remove <- env.post[env.post %!in% env.prior]
rm(list=to.remove); rm(env.post, to.remove)
