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

## Tesing variables
x <- plotStk$mass
massColors <- colorRampPalette(c("#f7fcb9", "#31a354"))
c.string <- massColors(5)
res.agg = 20
north.america = North.America
canada.focus = T
dist.map = mylu.dist

legend.key = "Fill this in"


masterPlotter2 <- function(x, c.string, break.string, res.agg = 20, dist.map = NULL,
                           north.america = North.America, canada.focus = F,
                           legend.key, text.min =25,
                           save.name = NULL,  device.out = NULL,  ...){
  ##Function for plotting all  wintor spatial figures   
  # x <- item plotting, 
  # c.string <- for colors
  # breaks.by <- for handling the number of breaks to appear in the data
  # res.agg <- unit of aggragation (generally for the dev period with large maps)
  # dist.map <- species distribution map of you want it to be included in the plotting
  # north.america <- geo-political boundries to plot on top of
  # canada.focus <- Logical of wheather or not to focus on Western Canada
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
  ## Crop and mask to distribution
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
    north.america <- st_crop(north.america, extent(x.ag))
  }
  
  ## Canada focus flag
  if(canada.focus==T){
    can.ex <- c(-127.2429, 101.3304, 43.9173, 64.1641)
    x.ag <- crop(x.ag, extent(can.ex))
    north.america <- st_crop(north.america, extent(x.ag))
  }
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "winter")
  
  ## handing breaks for the visuals
  breaks <- break.string
  
  g.win <- ggplot() +
    ##Contouring
    geom_contour_fill(data = x.df,
                      aes(x= long, y = lat, z = winter),
                      breaks = breaks,
                      na.fill = -9999)+
    #handling the NA 
    stat_subset(data = x.df, 
                aes(x= long, y = lat, subset = winter == -9999),
                geom = "raster",
                fill = "#ffffff")+
    
    #oooohhhhh pretty colors
    scale_fill_gradientn(legend.key,
                         colors = c.string,
                         limits=  c(floor(minValue(x.ag)),
                                    ceiling(maxValue(x.ag)))) +
    
    ##North American political boundries
    geom_sf(data = north.america,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA)+
    
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= long, y = lat, z = winter),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    
    theme_bw()+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank())
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    dist.crop <- st_crop(dist.map, x.ag)
    
    g.win <- g.win +
      geom_sf(data = dist.crop,
              aes(group = "SP_ID"),
              colour = "black",
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
  
  return(g.win)
}


quack <- masterPlotter2(x = plotStk$win,
                          c.string = winterColors(5),
                          break.string = seq(0,360, by= 30),
                        dist.map = mylu.dist,
                          canada.focus = F,
                          legend.key = "Predicted\nDuration\nWnter\n(Days)",
                          text.min = 40)


#### Pair ####
x = "surv_2_100"
parent.data = plotStk
c.string = survColors_2x100(4)
text.min <- 25
legend.key = "Predicted\nBody Fat\nRequired (g)"


pairedPlotting2 <- function(x, parent.data = plotStk, 
                           res.agg = 20,
                           text.min = 25,
                           north.america = North.America,
                           canada.focus = F, dist.map = mylu.dist,
                           c.string, 
                           legend.key = "Fill this in",
                           save.name = NULL, device.out = NULL){
  ## Subset out the paired layers
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
    ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  ## Crop and mask to distribution
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
    north.america <- st_crop(north.america, extent(x.ag))
  }
  
  ## Canada focus flag
  if(canada.focus==T){
    can.ex <- c(-127.2429, 101.3304, 43.9173, 64.1641)
    x.ag <- crop(x.ag, extent(can.ex))
    north.america <- st_crop(north.america, extent(x.ag))
  }
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df) <- c("long", "lat", "Infected", "Uninfected")
  
  x.df <- x.df %>%
    gather(key = "Status", 
           value = "Value",
           c("Infected", "Uninfected"))
  ##reorder factor levels
  x.df$Status <- factor(x.df$Status, levels = c("Uninfected", "Infected"))
  ##break points for the legend
  break.string <- seq(floor(min(x.df$Value)), ceiling(max(x.df$Value)), by = .5)
  
  (g.win <- ggplot()+
      geom_contour_fill(data = x.df,
                        aes(x= long, y = lat, z = Value),
                        breaks = break.string,
                        na.fill = -9999)+
      #handling the NA 
      stat_subset(data = x.df, 
                  aes(x= long, y = lat, subset = Value == -9999),
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
      
      #Lables
      geom_text_contour(data = x.df, 
                        aes(x= long, y = lat, z = Value),
                        stroke = 0.2, min.size = text.min,
                        rotate = F, check_overlap = T)+
      
      theme_bw()+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      theme(legend.position = "bottom",
            legend.text=element_text(size=7),
            legend.title=element_text(size=9),
            axis.title = element_blank())+
    facet_wrap( .~ Status,
                ncol = 1))
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    dist.crop <- st_crop(dist.map, x.ag)
    
    g.win <- g.win +
      geom_sf(data = dist.crop,
              aes(group = "SP_ID"),
              colour = "black",
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
  
  return(g.win)
}

wack <- pairedPlotting2(x = "surv_2_100",
                        parent.data = plotStk,
                        c.string = survColors_2x100(4),
                        text.min = 25,
                        legend.key = "Predicted\nBody Fat\nRequired (g)")

#### Quad ####
x = "surv"
survColors_4x98 <- colorRampPalette(c("#fdb863",
                                      "#E8E3F0", "#BAABD2", "#8C73B5","#5E3C99"))
c.string = survColors_4x98(5)
legend.key = "Predicted\nBody Fat\nRemaining (g)"
parent.data = plotStk
north.america = North.America
dist.map = mylu.dist
quadPlot <- function(x,
                     parent.data = plotStk, 
                     res.agg = 20,
                     north.america = North.America,
                     canada.focus = F,
                     dist.map = mylu.dist,
                     c.string,
                     legend.limits,
                     legend.key = "Fill this in",
                     save.name = NULL,
                     device.out = NULL){
  
  ## Subset out the paired layers
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  ## Crop and mask to distribution
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
    north.america <- st_crop(north.america, extent(x.ag))
  }
  
  ## Canada focus flag
  if(canada.focus==T){
    can.ex <- c(-127.2429, 101.3304, 43.9173, 64.1641)
    x.ag <- crop(x.ag, extent(can.ex))
    north.america <- st_crop(north.america, extent(x.ag))
  }
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df)[1:2] <- c("long", "lat")
  
  x.df <- x.df %>%
    gather(key ="Layer",
           value = "Value",
           starts_with(strsplit(names(x.ag),"_")[[1]][[1]])) %>% 
    mutate(Infection_status = case_when(str_detect(Layer, "inf") ~ "Infected",
                                        str_detect(Layer, "null") ~ "Uninfected"),
           Hibernation_Condition = case_when(str_detect(Layer, "2_100") ~ "2x100",
                                             str_detect(Layer, "4_98") ~ "4x98"))
  
  ##reorder factor levels
  x.df$Infection_status <- factor(x.df$Infection_status,
                                  levels = c("Uninfected", "Infected"))
  x.df$Hibernation_Condition <- factor(x.df$Hibernation_Condition,
                                       levels = c("4x98", "2x100"))
  
  ##break points for the legend
  break.string <- seq(floor(min(x.df$Value)), ceiling(max(x.df$Value)), by = .5)
  
  (g.win <- ggplot()+
      geom_contour_fill(data = x.df,
                        aes(x= long, y = lat, z = Value),
                        breaks = break.string,
                        na.fill = -9999,
                        guide = "colorstrip")+
      #handling the NA 
      stat_subset(data = x.df, 
                  aes(x= long, y = lat, subset = Value == -9999),
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
      
      #Lables
      geom_text_contour(data = x.df, 
                        aes(x= long, y = lat, z = Value),
                        stroke = 0.2, min.size = text.min,
                        rotate = F, check_overlap = T)+
      
      theme_bw()+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      theme(legend.position = "bottom",
            legend.text=element_text(size=7),
            legend.title=element_text(size=9),
            axis.title = element_blank())+
      facet_grid( rows = vars(Hibernation_Condition),
                  cols = vars(Infection_status))
  )
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    dist.crop <- st_crop(dist.map, x.ag)
    
    g.win <- g.win +
      geom_sf(data = dist.crop,
              aes(group = "SP_ID"),
              colour = "black",
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
  
  return(g.win)
}
#### Increase ####
x <- "fatReq_4_98"
parent.data = plotStk
res.agg = 20
north.america = North.America
canada.focus = F
dist.map = mylu.dist
legend.key = "Precent\nIncreased\nFat\nRequired"


increasedExpendaturePlot2 <- function(x,
                                     parent.data = plotStk, 
                                     res.agg = 20,
                                     north.america = North.America,
                                     canada.focus = F,
                                     dist.map = mylu.dist,
                                     text.min = 35,
                                     legend.key = "Fill this in",
                                     save.name = NULL,
                                     device.out = NULL){
  
  ## Subset out the paired layers
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  ## Crop and mask to distribution
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
    north.america <- st_crop(north.america, extent(x.ag))
  }
  
  ## Canada focus flag
  if(canada.focus==T){
    can.ex <- c(-127.2429, 101.3304, 43.9173, 64.1641)
    x.ag <- crop(x.ag, extent(can.ex))
    north.america <- st_crop(north.america, extent(x.ag))
  }
  
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df) <- c("long", "lat", "Infected", "Uninfected")
  
  x.df <- x.df %>%
    mutate(precIncrease = (Infected/Uninfected)*100)
  
  ## create breaks
  break.string <- seq(floor(min(x.df$precIncrease)),
                       ceiling(max(x.df$precIncrease)),
                       by = 25)
  colourCount = length(break.string)
  getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Spectral"))
  
  g.win <- ggplot() +
    ##Contouring
    geom_contour_fill(data = x.df,
                      aes(x= long, y = lat, z = precIncrease),
                      breaks = break.string,
                      na.fill = -9999,
                      guide = "colorstrip")+
    #handling the NA 
    stat_subset(data = x.df, 
                aes(x= long, y = lat, subset = precIncrease == -9999),
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
    
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= long, y = lat, z = precIncrease),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    
    theme_bw()+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank())
  
  
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    dist.crop <- st_crop(dist.map, x.ag)
    
    g.win <- g.win +
      geom_sf(data = dist.crop,
              aes(group = "SP_ID"),
              colour = "black",
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
  
  return(g.win)
}

twak <- increasedExpendaturePlot2(x = "fatReq_4_98",
                                  parent.data = plotStk,
                                  res.agg = 20,
                                  north.america = North.America,
                                  canada.focus = F,
                                  dist.map = mylu.dist,
                                  legend.key = "Precent\nIncreased\nFat\nRequired")
