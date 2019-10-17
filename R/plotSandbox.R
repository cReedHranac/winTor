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
break.size = .5
text.min=25
massColors <- colorRampPalette(c("#f7fcb9", "#31a354"))
c.string <- massColors(5)
res.agg = 20
north.america = NA.utm
canada.focus = NULL
dist.map = mylu.utm
legend.key = "Fill this in"


canada.focus = can.utm

masterPlotter2 <- function(x, break.size, c.string, res.agg = 20,
                           dist.map = mylu.utm,
                           north.america = NA.utm,
                           canada.focus = NULL,
                           legend.key, text.min =25,
                           save.name = NULL,  device.out = NULL,  ...){
  ##Function for plotting all  wintor spatial figures   
  # x <- item plotting, 
  # c.string <- for colors
  # break.by <- for handling the number of break to appear in the data
  # res.agg <- unit of aggragation (generally for the dev period with large maps)
  # dist.map <- species distribution map of you want it to be included in the plotting
  # north.america <- geo-political boundries to plot on top of
  # canada.focus <- something you can get an extent from to crop from
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
  
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    north.america <- st_crop(north.america, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
  }
  else{
    ## Crop background to distribution
    north.america <- st_crop(north.america, extent(x.ag))
    dist.crop <- dist.map
  }
  
  
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("Easting", "Northing", "winter")
  
  ## handing break for the visuals
  break.string <- seq(floor(min(x.df$winter, na.rm=T)),
                      ceiling(max(x.df$winter, na.rm=T)),
                      by = break.size)
  
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
            fill=NA)+
    
    geom_sf(data = dist.crop,
            aes(group = "SP_ID"),
            colour = "dodgerblue4",
            fill = NA)   +
    
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= Easting, y = Northing, z = winter),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    
    theme_bw()+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
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
                        break.size = 30,
                        c.string = winterColors(5),
                        canada.focus = can.utm,
                        legend.key = "Predicted\nDuration\nWnter\n(Days)",
                        text.min = 25)
## Passes all tests for from home machine


#### Pair ####
above0 <- colorRampPalette(c("#E8E3F0", "#5E3C99"))

survColors_2x100 <- colorRampPalette(c("#fdb863",## The one below 0
                                       above0(5)))
x = "surv_2_100"
res.agg = 20
break.size = .5
parent.data = plotStk
canada.focus = NULL
north.america = NA.utm
dist.map = mylu.utm
c.string = survColors_2x100(6)
text.min <- 25
legend.key = "Predicted\nBody Fat\nRequired (g)"


pairedPlotting2 <- function(x,
                            parent.data = plotStk,
                            break.size,
                            res.agg = 20,
                            text.min = 25,
                            north.america = NA.utm,
                            canada.focus = NULL,
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
  ## Crop background to distribution
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    north.america <- st_crop(north.america, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
  }
  else{
    ## Crop background to distribution
    north.america <- st_crop(north.america, extent(x.ag))
    dist.crop <- dist.map
  }
  
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
  break.string <- seq(floor(min(x.df$Value, na.rm = T)),
                      ceiling(max(x.df$Value, na.rm = T)),
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
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      theme(legend.position = "bottom",
            legend.text=element_text(size=7),
            legend.title=element_text(size=9),
            axis.title = element_blank(),
            plot.margin=grid::unit(c(0,0,0,0), "mm"))+
      facet_wrap( .~ Status,
                  ncol = 1)
  
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
                        # canada.focus = can.utm,
                        break.size = .5,
                        parent.data = plotStk,
                        c.string = survColors_2x100(6),
                        text.min = 25,
                        legend.key = "Predicted\nBody Fat\nRequired (g)")

#### Quad ####
x = "surv"
survColors_4x98 <- colorRampPalette(c("#fdb863",
                                      "#E8E3F0", "#BAABD2", "#8C73B5","#5E3C99"))
c.string = survColors_4x98(5)
legend.key = "Predicted\nBody Fat\nRemaining (g)"
parent.data = plotStk

quadPlot <- function(x,
                     parent.data = plotStk, 
                     res.agg = 20,
                     north.america = NA.utm,
                     canada.focus = NULL,
                     dist.map = mylu.utm,
                     c.string,
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
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    north.america <- st_crop(north.america, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
  }
  else{
    ## Crop background to distribution
    north.america <- st_crop(north.america, extent(x.ag))
    dist.crop <- dist.map
  }
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df)[1:2] <- c("Easting", "Northing")
  
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
  break.string <- seq(floor(min(x.df$Value, na.rm = T)),
                      ceiling(max(x.df$Value, na.rm = T)),
                      by = .5)
  
  (g.win <- ggplot()+
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
              fill=NA)  +
      geom_sf(data = dist.crop,
              aes(group = "SP_ID"),
              colour = "dodgerblue4",
              size = .1,
              fill = NA) +
      
      #Lables
      geom_text_contour(data = x.df, 
                        aes(x= Easting, y = Northing, z = Value),
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
                                     north.america =NA.utm,
                                     canada.focus = NULL,
                                     dist.map = mylu.utm,
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
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    north.america <- st_crop(north.america, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
  }
  else{
    ## Crop background to distribution
    north.america <- st_crop(north.america, extent(x.ag))
    dist.crop <- dist.map
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
                      na.fill = -9999,
                      guide = "colorstrip")+
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
            size = .1,
            fill = NA)   +
    
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= Easting, y = Northing, z = precIncrease),
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
                                  north.america = NA.utm,
                                  # canada.focus = F,
                                  dist.map = mylu.utm,
                                  legend.key = "Precent\nIncreased\nFat\nRequired")






ggplot()+
  geom_sf(data = North.America,
          aes(group = "SP_ID"),
          color="grey20",
          fill=NA) + 
  geom_sf(data = can.sf)
