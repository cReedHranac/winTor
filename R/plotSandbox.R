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


masterPlotter2 <- function(x, c.string, breaks.by, res.agg = 20, dist.map = NULL,
                          north.america = North.America, canada.focus = F,
                          legend.key,
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
  breaks <- seq(from = floor(minValue(x.ag)),
                to = ceiling(maxValue(x.ag)),
                by = breaks.by)
  
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


quack <- masterPlotter2(x = plotStk$mass,
                        c.string = massColors(5),
                        breaks.by = .5,
                        dist.map = mylu.dist,
                        legend.key = "Fill this in")
