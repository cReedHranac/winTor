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
x <- "fatReq_4_98"
c.string <- reqColors(4)





pairedPlotting <- function(x, parent.data = plotStk, 
                           res.agg = 25, north.america = North.America,
                           canada.focus = F, dist.map = mylu.dist,
                           c.string, legend.key = "Fill this in",
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
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df) <- c("long", "lat", "inf", "nul")
  
  x.df <- x.df %>%
    gather(key = "Status", 
           value = "Value",
           c("inf", "nul"))
  
  (g.win <- ggplot(data = x.df, aes(x=long, y = lat, z = Value))+
      coord_fixed(xlim = extent(x.ag)[1:2], ylim = extent(x.ag)[3:4]) +
      geom_contour_fill(na.fill = 0) +
      geom_contour_tanaka() +
      geom_text_contour(stroke = 0.2)+
      scale_fill_gradientn(legend.key,
                           colors = c.string,
                           limits=  c(floor(min(x.df$Value)),
                                      ceiling(max(x.df$Value)))) +
      scale_x_longitude() +
      scale_y_latitude() +
      
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
      facet_wrap( ~ Status, ncol = 1)+
      theme(legend.position = c(0.1,0.40),
            legend.margin = margin(),
            legend.key.width = unit(0.5, "cm"),
            legend.key.height = unit(0.4, "cm"),
            legend.text=element_text(size=7),
            legend.title=element_text(size=9),
            axis.title = element_blank()) 
  )
  
}



