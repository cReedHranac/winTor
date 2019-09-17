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
x <- "fatReq"
reqColors <- colorRampPalette(c("#ffffff", "#5E3C99"))
parent.data <- plotStk
c.string <- reqColors(4)
res.agg = 20
north.america = North.America
canada.focus = F
dist.map = mylu.dist
legend.limits <- c(-1,4)
legend.key = "Fill this in"




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
  breaks <- seq(legend.limits[[1]], legend.limits[[2]], by = 0.5)
  (g.win <- ggplot(data = x.df, aes(x=long, y = lat, z = Value))+
      coord_fixed(xlim = extent(x.ag)[1:2], ylim = extent(x.ag)[3:4]) +
      geom_contour_fill(breaks = breaks,
                        na.fill = -9999) +
      scale_fill_gradientn(legend.key,
                           colors = c.string,
                           limits=  legend.limits) +
      scale_x_longitude() +
      scale_y_latitude() +
      #border lines
      geom_polygon(data= fortify(north.america),
                   aes(long,lat,group=group),
                   color="grey20",
                   fill=NA,
                   inherit.aes = F) +
      ##Species distribution
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) +
      ## Labels
      geom_text_contour(stroke = 0.2,
                        min.size = 25,
                        rotate = F, 
                        check_overlap = T)+
      #general malarkey
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      theme_bw()+
      theme(legend.position = c(0.1,0.15),
            legend.margin = margin(),
            legend.key.width = unit(0.5, "cm"),
            legend.key.height = unit(0.4, "cm"),
            legend.text=element_text(size=7),
            legend.title=element_text(size=9),
            axis.title = element_blank()) +
    facet_grid( rows = vars(Hibernation_Condition),
                cols = vars(Infection_status))
  )
  
  ## Canada focus flag
  if(canada.focus==T){
    can.ext <- c(-140,-104,41,60)
    g.win <- g.win +
      coord_cartesian(xlim = can.ext[1:2],
                      ylim = can.ext[3:4]) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) 
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
           width = 3, height = 8, unit = "in",
           dpi = 300,
           device = dev.ext)}
  
  return(g.win)
}


a <- quadPlot(x = "fatReq",
              parent.data = plotStk,
              c.string = reqColors(4),
              res.agg = 20,
              legend.limits =c(-1,4),
              legend.key = "Fill this in")



fuckedfunction <- function(a,
                           b){
  a.quo <- enquo(a)
  foo <- !!a.quo=="fuck"
  return(foo)
}

fuckedfunction(a=fuck)
