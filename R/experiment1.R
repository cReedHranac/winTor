#### Post Prediction ####

##Links
win.dat <- file.path("D:", "Dropbox", "winTor_aux", "data")
win.res <- file.path("D:", "Dropbox", "winTor_aux", "Results")

##libraries
library(tidyverse); library(raster); library(RColorBrewer)


##load raster layers to play with
dif.win <- stack(list.files(win.res, pattern = "pred_", full.names = T))


## Plot 9 and 12 month contours 

x <- dif.win[[3]] 

wintorContour <- function(x, res.agg = 25,  save = F, ...){
  ## Create DataFrame
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
   
  x.class <- calc(x.ag, function(x) {x[is.na(x)] <- NA; as.factor(round(x/30))}) #make month factors 
  m <- rbind(c(-1000, 0, 0),
             c(0,3,3), 
             c(3,6,6),
             c(6,9,9),
             c(9,12,12),
             c(12, 1000, 12))
  
  rast.trimmed <- reclassify(x.class, m) #trim ends
  x.pts <- rasterToPoints(rast.trimmed) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "Winter")
  x.df$Winter <- as.factor(x.df$Winter)
  
  g.winTor <- ggplot(x.df, aes(x = long, y = lat, z = Winter)) +
    coord_fixed(xlim = extent(x)[1:2],ylim=extent(x)[3:4])+
  #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
  #Raster fill
    geom_raster(aes(fill = Winter),  interpolate = T) +
  #oooohhhhh pretty colors
    scale_fill_brewer(palette="Spectral", na.value="white") +
  #general malarkey
    ggtitle(names(x)) + 
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
    
  if(save == T){
    ggsave(filename = file.path(win.res, names(x)),
           g.winTor, ...)
  }
  
  return(g.winTor)
}


win.maps <- lapply(unstack(dif.win),
                   wintorContour,
                   res.agg = NULL,
                   save = T,
                   device = "pdf")
