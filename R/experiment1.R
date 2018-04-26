#### Post Prediction ####

##Links
win.dat <- file.path("D:", "Dropbox", "winTor_aux", "data")
win.res <- file.path("D:", "Dropbox", "winTor_aux", "Results")

##libraries
library(tidyverse); library(raster)


##load raster layers to play with
dif.win <- stack(list.files(win.res, pattern = "pred_", full.names = T))


## Plot 9 and 12 month contours 

x <- dif.win[[3]]

wintorContour <- function(x,...){
  ## Create DataFrame
  m <- rbind(c(-Inf, 0, 0),
             c(365, Inf, 365))
  rast.trimmed <- reclassify(x, m)
  x.pts <- rasterToPoints(rast.trimmed)
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "Winter")
  
  g.winTor <- ggplot(x.df) +
    coord_fixed()+
  #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
  #Raster fill
    geom_raster( aes_(~long, ~lat, fill = ~Winter), interpolate = T) +
  #contour lines
    stat_contour(aes_(x = ~long, y = ~lat, z = ~Winter, color = ~Winter),
                 alpha = .5, binwidth = 90) +
  #oooohhhhh pretty colors
    scale_fill_gradient(palette="Spectral", na.value="white") + 
  #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
}
