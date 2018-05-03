#### Post Prediction ####

##Links
win.dat <- file.path("D:", "Dropbox", "winTor_aux", "data")
win.res <- file.path("D:", "Dropbox", "winTor_aux", "Results")

##libraries
library(tidyverse); library(raster); library(RColorBrewer)


z <- raster(file.path(win.res, "f3pred_freeze.tif"))
names(z) <- "frost"
x <- z


wintor <- function(x, res.agg = 25, id,  save = F, ...){
  ## Create DataFrame
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
   
  x.class <- calc(x.ag, function(x) {x[is.na(x)] <- NA; as.factor(round(x/30))}) #make month factors 
  m <- rbind(c(-1000, 0, 0),
             c(0,2,2), 
             c(2,4,4),
             c(4,6,6),
             c(6,8,8),
             c(8,10,10),
             c(10,12,12),
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
    geom_raster(data = x.df, aes(fill = Winter),  interpolate = T) +
  #oooohhhhh pretty colors
    scale_fill_brewer(palette="Spectral", na.value="white") +
  #general malarkey
    ggtitle(names(x)) + 
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
    
  if(save == T){
    ggsave(filename = file.path(win.res, paste0(id, names(x))),
                  g.winTor, ...)
  }
  
  return(g.winTor)
}

wintor(x = z, res.agg = NULL,id = "Main", save = T, device = "pdf")



win.maps <- lapply(unstack(dif.win),
                   wintorContour,
                   res.agg = NULL,
                   save = T,
                   device = "pdf")
