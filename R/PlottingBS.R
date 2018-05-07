### Plotting the residuals spatially 
env.df
env.df$Resid <- frosty$residuals #extracted f3 model object

## data layer
frosty.rast <- raster(file.path(win.res, "f3Pred_frost.tif"))

wintorResid <- function(x, id, res.agg = 25,  save = F, ...){
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
  
  g.winTor <- ggplot() +
    aes(x = long, y = lat) +
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

frsty.pic <- wintorResid(x = frosty.rast, id = NULL)
frsty.pic + geom_text(data = env.df, aes(x= long, y = lat, label = round(Resid))) + 
  coord_cartesian(xlim = c(-138, -110), 
                  ylim = c(45, 60))

wintTorHighRes <- function(x, save, id,  ...){
  m <- rbind(c(-1000, 0, 0),
             c(365, 1000, 365))
  x.clean <- reclassify(x, rcl = m)
  x.pts <- data.frame(rasterToPoints(x.clean))
  colnames(x.pts) <- c("long", "lat", "Winter")
  
  win.plot <- ggplot(x.pts) +
    aes(x = long, y = lat) +
    coord_fixed(xlim = extent(x)[1:2],ylim=extent(x)[3:4])+
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(data = x.pts, aes(fill = Winter),  interpolate = T) +
    scale_fill_gradientn("Winter\nLength\n(days)",
                         colors = c("#e66101", "#fdb863","#ffffff", "#b2abd2", "#5e3c99")) + #purp low orange hi
    ggtitle(names(x)) + 
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  if(save == T){
    ggsave(filename = file.path(win.res, id),
           win.plot, ...)
    
    return(win.plot)
  }
  
}


wintTorHighRes(x = frosty.rast, save = T, id = "f3Frost.tif", device = "pdf")
