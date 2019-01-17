### Survival capacity figure

#### Extra Paths ####
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

library(tidyverse);library(raster)

## Read in raster data

  ## fat required
null.stk <- stack(list.files(win.res, pattern = "*_98_4_fat.null.tif",full.names = T))
names(null.stk) <- c("est", "lwr", "upr")
## the CI bounds for the fat required are generally w/in .5g 

inf.stk <- stack(list.files(win.res, pattern = "*_98_4_fat.inf.tif",full.names = T))
names(inf.stk) <- c("est", "lwr", "upr")
## bounds are larger for this up to 2, but generally ~.5

fatReq <- stack(null.stk, inf.stk)

  ## fat avaliable 
massEst <- raster(file.path(win.res, "massRaster_p.tif"))
fatEst <- calc(massEst, fun = function(x){-2.84 + 0.593*x})

## I think this is working?
fatBin <- function(x, y){
## fuction for doing some shit that I'll probably never actually use. who knows
  ## x <- fat avalible
  ## y <- stack of fat required ordered: est, lwr, upr
  x.matrix <- as.vector(x)
  y.matrix <- as.matrix(y)

  doo <- function(x, y){
    z <- ifelse(is.na(x),NA,
                5*(x>=y[,3])+
                  1*(x<=y[,2])+
                  2*(x < y[,1] & x > y[,3])+
                  4*(x > y[,2] & x < y[,1])+
                       3*(x==y[,1]))
    return(z)
    }
  
  binned <- doo(x.matrix, y.matrix)
  te <- x
  values(te) <- binned

  return(te)
}


survConf.plot <- function(x, y, c.string, res.agg = 25, dist.map = NULL,
                         save.name = NULL, device.out = NULL, ...){
  ## function for creating a binned survival map with defference played to the CI
  
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
    y.ag <- raster::aggregate(y, res.agg)
  }
  else{
    x.ag <- x
    y.ag <- y
  }
  ## Crop and mask to distribution
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
    y.ag <- mask(crop(y.ag, dist.map), dist.map)
  }
  
  ## bin that shit
  binned <- fatBin(x.ag, y.ag)
  
  ## to dataframe
  binned.points <- rasterToPoints(binned)
  binned.df <- data.frame(binned.points)
  colnames(binned.df) <- c("long", "lat", "bin")
  binned.df$bin <- as.factor(binned.df$bin)
  
  surv.plot <- ggplot(data = binned.df, 
                      aes(x = long, y = lat, z = bin))+
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    geom_raster(aes(fill = bin)) +
    scale_discrete_manual(values = c.string,
                          aesthetics = "fill") +
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    theme_bw()
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    surv.plot <- surv.plot +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  return(surv.plot)
}

(inf.test <- survConf.plot(x= fatReq, y = inf.stk, 
                          c.string <- cat.Colors))

cat.Colors <- c("#ca0020", "#f4a582", "#f7f7f7", "92c5de", "#0571b0")


