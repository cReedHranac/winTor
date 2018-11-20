### Evaluating mass accross the new length of winter scripts

library(batwintor)

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

#### Functions ####
##were going to need to modify the 

library(tidyverse); library(data.table)
survivalFat <- function(mod.df, pct.rh.rast, temp.rast, win.rast){
  #Raster modifications for Kelvin temperatures
  if(summary(temp.rast)[1] > 200){
    temp.c <- temp.rast - 273
  } else{
    temp.c <- temp.rast
  }
  
  #Creating output raster dimensions
  out <- raster(pct.rh.rast); values(out) <- NA
  out.s <- stack(out,out); names(out.s) <- c("fat.inf", "fat.null")
  
  #Extract data from rasters  to matrix for speed
  pct.rh <- as.matrix(pct.rh.rast, nrow = nrow(pct.rh.rast), ncol = ncol(pct.rh.rast))
  temp <- as.matrix(temp.c, nrow = nrow(temp.c), ncol = ncol(temp.c))
  win <- as.matrix(win.rast, nrow = nrow(win.rast), ncol = ncol(win.rast))
  
  ## shifting hours to days
  mod.dif <- mod.df %>%
    mutate_(days = ~hour.to.day(time)) 
  
  ####Look Up Table ####
  #Vectors for look up table structure
  Ta_vals <- unique(mod.dif$Ta)
  pct.rh_vals <- unique(mod.dif$pct.rh)
  days_vals <- unique(mod.dif$days)
  
  #Look Up Table
  lut <- array(NA, dim=c(length(Ta_vals), length(pct.rh_vals), length(days_vals),  2)) # 2 for fat.inf and fat.null
  dimnames(lut)[[1]] <- Ta_vals
  dimnames(lut)[[2]] <- pct.rh_vals
  dimnames(lut)[[3]] <- days_vals
  dimnames(lut)[[4]] <- c("fat.inf", "fat.null")
  
  
  #Fill look up table
  for (i in seq_len(nrow(mod.dif))) {
    d <- mod.dif[i,]
    if (i %% 10000 == 0) {
      cat("Look up table generation up to", i, "of", nrow(mod.dif), "\n")
    }
    lut[as.character(d$Ta), as.character(d$pct.rh), as.character(d$days),] <- c(d$g.fat.consumed,
                                                          d$n.g.fat.consumed)
  }
  
  ####Find closest####
  find_closest <- function(x, y) {
    # Find the closest item in the vector y to x.
    # NOTE: Assumes that y is increasing, equi-spaced vector
    dy <- (y[length(y)] - y[1]) / (length(y)-1)
    wch <- round((x - y[1]) / dy + 1)
    # check the range.
    clamp <- function(x, xmin, xmax) {
      min(max(x, xmin),xmax)
    }
    
    clamp(wch, 1, length(y))
  }
  
  #Run lookup
  for(j in 1:nlayers(out.s)){
    #Create output matrix
    out.z <- matrix(ncol = ncol(pct.rh), nrow = nrow(pct.rh))
    for(i in 1:length(pct.rh)){
      # first find the closest humidity and Ta
      if(i %% 1000 == 0){
        cat("Raster layer: ", j, "of", nlayers(out.s), "up to", i, "of", length(pct.rh), "\n")
      }
      pct.rh_i <- find_closest(pct.rh[[i]], pct.rh_vals)
      Ta_i  <- find_closest(temp[[i]], Ta_vals)
      win_i <- find_closest(win[[i]], days_vals)
      out.z[[i]] <- lut[Ta_i, pct.rh_i, win_i, j]
    }
    # Set values back from matrix to raster
    out.s[[j]] <- setValues(out.s[[j]], out.z)
  }
  return(out.s)
}

library(raster)
### Takes forever (+48 hours) run sparingly
mod.big <- fread("D://Dropbox/winTor_aux/data/myluModHUGE.csv")
win <- raster(file.path(win.res, "win3Pred_growing.tif"))
rh <- raster("D://Dropbox/batwintor_aux/paramFiles/RH_NA.tif")
mat <- raster("D://WorldClim/bclim/bio_1.bil")

rh.fix <- projectRaster(rh, win); rm(rh)
mat. <- projectRaster(mat, win); rm(mat)
mat.fix <- calc(mat., function(x){x/10}); rm(mat.)
rm(win)
library(batwintor)

fat.rast <- survivalFat(mod.df = mod.big,
                        pct.rh.rast = rh.fix,
                        temp.rast = mat.fix,
                        win.rast = win)

writeRaster(fat.rast,
            filename = file.path(win.res, "MYLU.tif"),
            format = "GTiff",
            bylayer = T,
            suffix = "names",
            overwrite = T)

#### uncertianty win ####
# win.lwr <- raster(file.path(win.res, "winSE_Conf_lwr.tif"))
# 
# fat.lwr <- survivalFat(mod.df = mod.big,
#                         pct.rh.rast = rh.fix,
#                         temp.rast = mat.fix,
#                         win.rast = win.lwr)
# 
# writeRaster(fat.lwr,
#             filename = file.path(win.res, "MYLU_fat_Conf_LWR.tif"),
#             format = "GTiff",
#             bylayer = T,
#             suffix = "names",
#             overwrite = T)
# rm(win.lwr, fat.lwr)
# 
# 
# 
# win.upr <- raster(file.path(win.res, "winSE_Conf_upr.tif"))
# 
# fat.upr <- survivalFat(mod.df = mod.big,
#                        pct.rh.rast = rh.fix,
#                        temp.rast = mat.fix,
#                        win.rast = win.upr)
# 
# writeRaster(fat.upr,
#             filename = file.path(win.res, "MYLU_fat_Conf_upr.tif"),
#             format = "GTiff",
#             bylayer = T,
#             suffix = "names",
#             overwrite = T)
# rm(win.upr, fat.upr)

#### Gathered Data ####
# dat <- read.csv("data/massLocations.csv")
# coordinates(dat) <- ~ Long + Lat
# proj4string(dat) <- proj4string(fat.rast)
# dat.fat <- as.data.frame(cbind(dat, raster::extract(fat.rast, dat)))
# 
# dat.fat <- dat.fat %>%
#   mutate(resid.inf = g.fat - fat.inf,
#          resid.null = g.fat - fat.null)
# 
# ##Explore
# hist(dat.fat$resid.null)
# ## how many below 0
# length(which(dat.fat$resid.null < 0))
# length(which(dat.fat$resid.null < -2))
# 
# hist(dat.fat$resid.inf)
# length(which(dat.fat$resid.inf > 0 ))

#### Figures ####
massWinner <- raster(file.path(win.res, "fat3Pred_freeze.tif"))
MassContinious <- function(x, id, res.agg = 25, save = F, ...){
  ## Create DataFrame
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  
  ## round and trim
  
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "Mass")
  
  g.winTor <- ggplot(x.df, aes(x = long, y = lat, z = Mass)) +
    coord_fixed(xlim = extent(x)[1:2],ylim=extent(x)[3:4])+
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = Mass),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn(colors = brewer.pal(8, "YlGnBu"), na.value="white") +
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    ggtitle("Predicted Body Mass") + 
    theme_bw()
  
  if(save == T){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", paste0(id, names(x), ".pdf")),
           g.winTor, width = 7, height = 7/aspect.ratio, dpi = 900,
           ...)
  }
  return(g.winTor)
}

massPlot <- MassContinious(massWinner, id = "winner", res.agg = NULL, save = T, device = "pdf")
