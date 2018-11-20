#### winTor analysis
## Combine literature values, and WCS data sets, extract values from raster 
## covariates and run linear models across. Predict values from each and test.

## Reed Hranac, 02/May/2018

## update 09/July/2018

## libraries
library(raster);library(tidyverse)
library(lubridate); library(broom)
library(AICcmodavg); library(maptools)

## Extra Paths
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

#### Functions ####
scrapeResults <- function(x){
  ### Function for creating an easy results table 
  results <- tidy(x)
  tidy.table <- results[which(results$term != "(Intercept)"),]
  bonus.cols <- cbind(intercept.est = results[which(results$term == "(Intercept)"), "estimate"],
                      intercept.Pval = results[which(results$term == "(Intercept)"), "p.value"],
                      r.squared = summary(x)$r.squared)
  res.out <- cbind(tidy.table, bonus.cols)
  return(res.out)
}

rapid.lm.clean <- function(x, name){
  ###function for lapplying lm
  ## updated methods to remove outliers internally
  ## x consists of $dat (modeling data frame) & $mod (model formula)
  
  ## name <- string for naming file results when written out
  
  ## outputs final lm, and new modified dataframe 
  
  ## add row names to try to select what comes up
  rownames(x$dat) <- paste0("a",1:nrow(x$dat))
  
  ## initial lm
  f1.lm <- lm(formula = x$mod, data = x$dat)
  
  ## filter based on residuals
  lm.df2 <- x$dat %>% ## select based on rownames and feed back through
    rownames_to_column("rn") %>%
    dplyr::filter(rn %!in% names(which(abs(rstandard(f1.lm))>2))) %>%
    column_to_rownames("rn")
    
  ##Run second lm
  f2.lm <- lm(formula = x$mod, data = lm.df2)
  
  lm.df3 <- lm.df2 %>%
    rownames_to_column("rn") %>%
    dplyr::filter(rn %!in% names(which(abs(rstandard(f2.lm))>2))) %>%
    column_to_rownames("rn")
  
  dim(lm.df3)
  
  ##third?
  f3.lm <- lm(formula = x$mod, data = lm.df3)
  lm.df4 <- lm.df3 %>%
    rownames_to_column("rn") %>%
    dplyr::filter(rn %!in% names(which(abs(rstandard(f3.lm))>2))) %>%
    column_to_rownames("rn")
  dim(lm.df4)
  
  df.rm <- x$dat %>%
    rownames_to_column("rn") %>%
    dplyr::filter(rn %in% rownames(lm.df3)) %>%
    column_to_rownames("rn")
    
  
  ## create a statement to tell me which row were removed
  if(nrow(x$dat) != nrow(lm.df3)){
    cat( as.character(x$dat$ID[which(row.names(x$dat) %!in% row.names(lm.df3))]),
         " were removed during model fit", paste0(replace(x$mod[length(x$mod)], "+", "_")), "\n")
  }
  
  pdf(file = file.path(win.res,paste0(name,replace(x$mod[length(x$mod)], "+", "_"),".pdf")))
  par(mfrow = c(3,2))
  plot(f3.lm, which = 1:6, main = x$mod[[length(x$mod)]], ask = F)
  dev.off()
  
  return(list(lm = f3.lm, dp.rm = df.rm))
}
# 
# wintorContour <- function(x, id, res.agg = 25,  save = F, ...){
#   ## Create DataFrame
#   if(!is.null(res.agg)){ #aggratetion bits
#     x.ag <- raster::aggregate(x, res.agg)
#   }
#   else{
#     x.ag <- x}
#   
#   x.class <- calc(x.ag, function(x) {x[is.na(x)] <- NA; as.factor(round(x/30))}) #make month factors 
#   m <- rbind(c(-1000, 0, 0),
#              c(0,2,2), 
#              c(2,4,4),
#              c(4,6,6),
#              c(6,8,8),
#              c(8,10,10),
#              c(10,12,12),
#              c(12, 1000, 12))
#   
#   rast.trimmed <- reclassify(x.class, m) #trim ends
#   x.pts <- rasterToPoints(rast.trimmed) #to points
#   x.df <- data.frame(x.pts)
#   colnames(x.df) <- c("long", "lat", "Winter")
#   x.df$Winter <- as.factor(x.df$Winter)
#   
#   g.winTor <- ggplot(x.df, aes(x = long, y = lat, z = Winter)) +
#     coord_fixed(xlim = extent(x)[1:2],ylim=extent(x)[3:4])+
#     #border lines
#     borders("world",
#             xlim=extent(x)[1:2],ylim=extent(x)[3:4],
#             colour = "grey20",
#             fill = "grey80")+
#     #Raster fill
#     geom_raster(aes(fill = Winter),  interpolate = T) +
#     #oooohhhhh pretty colors
#     scale_fill_brewer(palette="Spectral", na.value="white") +
#     #general malarkey
#     ggtitle(names(x)) + 
#     scale_x_continuous(expand = c(0,0))+
#     scale_y_continuous(expand = c(0,0))+
#     theme_bw()
#   
#   if(save == T){
#     ex <- as.vector(extent(x))
#     aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
#     ggsave(filename = file.path(win.res, paste0(id, names(x))),
#            g.winTor, width = 7, height = 7/aspect.ratio,
#            ...)
#   }
#   
#   return(g.winTor)
# }
# wintorContinious <- function(x, id, res.agg = 25, save = F, ...){
#   ## Create DataFrame
#   if(!is.null(res.agg)){ #aggratetion bits
#     x.ag <- raster::aggregate(x, res.agg)
#   }
#   else{
#     x.ag <- x}
#   
#   ## round and trim
#   x.count <- calc(x.ag, function(x) {x[is.na(x)] <- NA; round(x/30, 2)})
#   m <- rbind(c(-1000, 0, 0))
#   rast.trimmed <- reclassify(x.count, m) #trim ends
#   
#   x.pts <- rasterToPoints(rast.trimmed) #to points
#   x.df <- data.frame(x.pts)
#   colnames(x.df) <- c("long", "lat", "Winter")
#   #x.df$Winter <- as.factor(x.df$Winter)
#   
#   g.winTor <- ggplot(x.df, aes(x = long, y = lat, z = Winter)) +
#     coord_fixed(xlim = extent(x)[1:2],ylim=extent(x)[3:4])+
#     #border lines
#     borders("world",
#             xlim=extent(x)[1:2],ylim=extent(x)[3:4],
#             colour = "grey20",
#             fill = "grey80")+
#     #Raster fill
#     geom_raster(aes(fill = Winter),  interpolate = T) +
#     #oooohhhhh pretty colors
#     scale_fill_gradientn(colors = brewer.pal(8, "YlGnBu"), na.value="white") +
#     #general malarkey
#     scale_x_continuous(expand = c(0,0))+
#     scale_y_continuous(expand = c(0,0))+
#     theme(plot.title = element_text(hjust = .05))+
#     ggtitle("Predicted Winter Length") + 
#     theme_bw()
#   
#   if(save == T){
#     ex <- as.vector(extent(x))
#     aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
#     ggsave(filename = file.path(win.res, paste0(id, names(x))),
#            g.winTor, width = 7, height = 7/aspect.ratio,
#            ...)
#     return(g.winTor)
#   }
# }
# wintorContinious2 <- function(x, id, res.agg = 25, save = F, ...){
#   ## Create DataFrame
#   if(!is.null(res.agg)){ #aggratetion bits
#     x.ag <- raster::aggregate(x, res.agg)
#   }
#   else{
#     x.ag <- x}
#   
#   ## round and trim
#   x.con <- calc(x.ag, function(x){x[is.na(x)] <- NA; batwintor::day.to.month(x)})
#   x.pts <- rasterToPoints(x.con) #to points
#   x.df <- data.frame(x.pts)
#   colnames(x.df) <- c("long", "lat", "Winter")
#   
#   
#   g.winTor <- ggplot(x.df, aes(x = long, y = lat, z = Winter)) +
#     coord_fixed(xlim = extent(x)[1:2],ylim=extent(x)[3:4])+
#     #border lines
#     borders("world",
#             xlim=extent(x)[1:2],ylim=extent(x)[3:4],
#             colour = "grey20",
#             fill = "grey80")+
#     #Raster fill
#     geom_raster(aes(fill = Winter),  interpolate = T) +
#     #oooohhhhh pretty colors
#     scale_fill_gradientn("Survival\nCapacity\n(months)",
#                          colors = c("#e66101", "#fdb863","#ffffff", "#b2abd2", "#5e3c99"),
#                          limits=  c(-8,8)) + 
#     #general malarkey
#     scale_x_continuous(expand = c(0,0))+
#     scale_y_continuous(expand = c(0,0))+
#     theme(plot.title = element_text(hjust = .05))+
#     ggtitle("Predicted Winter Survival") + 
#     theme_bw()
#   
#   if(save == T){
#     ex <- as.vector(extent(x))
#     aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
#     ggsave(filename = file.path(win.res, paste0(id, names(x))),
#            g.winTor, width = 7, height = 7/aspect.ratio,
#            ...)
#   }
# }

#### Data ####

env.df <- read.csv("data/modelingDataFrame.csv")

## Co-variates
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_OG1k")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)

#### explore data ####
# NA.extent <- c(-172.3, -52,23.5,66.5) #extent of NA from artic circle to tropics
# 
# point.map <- ggplot() +
#   borders("world",
#           xlim=extent(NA.extent)[1:2],ylim=extent(NA.extent)[3:4],
#           colour = "grey20",
#           fill = "grey80")+ 
#   geom_point(data = dat,
#              aes(x = long, y = lat, color = winter.duration))+
#   coord_equal(xlim=extent(NA.extent)[1:2],
#                   ylim=extent(NA.extent)[3:4],
#                   expand = F)+
#   theme_bw()
# ## points are obvious clustered. lets see what we can do.
# # create study extent as owin 
# n.america <- raster(file.path(win.dat, "NA_dem.tif"))
# NA.poly <- rasterToPolygons(n.america, fun = function(x){x==1}, dissolve = T)
# plot(NA.poly)
# library(spatstat)
# regions <- slot(NA.poly, "polygons")
# regions <- lapply(regions, function(x) { SpatialPolygons(list(x)) })
# windows <- lapply(regions, as.owin)
# #clean
# rm( regions)
# 
# ## points as a ppp
# dat.ppp <- ppp(x = dat$long, y = dat$lat,
#                window = windows[[1]],
#                marks = dat[,4:ncol(dat)])
# plot(dat.ppp)
#### Models ####

## formulas
mod.formulas <- list(winter.duration ~ NA_dem,
                     winter.duration ~ NA_northing,
                     winter.duration ~ NA_nonGrowingDays,
                     winter.duration ~ NA_nDaysFreeze,
                     winter.duration ~ NA_nFrostyDays,
                     winter.duration ~ NA_OG1k)

## args list
f1.list <- list()
for(i in 1:length(mod.formulas)){
  f1.list[[i]] <- list(mod = mod.formulas[[i]], dat = env.df)
}

## models
f1 <- lapply(f1.list, rapid.lm.clean, name = "newWin")
#split into dfs and models
f1.mod <- list();for(i in 1:length(f1)){f1.mod[[i]] <- f1[[i]]$lm}
f1.df <- list();for(i in 1:length(f1)){f1.df[[i]] <- f1[[i]]$dp.rm}


## summaries
f1.sum <- lapply(f1.mod, summary.lm)

## AIC tab
library(AICcmodavg)
f1.res <- aictab(f1.mod, 
                 modnames = c("dem",
                              "northing",
                              "frost",
                              "growing",
                              "freeze",
                              "OG"))
##write
write.csv(f1.res,file =  file.path(win.res, 'NEWwin1AICtable.csv'), row.names = F)

## results table
f1.scrape <- lapply(f1.mod, scrapeResults)
f1.scrape.df <- do.call(rbind, f1.scrape)

write.csv(f1.scrape.df, file = file.path(win.res, 'NEWwin1Results.csv'), row.names = F)

##Create prediction rasters
f1Pred.rasters <- lapply(f1.mod, FUN = raster::predict, object = env.stk)
pred.stk <- do.call(stack, f1Pred.rasters)
names(pred.stk) <- c("dem",
                     "northing",
                     "frost",
                     "growing",
                     "freeze",
                     "OG")
writeRaster(pred.stk, filename = file.path(win.res, "NEWwin1Pred"),
  format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

## Clean for memory
rm(pred.stk,f1Pred.rasters)

#### Level 2 models ####

## formulas
mod.formulas2 <- list(winter.duration ~ NA_northing + NA_dem,
                     winter.duration ~ NA_northing + NA_nFrostyDays,
                     winter.duration ~ NA_northing + NA_nonGrowingDays,
                     winter.duration ~ NA_northing + NA_nDaysFreeze,
                     winter.duration ~ NA_northing + NA_OG1k)

## args list
f2.list <- list()
for(i in 1:length(mod.formulas2)){
  f2.list[[i]] <- list(mod = mod.formulas2[[i]], dat = env.df)
}

## models
f2 <- lapply(f2.list, rapid.lm.clean, name = "NEWwin")
#split into dfs and models
f2.mod <- list();for(i in 1:length(f2)){f2.mod[[i]] <- f2[[i]]$lm}
f2.df <- list();for(i in 1:length(f2)){f2.df[[i]] <- f2[[i]]$dp.rm}

## summaries
f2.sum <- lapply(f2.mod, summary.lm)

## AIC table
f2.res <- aictab(f2.mod, 
                 modnames = c("dem",
                              "frost",
                              "growing",
                              "freeze",
                              "OG"))
#write
write.csv(f2.res,file =  file.path(win.res, 'NEWwin2AICtable.csv'), row.names = F)

## results table
f2.scrape <- lapply(f2.mod, scrapeResults)
f2.scrape.df <- do.call(rbind, f2.scrape)

write.csv(f2.scrape.df, file = file.path(win.res, 'NEWwin2Results.csv'), row.names = F)

# ## Create prediction rasters
# 
# f2Pred.rasters <- lapply(f2.mod, FUN = raster::predict, object = env.stk)
# pred.stk2 <- do.call(stack, f2Pred.rasters)
# names(pred.stk2) <- c("dem",
#                      "frost",
#                      "growing",
#                      "freeze",
#                      "OG")
# writeRaster(pred.stk2, filename = file.path(win.res, "win2Pred"),
#             format = "GTiff", bylayer = T, suffix = "names", overwrite = T)
# 
## Clean for memory
# rm(pred.stk2, f2Pred.rasters)

#### Level 3 models ####

## formulas
mod.formulas3 <- list(winter.duration ~ NA_northing + NA_nFrostyDays + NA_dem,
                      winter.duration ~ NA_northing + NA_nonGrowingDays + NA_dem,
                      winter.duration ~ NA_northing + NA_nDaysFreeze + NA_dem,
                      winter.duration ~ NA_northing + NA_OG1k + NA_dem)

f3.list <- list()
for(i in 1:length(mod.formulas3)){
  f3.list[[i]] <- list(mod = mod.formulas3[[i]], dat = env.df)
}

## models
f3<- lapply(f3.list, rapid.lm.clean, name = "NEWwin")
#split into dfs and models
f3.mod <- list();for(i in 1:length(f3)){f3.mod[[i]] <- f3[[i]]$lm}
f3.df <- list();for(i in 1:length(f3)){f3.df[[i]] <- f3[[i]]$dp.rm}
## Summaries
f3.sum <- lapply(f3.mod, summary.lm)

# 
# ## addressing the potential sptial auto corrilation
# n1Mod <- f3.mod[[1]]
# n1.resid <- data.table(lat =env.df$lat, long = env.df$long, resid =   n1Mod$residuals)
# 
# 
# ## plot residuals to map
# library(mapview)
# coordinates(n1.resid) <- ~long + lat
# proj4string(n1.resid) <- proj4string(env.stk)
# mapview(n1.resid)



## AIC table
f3.res <- aictab(f3.mod, 
                 modnames = c("frost",
                              "growing",
                              "freeze",
                              "OG"))
##write
write.csv(f3.res,file =  file.path(win.res, 'NEWwin3AICtable.csv'), row.names = F)

## results table
f3.scrape <- lapply(f3.mod, scrapeResults)
f3.scrape.df <- do.call(rbind, f3.scrape)

write.csv(f3.scrape.df, file = file.path(win.res, 'NEWwin3Results.csv'), row.names = F)

## Create prediction rasters
# 
# f3Pred.rasters <- lapply(f3.mod, FUN = raster::predict, object = env.stk)
# pred.stk3 <- do.call(stack, f3Pred.rasters)
# names(pred.stk3) <- c("frost",
#                       "growing",
#                       "freeze",
#                       "OG")
# writeRaster(pred.stk3, filename = file.path(win.res, "win3Pred"),
#             format = "GTiff", bylayer = T, suffix = "names", overwrite = T)
# 
# ## Clean for memory
# rm(pred.stk3, f3Pred.rasters)


#### making a bunch of pretty figures ####
major.table <- aictab(c(f1.mod, f2.mod, f3.mod), 
                      modnames = c("dem",
                                   "northing",
                                   "frost",
                                   "growing",
                                   "freeze",
                                   "OG",
                                   "north + dem",
                                   "north + frost",
                                   "north + growing",
                                   "north + freeze",
                                   "north + OG",
                                   "north + dem + frost",
                                   "north + dem + growing",
                                   "north + dem + freeze",
                                   "north + dem + OG"))
write.csv(major.table, file =  file.path(win.res, 'NEWwinModelAICtable.csv'), row.names = F)

# win.maps <- lapply(unstack(pred.stk),
#                    wintorContour,
#                    res.agg = NULL,
#                    save = T,
#                    id = "f1",
#                    device = "pdf")
# 
# win.maps2 <- lapply(unstack(pred.stk2),
#                    wintorContour,
#                    res.agg = NULL,
#                    save = T,
#                    id = "f2",
#                    device = "pdf")
# 
# win.maps3 <- lapply(unstack(pred.stk3),
#                    wintorContour,
#                    res.agg = NULL,
#                    save = T,
#                    id = "f3",
#                    device = "pdf")
# 

#### Variogram of the top model ####
library(gstat)

#top model
mod <- f1.mod[[4]]
mod.df <- f1.df[[4]]

#Create locations points
coordinates(mod.df) <- ~ long + lat
proj4string(mod.df) <- proj4string(env.stk)

#formula winter.duration ~ NA_northing + NA_nonGrowingDays + NA_dem
v <- variogram( winter.duration ~  NA_nDaysFreeze,
               data = mod.df)
v.exp = fit.variogram(v, vgm("Exp"), fit.kappa = T)
v.mat <- fit.variogram(v, vgm("Mat"), fit.kappa = T)
v.sph <- fit.variogram(v, vgm("Sph"), fit.kappa = T)

par(mfrow = c(1,3))

plot(v, v.exp)
plot(v, v.mat)
plot(v, v.sph)


## Looks like the correlation is fine, we'll leave with this model structure

#### Uncertainty ####
#top model
mod <- f3.mod[[2]]
mod.df <- f3.df[[2]]
# predfun <- function(model, data) {
#   v <- predict(model, data, interval = "prediction")
#   cbind(p=as.vector(v[,"fit"]), lwr=as.vector(v[,"lwr"]), upr=as.vector(v[,"upr"]) )
# }
# 
# pred.int <- raster::predict(mod, object = env.stk, fun = predfun, index = 1:3)
# names(pred.int) <- c("p", "lwr", "upr")
# 
# pred.int$diff <- pred.int$upr - pred.int$lwr
# plot(pred.int$diff)
# 
# 
# writeRaster(pred.int,
#             filename = file.path(win.res, "winSE"),
#             format = "GTiff",
#             bylayer = T,
#             suffix = "names",
#             overwrite = T)

conffun <- function(model, data) {
  v <- predict(model, data, interval = "confidence")
  cbind(p=as.vector(v[,"fit"]), lwr=as.vector(v[,"lwr"]), upr=as.vector(v[,"upr"]) )
}

conf.int <- raster::predict(mod, object = env.stk, fun = conffun, index = 1:3)
names(conf.int) <- c("p", "lwr", "upr")

conf.int$diff <- conf.int$upr - conf.int$lwr
plot(conf.int$diff)


writeRaster(conf.int,
            filename = file.path(win.res, "winSE_Conf"),
            format = "GTiff",
            bylayer = T,
            suffix = "names",
            overwrite = T)


#### Plots for top Layer ####
newWIN <- raster(file.path(win.res, "NEWwin1Pred_freeze.tif"))
win.plot <- function(x, save.name = NULL, res.agg = 25,  dist.map = NULL, ...){
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  ## Crop and mask to distribution
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
  }
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "winter")
  
  g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
    coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
    #border lines
    borders("world",
            xlim=extent(x)[1:2],ylim=extent(x)[3:4],
            colour = "grey20",
            fill = "grey80")+
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\nDuration\nWinter (g)",
                         colors = c("#5e3c99","#b2abd2", "#ffffff","#fdb863", "#e66101" ),
                         limits=  c(floor(minValue(x.ag)),
                                    ceiling(maxValue(x.ag)))) +
    
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    #ggtitle("Predicted Body Mass") + 
    theme_bw()
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    g.win <- g.win +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("data", save.name),
           g.Fat, width = 7, height = 7/aspect.ratio, dpi = 900,
           ...)}
  
  return(g.win)
}
z <- win.plot(newWIN)

win <- raster(file.path(win.res, "win3Pred_growing.tif"))
win.bin <- wintorContour(win)

plot(win)
plot(newWIN)
