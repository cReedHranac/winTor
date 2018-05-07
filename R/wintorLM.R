#### winTor analysis
## Combine literature values, and WCS data sets, extract values from raster 
## covariates and run linear models across. Predict values from each and test.

## Reed Hranac, 02/May/2018

## libraries
library(raster);library(tidyverse)
library(lubridate); library(broom)
library(AICcmodavg)

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

#### Functions ####
scrapeResults <- function(x){
  ### FUnction for creating an easy results table 
  results <- tidy(x)
  tidy.table <- results[which(results$term != "(Intercept)"),]
  bonus.cols <- cbind(intercept.est = results[which(results$term == "(Intercept)"), "estimate"],
                      intercept.Pval = results[which(results$term == "(Intercept)"), "p.value"],
                      r.squared = summary(x)$r.squared)
  res.out <- cbind(tidy.table, bonus.cols)
  return(res.out)
}

rapid.lm <- function(x){
 ###function for lapplying lm
  f1.lm <- lm(formula = x$mod, data = x$dat)
  pdf(file = file.path(win.res,paste0(replace(x$mod[length(x$mod)], "+", "_"),".pdf")))
  par(mfrow = c(3,2))
  plot(f1.lm, which = 1:6, main = x$mod[[length(x$mod)]], ask = F)
  dev.off()
  
  return(f1.lm)
}

wintorContour <- function(x, id, res.agg = 25,  save = F, ...){
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
    geom_raster(aes(fill = Winter),  interpolate = T) +
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

#### Data ####

## literature data
lit.dat <- as_tibble(read.csv("data/durationData.csv"))
lit.dat$id <- paste0("L", 1:nrow(lit.dat)) ## add id

  ## determine duration 
lit.dat$Start <- as.Date(lit.dat$Start, '%d-%h')
lit.dat$End <- as.Date(lit.dat$End, '%d-%h')


for(i in 1:nrow(lit.dat)){ # Create duration since there's no years in the df
  ifelse(is.na(lit.dat$Duration[[i]]), 
         lit.dat$Duration[[i]] <- 365 - (lit.dat$Start[[i]]- lit.dat$End[[i]]),
         lit.dat$Duration[[i]] <- lit.dat$Duration[[i]])  
}

lit.sub <- lit.dat %>%
  dplyr::select(id, long, lat, Duration) %>%
  rename(winter.duration = Duration)


  ## recorder data
rec.dat <- as_tibble(read.csv("data/winDurationClean.csv"))

rec.sub <- rec.dat %>%
  dplyr::select(id, long, lat, winter.duration) %>%
  filter(!is.na(winter.duration))

## join 
dat <- bind_rows(lit.sub, rec.sub)
dat$ID <- 1:nrow(dat)

## add spatial orientation
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
coordinates(dat) <- ~ long + lat
proj4string(dat)  <- wgs84

## Co-variates
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_frostFreeze")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)

## extract data from locations
env.dat <- raster::extract(env.stk, dat, cellnumber = T, df = T)

env.df <- as_tibble(left_join(as.data.frame(dat), env.dat, by = "ID"))

#### Models ####

## formulas
mod.formulas <- list(winter.duration ~ NA_dem,
                     winter.duration ~ NA_northing,
                     winter.duration ~ NA_nFrostyDays,
                     winter.duration ~ NA_nonGrowingDays,
                     winter.duration ~ NA_nDaysFreeze,
                     winter.duration ~ NA_frostFreeze)

## args list
f1.list <- list()
for(i in 1:length(mod.formulas)){
  f1.list[[i]] <- list(mod = mod.formulas[[i]], dat = env.df)
}

## models
f1.mod <- lapply(f1.list, rapid.lm)


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
                              "frostFreeze"))
##write
write.csv(f1.res,file =  file.path(win.res, 'f1AICtable.csv'), row.names = F)

## results table
f1.scrape <- lapply(f1.mod, scrapeResults)
f1.scrape.df <- do.call(rbind, f1.scrape)

write.csv(f1.scrape.df, file = file.path(win.res, 'f1Results.csv'), row.names = F)

##Create prediction rasters

f1Pred.rasters <- lapply(f1.mod, FUN = raster::predict, object = env.stk)
pred.stk <- do.call(stack, f1Pred.rasters)
names(pred.stk) <- c("dem",
                     "northing",
                     "frost",
                     "growing",
                     "freeze",
                     "frostFreeze")
writeRaster(pred.stk, filename = file.path(win.res, "f1Pred"),
            format = "GTiff", bylayer = T, suffix = "names")

#### Level 2 models ####

## formulas
mod.formulas2 <- list(winter.duration ~ NA_northing + NA_dem,
                     winter.duration ~ NA_northing + NA_nFrostyDays,
                     winter.duration ~ NA_northing + NA_nonGrowingDays,
                     winter.duration ~ NA_northing + NA_nDaysFreeze,
                     winter.duration ~ NA_northing + NA_frostFreeze)

## args list
f2.list <- list()
for(i in 1:length(mod.formulas2)){
  f2.list[[i]] <- list(mod = mod.formulas2[[i]], dat = env.df)
}

## models
f2.mod <- lapply(f2.list, rapid.lm)

## summaries
f2.sum <- lapply(f2.mod, summary.lm)

## AIC table
f2.res <- aictab(f2.mod, 
                 modnames = c("dem",
                              "frost",
                              "growing",
                              "freeze",
                              "frostFreeze"))
##write
write.csv(f2.res,file =  file.path(win.res, 'f2AICtable.csv'), row.names = F)

## results table
f2.scrape <- lapply(f2.mod, scrapeResults)
f2.scrape.df <- do.call(rbind, f2.scrape)

write.csv(f2.scrape.df, file = file.path(win.res, 'f2Results.csv'), row.names = F)

## Create prediction rasters

f2Pred.rasters <- lapply(f2.mod, FUN = raster::predict, object = env.stk)
pred.stk2 <- do.call(stack, f2Pred.rasters)
names(pred.stk2) <- c("dem",
                     "frost",
                     "growing",
                     "freeze",
                     "frostFreeze")
writeRaster(pred.stk2, filename = file.path(win.res, "f2Pred"),
            format = "GTiff", bylayer = T, suffix = "names")

#### Level 3 models ####

## formulas
mod.formulas3 <- list(winter.duration ~ NA_northing + NA_nFrostyDays + NA_dem,
                      winter.duration ~ NA_northing + NA_nonGrowingDays + NA_dem,
                      winter.duration ~ NA_northing + NA_nDaysFreeze + NA_dem,
                      winter.duration ~ NA_northing + NA_frostFreeze + NA_dem)

f3.list <- list()
for(i in 1:length(mod.formulas3)){
  f3.list[[i]] <- list(mod = mod.formulas3[[i]], dat = env.df)
}

## models
f3.mod <- lapply(f3.list, rapid.lm)

## Summaries
f3.sum <- lapply(f3.mod, summary.lm)

## plots
## add some trickery to split the plots and the models
f3.plots <- list()
for(i in 1:length(f3.mod)){
  f3.plots[[i]] <- f3.mod[[i]][[2]]
  f3.mod[[i]] <- f3.mod[[i]][[1]]
} # in order to view you need to call plot on the items

## AIC table
f3.res <- aictab(f3.mod, 
                 modnames = c("frost",
                              "growing",
                              "freeze",
                              "frostFreeze"))
##write
write.csv(f3.res,file =  file.path(win.res, 'f3AICtable.csv'), row.names = F)

## results table
f3.scrape <- lapply(f3.mod, scrapeResults)
f3.scrape.df <- do.call(rbind, f3.scrape)

write.csv(f3.scrape.df, file = file.path(win.res, 'f3Results.csv'), row.names = F)

## Create prediction rasters

f3Pred.rasters <- lapply(f3.mod, FUN = raster::predict, object = env.stk)
pred.stk3 <- do.call(stack, f3Pred.rasters)
names(pred.stk3) <- c("frost",
                      "growing",
                      "freeze",
                      "frostFreeze")
writeRaster(pred.stk2, filename = file.path(win.res, "f3Pred"),
            format = "GTiff", bylayer = T, suffix = "names")

#### making a bunch of pretty figures ####
major.table <- aictab(c(f1.mod, f2.mod, f3.mod), 
                      modnames = c("dem",
                                   "northing",
                                   "frost",
                                   "growing",
                                   "freeze",
                                   "frostFreeze",
                                   "north + dem",
                                   "north + frost",
                                   "north + growing",
                                   "north + freeze",
                                   "north + frostFreeze",
                                   "north + dem + frost",
                                   "north + dem + growing",
                                   "north + dem + freeze",
                                   "north + dem + frostFreeze"))
write.csv(major.table, file =  file.path(win.res, 'allModelAICtable.csv'), row.names = F)

win.maps <- lapply(unstack(pred.stk),
                   wintorContour,
                   res.agg = NULL,
                   save = T,
                   id = "f1",
                   device = "pdf")

win.maps2 <- lapply(unstack(pred.stk2),
                   wintorContour,
                   res.agg = NULL,
                   save = T,
                   id = "f2",
                   device = "pdf")

win.maps3 <- lapply(unstack(pred.stk3),
                   wintorContour,
                   res.agg = NULL,
                   save = T,
                   id = "f3",
                   device = "pdf")
