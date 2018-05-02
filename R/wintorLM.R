#### winTor analysis
## Combine literature values, and WCS data sets, extract values from raster 
## covariates and run linear models across. Predict values from each and test.

## Reed Hranac, 02/May/2018

## libraries
library(raster);library(tidyverse)
library(lubridate); library(broom)
library(rlang)

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

sngl.lm <- function(x){
 ###function for lapplying single predictor lm 
  f1.lm <- lm(formula = x$mod, data = x$dat)
  return(f1.lm)
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
f1.mod <- lapply(f1.list, sngl.lm)

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