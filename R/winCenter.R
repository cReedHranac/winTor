#### Finding the center of witner? ####
library(tidyselect); library(skimr); library(rgdal); library(raster); library(lubridate)

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


#### data ####
dat <- tbl_df(read.csv("data/modelingDataFrame.csv", stringsAsFactors = F))
# create dates
dat$Start <- as.Date(dat$Start)
dat$End <- as.Date(dat$End)
# remove cells (not needed)
dat <- dat[,-7]
#remove L6 (no start or end)
dat <- dat[-6,]

#create center metrics
dat.center <- dat %>%
  mutate(win.center = Start + floor(winter.duration/2),
         n.Start = yday(Start) - 182, ##create columns for numerical date of year
         n.End = yday(End) + 183, 
         n.center = abs(yday(win.center) - 182))# %>%

## check : duration should remain the same          
all.equal(dat.center$n.End - dat.center$n.Start, dat.center$winter.duration)
## still not perfect. there appear to be 10 insances of leap years involved... good enough?
## yes

## check to make sure that center is inbetween start and end
dat.center$n.Start < dat.center$n.center && dat.center$n.center < dat.center$n.End #TRUE

##Env stack for rasters
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_frostFreeze", "NA_OG1k")
env.stk <- raster::stack(paste0(win.dat,"/",env.names,".tif"))

#### Modeling ####
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

rapid.lm <- function(x,name){
  ###function for lapplying lm
  f1.lm <- lm(formula = x$mod, data = x$dat)
  pdf(file = file.path(win.res,paste0(name,replace(x$mod[length(x$mod)], "+", "_"),".pdf")))
  par(mfrow = c(3,2))
  plot(f1.lm, which = 1:6, main = x$mod[[length(x$mod)]], ask = F)
  dev.off()
  
  return(f1.lm)
}

## formulas
mod.formulas <- list(n.center ~ NA_dem,
                     n.center ~ NA_northing,
                     n.center ~ NA_nFrostyDays,
                     n.center ~ NA_nonGrowingDays,
                     n.center ~ NA_nDaysFreeze,
                     n.center ~ NA_frostFreeze,
                     n.center ~ NA_OG1k)

## args list
c1.list <- list()
for(i in 1:length(mod.formulas)){
  c1.list[[i]] <- list(mod = mod.formulas[[i]], dat = dat.center)
}

## models
c1.mod <- lapply(c1.list, rapid.lm, name = "c1")


## summaries
c1.su m<- lapply(c1.mod, summary.lm)


## AIC tab
library(AICcmodavg)
c1.res <- aictab(c1.mod, 
                 modnames = c("dem",
                              "northing",
                              "frost",
                              "growing",
                              "freeze",
                              "frostFreeze",
                              "OG"))
##write
write.csv(c1.res,file =  file.path(win.res, 'c1AICtable.csv'), row.names = F)

## results table
c1.scrape <- lapply(c1.mod, scrapeResults)
c1.scrape.df <- do.call(rbind, c1.scrape)

write.csv(c1.scrape.df, file = file.path(win.res, 'c1Results.csv'), row.names = F)

##Create prediction rasters

c1Pred.rasters <- lapply(c1.mod, FUN = raster::predict, object = env.stk)
pred.stk <- do.call(stack, c1Pred.rasters)
names(pred.stk) <- c("dem",
                     "northing",
                     "frost",
                     "growing",
                     "freeze",
                     "frostFreeze",
                     "OG")
writeRaster(pred.stk, filename = file.path(win.res, "c1Pred"),
            format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

#### Level 2 models ####

## formulas
mod.formulas2 <- list(n.center ~ NA_northing + NA_dem,
                      n.center ~ NA_northing + NA_nFrostyDays,
                      n.center ~ NA_northing + NA_nonGrowingDays,
                      n.center ~ NA_northing + NA_nDaysFreeze,
                      n.center ~ NA_northing + NA_frostFreeze,
                      n.center ~ NA_northing + NA_OG1k)

## args list
c2.list <- list()
for(i in 1:length(mod.formulas2)){
  c2.list[[i]] <- list(mod = mod.formulas2[[i]], dat = dat.center)
}

## models
c2.mod <- lapply(c2.list, rapid.lm, name = "c2")

## summaries
c2.sum <- lapply(c2.mod, summary.lm)

## AIC table
c2.res <- aictab(c2.mod, 
                 modnames = c("dem",
                              "frost",
                              "growing",
                              "freeze",
                              "frostFreeze",
                              "OG"))
#write
write.csv(c2.res,file =  file.path(win.res, 'c2AICtable.csv'), row.names = F)

## results table
c2.scrape <- lapply(c2.mod, scrapeResults)
c2.scrape.df <- do.call(rbind, c2.scrape)

write.csv(c2.scrape.df, file = file.path(win.res, 'c2Results.csv'), row.names = F)

## Create prediction rasters

c2Pred.rasters <- lapply(c2.mod, FUN = raster::predict, object = env.stk)
pred.stk2 <- do.call(stack, c2Pred.rasters)
names(pred.stk2) <- c("dem",
                      "frost",
                      "growing",
                      "freeze",
                      "frostFreeze",
                      "OG")
writeRaster(pred.stk2, filename = file.path(win.res, "c2Pred"),
            format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

#### Level 3 models ####

## formulas
mod.formulas3 <- list(n.center ~ NA_northing + NA_nFrostyDays + NA_dem,
                      n.center ~ NA_northing + NA_nonGrowingDays + NA_dem,
                      n.center ~ NA_northing + NA_nDaysFreeze + NA_dem,
                      n.center ~ NA_northing + NA_frostFreeze + NA_dem,
                      n.center ~ NA_northing + NA_OG1k + NA_dem)

c3.list <- list()
for(i in 1:length(mod.formulas3)){
  c3.list[[i]] <- list(mod = mod.formulas3[[i]], dat = dat.center)
}

## models
c3.mod <- lapply(c3.list, rapid.lm, name = "c3")

## Summaries
c3.sum <- lapply(c3.mod, summary.lm)

## AIC table
c3.res <- aictab(c3.mod, 
                 modnames = c("frost",
                              "growing",
                              "freeze",
                              "frostFreeze",
                              "OG"))
##write
write.csv(c3.res,file =  file.path(win.res, 'c3AICtable.csv'), row.names = F)

## results table
c3.scrape <- lapply(c3.mod, scrapeResults)
c3.scrape.df <- do.call(rbind, c3.scrape)

write.csv(c3.scrape.df, file = file.path(win.res, 'c3Results.csv'), row.names = F)

## Create prediction rasters

c3Pred.rasters <- lapply(c3.mod, FUN = raster::predict, object = env.stk)
pred.stk3 <- do.call(stack, c3Pred.rasters)
names(pred.stk3) <- c("frost",
                      "growing",
                      "freeze",
                      "frostFreeze",
                      "OG")
writeRaster(pred.stk3, filename = file.path(win.res, "c3Pred"),
            format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

#### making a bunch of pretty figures ####
major.table <- aictab(c(c1.mod, c2.mod, c3.mod), 
                      modnames = c("dem",
                                   "northing",
                                   "frost",
                                   "growing",
                                   "freeze",
                                   "frostFreeze",
                                   "OG",
                                   "north + dem",
                                   "north + frost",
                                   "north + growing",
                                   "north + freeze",
                                   "north + frostFreeze",
                                   "north + OG",
                                   "north + dem + frost",
                                   "north + dem + growing",
                                   "north + dem + freeze",
                                   "north + dem + frostFreeze",
                                   "north + dem + OG"))
write.csv(major.table, file =  file.path(win.res, 'allModelAICtableCenter.csv'), row.names = F)


