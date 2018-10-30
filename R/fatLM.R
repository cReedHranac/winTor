#### Fat over space 
## predict the mass of Myotis lucifigus acrross the distribution
## largely coppied from the wintorLM analysis

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

#### Functions ####
'%!in%' <- function(x,y)!('%in%'(x,y))

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
    dplyr::filter(rn %!in% names(which(cooks.distance(f2.lm) > 4/nrow(lm.df2)))) %>%
    column_to_rownames("rn")
  
  ##third?
  f3.lm <- lm(formula = x$mod, data = lm.df3)
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

#### data ####
dat.fat <- read.csv("data/massLocations.csv")
dat.fat$ID <- paste0('i',1:nrow(dat.fat))#needed do not remove

## Co-variates 
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_OG1k")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)

## add cov extract to dataset
coordinates(dat.fat) <- ~ Long + Lat
proj4string(dat.fat) <- proj4string(dat.fat)
env.df <- as.data.frame(cbind(dat.fat, raster::extract(env.stk, dat.fat)))



#### L1 Modeling ####
## formulas
m1.forms <- list(avgMass ~ NA_dem,
                 avgMass ~ NA_northing,
                 avgMass ~ NA_nFrostyDays,
                 avgMass ~ NA_nonGrowingDays,
                 avgMass ~ NA_nDaysFreeze,
                 avgMass ~ NA_OG1k)

## args list
fat1.list <- list()
for(i in 1:length(m1.forms)){
  fat1.list[[i]] <- list(mod = m1.forms[[i]], dat = env.df)
}

## model
fat1 <- lapply(fat1.list, rapid.lm.clean, name = "fat")
#split into dfs and models
fat1.mod <- list();for(i in 1:length(fat1)){fat1.mod[[i]] <- fat1[[i]]$lm}
fat1.df <- list();for(i in 1:length(fat1)){fat1.df[[i]] <- fat1[[i]]$dp.rm}

## summaries
fat1.sum <- lapply(fat1.mod, summary.lm)

## AIC tab
library(AICcmodavg)
fat1.res <- aictab(fat1.mod, 
                 modnames = c("dem",
                              "northing",
                              "frost",
                              "growing",
                              "freeze",
                              "OG"))
##write
write.csv(fat1.res,file =  file.path(win.res, 'fat1AICtable.csv'), row.names = F)

## results table
fat1.scrape <- lapply(fat1.mod, scrapeResults)
fat1.scrape.df <- do.call(rbind, fat1.scrape)

write.csv(fat1.scrape.df, file = file.path(win.res, 'fat1Results.csv'), row.names = F)

##Create prediction rasters

fat1Pred.rasters <- lapply(fat1.mod, FUN = raster::predict, object = env.stk)
pred.stk <- do.call(stack, fat1Pred.rasters)
names(pred.stk) <- c("dem",
                     "northing",
                     "frost",
                     "growing",
                     "freeze",
                     "OG")
writeRaster(pred.stk, filename = file.path(win.res, "fat1Pred"),
            format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

##clean for memory 
rm(pred.stk, fat1Pred.rasters)

#### Level 2 models ####
## formulas
mod.formulas2 <- list(avgMass ~ NA_northing + NA_dem,
                      avgMass ~ NA_northing + NA_nFrostyDays,
                      avgMass ~ NA_northing + NA_nonGrowingDays,
                      avgMass ~ NA_northing + NA_nDaysFreeze,
                      avgMass ~ NA_northing + NA_OG1k)

## args list
fat2.list <- list()
for(i in 1:length(mod.formulas2)){
  fat2.list[[i]] <- list(mod = mod.formulas2[[i]], dat = env.df)
}

## models
fat2 <- lapply(fat2.list, rapid.lm.clean, name = "fat")
#split into dfs and models
fat2.mod <- list();for(i in 1:length(fat2)){fat2.mod[[i]] <- fat2[[i]]$lm}
fat2.df <- list();for(i in 1:length(fat2)){fat2.df[[i]] <- fat2[[i]]$dp.rm}

## summaries
fat2.sum <- lapply(fat2.mod, summary.lm)

## AIC table
fat2.res <- aictab(fat2.mod, 
                 modnames = c("dem",
                              "frost",
                              "growing",
                              "freeze",
                              "OG"))
#write
write.csv(fat2.res,file =  file.path(win.res, 'fat2AICtable.csv'), row.names = F)

## results table
fat2.scrape <- lapply(fat2.mod, scrapeResults)
fat2.scrape.df <- do.call(rbind, fat2.scrape)

write.csv(fat2.scrape.df, file = file.path(win.res, 'fat2Results.csv'), row.names = F)

## Create prediction rasters
fat2Pred.rasters <- lapply(fat2.mod, FUN = raster::predict, object = env.stk)
pred.stk2 <- do.call(stack, fat2Pred.rasters)
names(pred.stk2) <- c("dem",
                      "frost",
                      "growing",
                      "freeze",
                      "OG")
writeRaster(pred.stk2, filename = file.path(win.res, "fat2Pred"),
            format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

## clean for memory
rm(fat2Pred.rasters, pred.stk2)

#### Level 3 models ####

## formulas
mod.formulas3 <- list(avgMass ~ NA_northing + NA_nFrostyDays + NA_dem,
                      avgMass ~ NA_northing + NA_nonGrowingDays + NA_dem,
                      avgMass ~ NA_northing + NA_nDaysFreeze + NA_dem,
                      avgMass ~ NA_northing + NA_OG1k + NA_dem)

fat3.list <- list()
for(i in 1:length(mod.formulas3)){
  fat3.list[[i]] <- list(mod = mod.formulas3[[i]], dat = env.df)
}

## models
fat3 <- lapply(fat3.list, rapid.lm.clean, name = fat)
#split into dfs and models
fat3.mod <- list();for(i in 1:length(fat3)){fat3.mod[[i]] <- fat3[[i]]$lm}
fat3.df <- list();for(i in 1:length(fat3)){fat3.df[[i]] <- fat3[[i]]$dp.rm}

## Summaries
fat3.sum <- lapply(fat3.mod, summary.lm)


## AIC table
fat3.res <- aictab(fat3.mod, 
                 modnames = c("frost",
                              "growing",
                              "freeze",
                              "OG"))
##write
write.csv(fat3.res,file =  file.path(win.res, 'fat3AICtable.csv'), row.names = F)

## results table
fat3.scrape <- lapply(fat3.mod, scrapeResults)
fat3.scrape.df <- do.call(rbind, fat3.scrape)

write.csv(fat3.scrape.df, file = file.path(win.res, 'fat3Results.csv'), row.names = F)

## Create prediction rasters

fat3Pred.rasters <- lapply(fat3.mod, FUN = raster::predict, object = env.stk)
pred.stk3 <- do.call(stack, fat3Pred.rasters)
names(pred.stk3) <- c("frost",
                      "growing",
                      "freeze",
                      "OG")
writeRaster(pred.stk3, filename = file.path(win.res, "fat3Pred"),
            format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

## Clean for memory
rm(fat3Pred.rasters,pred.stk3)

#### Summary AIC Table ####
major.table <- aictab(c(fat1.mod, fat2.mod, fat3.mod), 
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
write.csv(major.table, file =  file.path(win.res, 'allFatModelAICtable.csv'), row.names = F)


# # #### look at residuals for top layer ####
# best <- fat3.list[[3]]
# mod <- rapid.lm.clean(best) ## 6 points are romoved. but whitch 6?
# 
# mod.df <- env.df %>%
#   left_join( cbind(resid = mod$residuals, mod$model), "NA_nDaysFreeze") %>%
#   dplyr::filter(!is.na(avgMass.y)) %>%
#   select(Long, Lat, resid)
# 
# library(mapview)
# coordinates(mod.df) <- ~ Long + Lat
# proj4string(mod.df) <- proj4string(env.stk)
# mapview(mod.df)
