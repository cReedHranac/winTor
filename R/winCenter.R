

## Dirty data frame
dirt <- read.csv("data/WinterDurationP1.csv", stringsAsFactors = F)
dirt <- tbl_df(dirt)
## add ID collumn
dirt$id <- paste0("id_", 1:nrow(dirt))

## modify ND (No data) into NA
dirt.NA <- dirt %>%
  mutate_all(function(x)gsub(pattern = "Insufficient Number Recordings", replacement = NA, x)) %>%
  mutate_all(function(x)gsub(pattern = "ND", replacement = NA, x))


## process
dat.geo <- geoManager(x = dirt.NA)

## Which don't have any info on spatial
v<- dat.geo %>%
  dplyr::filter(is.na(long)) 

dat.pts <- dat.geo %>%
  filter(!is.na(long))

## Subset to the useful catagories for the analysis
dat.sub <- dat.pts %>%
  select(id, lat, long, nWinterFlights, LDH, lastFall,
         firstSpring, nMaxEmerging, nMaxSpring, LDE, elevation)

## set up dates
library(lubridate)
dat.sub$LDH <-  as.Date(dat.sub$LDH, "%d/%m/%Y") 
dat.sub$LDE <- as.Date(dat.sub$LDE, "%d/%m/%Y")
dat.sub$firstSpring <- as.Date(dat.sub$firstSpring, "%d/%m/%Y")
dat.sub$lastFall <- as.Date(dat.sub$lastFall, "%d/%m/%Y")

## duration of witner
dat.sub$winter.duration <- as.numeric(dat.sub$LDE - dat.sub$LDH)
hist(dat.sub$winter.duration)




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

lit.dat


## sub dataframes and combine
a.df <- lit.dat %>%
  select(id, Start, End, Duration) %>%
  filter(!is.na(Start))
## modify year
year(a.df$End) <- 2019


b.df <- dat.sub %>%
  select(id, LDH, LDE, winter.duration) %>%
  filter(!is.na(LDH) & !is.na(LDE)) %>%
  rename( Start = LDH, End = LDE, Duration = winter.duration)

## combine
df <- rbind(a.df, b.df)

## center of winter
df.est <- df %>%
  mutate(win.center = Start + floor(Duration/2)) %>%
  mutate(y = year(End), #create year col
         est.End = as.Date(paste0("15/04/",y), "%d/%m/%y"))
## fix for years...
year(df.est$est.End) <- df.est$y

#### estimate center m1 ####
df.est <- df.est %>%
  mutate( est.Center = est.End - floor(Duration/2),
          est.Dif = est.Center - win.center,
          fx.center = as.Date(paste0("15/01/",y), "%d/%m/%y"))
year(df.est$fx.center) <- df.est$y

df.est <- df.est %>%
  mutate(fx.Dif = fx.center - win.center)


hist(as.numeric(df.est$est.Dif))
hist(as.numeric(df.est$fx.Dif))

df.est$win.center

#### Estimate winter center through LM methods ####
## clean data to use 
m2.df <- df.est %>%
  select(id, Start, End, Duration, win.center) %>% #Select columns
  mutate(n.Start = yday(Start) - 182, ##create columns for numerical date of year
         n.End = yday(End) + 183, 
         n.center = abs(yday(win.center) - 182)) ## centered on the other side of the year
## check : duration should remain the same
all.equal(m2.df$n.End - m2.df$n.Start, m2.df$Duration) ## i appear to be off by one day...
## ammended n.End to 183 to fix & re-run
## still not perfect. there appear to be 10 insances of leap years involved... good enough?
## yes
## check to make sure that center is inbetween start and end
m2.df$n.Start < m2.df$n.center && m2.df$n.center < m2.df$n.End #TRUE

library(raster);library(rgdal)

## add spatial orientation
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
coordinates(m2.df) <- ~ long + lat
proj4string(m2.df)  <- wgs84

## Extract covariates and ammend to df
## Co-variates
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_frostFreeze", "NA_OG1k")
env.stk <- raster::stack(paste0(win.dat,"/",env.names,".tif"))

## extract data from locations
env.dat <- raster::extract(env.stk, m2.df, cellnumber = T, df = T)

env.df <- as_tibble(left_join(as.data.frame(m2.df), env.dat, by = "ID"))


## begin model
## attempt winter center

## apply rapid lm
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

rapid.lm <- function(x){
  ###function for lapplying lm
  f1.lm <- lm(formula = x$mod, data = x$dat)
  pdf(file = file.path(win.res,paste0(replace(x$mod[length(x$mod)], "+", "_"),".pdf")))
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
                              "frostFreeze",
                              "OG"))
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
                     "frostFreeze",
                     "OG")
writeRaster(pred.stk, filename = file.path(win.res, "f1Pred"),
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
                              "frostFreeze",
                              "OG"))
#write
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
                      "frostFreeze",
                      "OG")
writeRaster(pred.stk2, filename = file.path(win.res, "f2Pred"),
            format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

#### Level 3 models ####

## formulas
mod.formulas3 <- list(n.center ~ NA_northing + NA_nFrostyDays + NA_dem,
                      n.center ~ NA_northing + NA_nonGrowingDays + NA_dem,
                      n.center ~ NA_northing + NA_nDaysFreeze + NA_dem,
                      n.center ~ NA_northing + NA_frostFreeze + NA_dem,
                      n.center ~ NA_northing + NA_OG1k + NA_dem)

f3.list <- list()
for(i in 1:length(mod.formulas3)){
  f3.list[[i]] <- list(mod = mod.formulas3[[i]], dat = env.df)
}

## models
f3.mod <- lapply(f3.list, rapid.lm)

## Summaries
f3.sum <- lapply(f3.mod, summary.lm)

## AIC table
f3.res <- aictab(f3.mod, 
                 modnames = c("frost",
                              "growing",
                              "freeze",
                              "frostFreeze",
                              "OG"))
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
                      "frostFreeze",
                      "OG")
writeRaster(pred.stk2, filename = file.path(win.res, "f3Pred"),
            format = "GTiff", bylayer = T, suffix = "names", overwrite = T)

#### making a bunch of pretty figures ####
major.table <- aictab(c(f1.mod, f2.mod, f3.mod), 
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
write.csv(major.table, file =  file.path(win.res, 'allModelAICtable.csv'), row.names = F)