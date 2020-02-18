#######################################
### GMB modling script for winTor###
#######################################

### Re-work of the linear modeling scripts to use gbm methodology

## Script is currently in the sandbox stage working on different methodology 
## conserving the first portion of the LM crossvalidation to remove outliers

#### Extra Paths####
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

##packages
library(tidyverse);library(gbm); library(raster)
#### Functions ####
mod.form <- function(x, coVar){
  ##Function to write out the formulas for me
  ## x is to be the item predicted
  ## covar is to be a list of the names oc covariates
  
  #univariate models
  l1 <- list()
  for(i in 1:length(coVar)){
    l1[[i]] <- as.formula(paste(x,"~",coVar[[i]]))
  }
  
  #with Norhting
  coVar2 <- coVar[-2]
  l2 <- list()
  for(i in 1:length(coVar2)){
    l2[[i]] <- as.formula(paste(x,"~",coVar[[2]],"+",coVar2[[i]]))
  }
  
  #with Northing and DEM
  coVar3 <- coVar[3:length(coVar)]
  l3 <- list()
  for(i in 1:length(coVar3)){
    l3[[i]] <- as.formula(paste(x,"~",coVar[[2]],"+",coVar[[1]],"+",coVar3[[i]]))
  }
  
  list.mods <- c(l1, l2, l3)
  
  return(list.mods)
}

#### Data ####
dur <- read.csv("data/durationDataReferenced.csv")
mass <- read.csv("data/massDataReferenced.csv")

## Co-variates
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_OG1k")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)

## ammending to have co-variate data
coordinates(mass) <- ~ Long + Lat
proj4string(mass) <- proj4string(env.stk)
mass.df <- as.data.frame(cbind(mass, raster::extract(env.stk, mass)))

coordinates(dur) <- ~ Long + Lat
proj4string(dur) <- proj4string(env.stk)
dur.df <- as.data.frame(cbind(dur, raster::extract(env.stk, dur)))

##Duration

mass.df <- mass.df[-which(mass.df$avgMass==14.5),]

#### Linear model application ####

dur.mods <- lapply(mod.form("winter.duration",coVar = env.names),
                   FUN = lm, data = dur.df )

mass.mods <- lapply(mod.form("avgMass",coVar = env.names),
                    FUN = lm, data = mass.df )

#### gbm ####

## Critical for model to operate apparently
dur.sub <- dur.df %>%
  dplyr::select(winter.duration,NA_dem, NA_northing, NA_nFrostyDays)


## set seed
set.seed(9)
## tain model
dur.fit <- gbm(
  formula = winter.duration ~ .,
  distribution = "gaussian",
  data = dur.sub,
  n.trees = 5000,
  interaction.depth = 3, 
  bag.fraction = 2,
  shrinkage = 0.05,
  cv.folds = 10
)

## print and examine
print(dur.fit)
min_MSE <- which.min(dur.fit$cv.error)
sqrt(dur.fit$cv.error[min_MSE])
gbm.perf(dur.fit, method = "cv")

### working on creating a function/loop for doing model selection across all

hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

random_index <- sample(1:nrow(dur.sub), nrow(dur.sub))
random_dur_train <- dur.sub[random_index, ]

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(9)
  
  # train model
  gbm.tune <- gbm(
    formula = winter.duration ~ .,
    distribution = "gaussian",
    data = dur.sub,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .9,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)




#### dismo ####
library(dismo)
dur.dis <- gbm.step(data = dur.df,
                    gbm.x = 6:8,
                    gbm.y = 4,
                    family = "gaussian",
                    tree.complexity = 2,
                    learning.rate = 0.1,
                    bag.fraction = .75,
                    n.trees = 10000)
### this doesn't appear to work at all...