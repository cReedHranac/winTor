#################################################
####  Hibernation Energetics Modeling        ####
####  Author: C. Reed Hranac                 ####
####  Tested 19 June 2020                    ####
#################################################

## Within this script we will simulate hibernation of Myotis lucifugus across a
## variety of potential hibernacula conditions. We will then compare the amount 
## of fat required to survive hibernation of different lengths. 


env.prior <- ls()
#### Hibernation Energetics Modeling ####
library(batwintor); library(data.table); library(raster); library(tidyverse)
rasterOptions(memfrac = .3); rasterOptions(maxmemory = 1e+08)

## Select species from the onboard dataset
# mylu.params <- bat.params[bat.params$Species == "MYLU",]
# 
# ## Changing the mass to the median of masses within our data set
# mylu.params$Mass <- 7.73
# 
# ## Select the fungal parameters that we want to use
# fung.ch <- fung.params["Chaturvedi",]
# 
# ## Set up the hibernation space to model winter across
# env <- buildEnv(temp = c(-4,10), #temperatures in degrees C
#                 pct.rh = c(46, 100), #precent humidity
#                 range.res.temp = .5, #resolution of the temperature
#                 range.res.rh = 1, #resolution of the humidithy
#                 twinter = 12, #maximal length of winter (in this case I have months)
#                 winter.res = 1) #resolution of the time vector in vectors
# 
# ## Hibernation energetics model
# mylu.mod <- hibernationModel(env = env,
#                              bat.params = mylu.params,
#                              fung.params = fung.ch)
# 
# ## write out
# data.table::fwrite(x = mylu.mod,
#                    file = win.dat)

## clean up as that's a pretty large calculation
env.hib <- ls()
to.remove <- env.post[env.hib %!in% env.prior]
rm(list=to.remove); rm(env.hib, to.remove)

#### Fat required ####
## mylu hibernation energetics model
mylu.mod <- fread(file.path(win.dat, "myluDynamicModel.csv"))

## winter duration layer
win <- raster(file.path(win.res, "durationRaster_p.tif"))

## create fixed microclimate conditions
rh.fix   <- calc(win, function(x) ifelse(!is.na(x), 98, NA))
temp.fix <- calc(win, function(x) ifelse(!is.na(x), 4, NA))

## Function to do the work
survivalMetrics <- function(mod.df, pct.rh.rast, temp.rast, win.rast){
  ### Function for converting the amount dynamic hibernation model results into 
  ### grams of fat and applying those against winter duration
  ## mod.df <- dynamic hibernation model results
  ## pch.rh.rast <- relative humidity raster to apply spatially
  ## temp.rast <- temperature raster to apply spatially
  ## win.rast <- winter duration raster to apply spatially
  
  #Raster modifications for Kelvin temperatures
  if(summary(temp.rast)[1] > 200){
    temp.c <- temp.rast - 273
  } else{
    temp.c <- temp.rast
  }
  
  #Creating output raster dimensions
  out <- raster(pct.rh.rast); values(out) <- NA
  out.s <- stack(out,out,out,out); names(out.s) <- c("fat.inf", "fat.null",
                                                     "max.inf", "max.null")
  
  #Extract data from rasters  to matrix for speed
  pct.rh <- as.matrix(pct.rh.rast, nrow = nrow(pct.rh.rast), ncol = ncol(pct.rh.rast))
  temp <- as.matrix(temp.c, nrow = nrow(temp.c), ncol = ncol(temp.c))
  win <- as.matrix(win.rast, nrow = nrow(win.rast), ncol = ncol(win.rast))
  
  ## shifting hours to days
  mod.dif <- mod.df %>%
    mutate_(days = ~hour.to.day(time)) %>%
    group_by_(~Ta, ~pct.rh) %>%
    mutate_(max.inf = ~max(days*surv.inf),
            max.null = ~max(days*surv.null)) %>%
    ungroup
  
  ####Look Up Table ####
  #Vectors for look up table structure
  Ta_vals <- unique(mod.dif$Ta)
  pct.rh_vals <- unique(mod.dif$pct.rh)
  days_vals <- unique(mod.dif$days)
  
  #Look Up Table
  lut <- array(NA, dim=c(length(Ta_vals), length(pct.rh_vals), length(days_vals),  4)) # 2 for fat.inf and fat.null
  dimnames(lut)[[1]] <- Ta_vals                                                        # 2 more for the max time
  dimnames(lut)[[2]] <- pct.rh_vals
  dimnames(lut)[[3]] <- days_vals
  dimnames(lut)[[4]] <- c("fat.inf", "fat.null", "max.inf", "max.null")
  
  
  #Fill look up table
  for (i in seq_len(nrow(mod.dif))) {
    d <- mod.dif[i,]
    if (i %% 10000 == 0) {
      cat("Look up table generation up to", i, "of", nrow(mod.dif), "\n")
    }
    lut[as.character(d$Ta), as.character(d$pct.rh), as.character(d$days),] <- c(d$g.fat.consumed,
                                                                                d$n.g.fat.consumed,
                                                                                d$max.inf,
                                                                                d$max.null)
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
      if(i %% 100000 == 0){
        cat("Raster layer: ", j, "of", nlayers(out.s),
            " -- ", (i/length(pct.rh))*100, "% complete\n")
      }
      pct.rh_i <- find_closest(pct.rh[[i]], pct.rh_vals)
      Ta_i  <- find_closest(temp[[i]], Ta_vals)
      win_i <- find_closest(win[[i]], days_vals)
      out.z[[i]] <- lut[Ta_i, pct.rh_i, win_i, j]
    }
    # Set values back from matrix to raster
    out.s[[j]] <- setValues(out.s[[j]], out.z)
  }
  
  # ammend the max survived layers
  out.s[[3]] <- out.s[[3]] - win.rast;names(out.s)[[3]] <- "survDays.inf"
  out.s[[4]] <- out.s[[4]] - win.rast;names(out.s)[[4]] <- "survDays.null"
  return(out.s)
}
 
## Apply the function
fat.req <- survivalMetrics(mod.df = mylu.mod,
                           pct.rh.rast = rh.fix,
                           temp.rast = temp.fix,
                           win.rast = win)

## write out
writeRaster(fat.req,
            filename = file.path(win.res, "myluFatReq.tif"),
            format = "GTiff",
            bylayer = T,
            suffix = "names",
            overwrite = T)
gc()
#### Best Temperature Available ####
## one more version, this time where we use the best temperature available 
## based on the work of McClure et al, 2020 (in submission, contact me for 
## access)

best.temp <- raster(file.path(win.dat, "Mylu_bestavailTF_NA.tif"))
best.fat.req <- survivalMetrics(mod.df = mylu.mod,
                                pct.rh.rast = rh.fix,
                                temp.rast = best.temp,
                                win.rast = win)

## write out
writeRaster(best.fat.req,
            filename = file.path(win.res, "myluFatReq_Best.tif"),
            format = "GTiff",
            bylayer = T,
            suffix = "names",
            overwrite = T)

#### Clean up script items ####
env.post <- ls()
to.remove <- env.post[env.post %!in% env.prior]
rm(list=to.remove); rm(env.post, to.remove)