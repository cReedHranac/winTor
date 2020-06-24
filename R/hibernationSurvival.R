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
library(batwintor); library(data.table)
## Select species from the onboard dataset
mylu.params <- bat.params[bat.params$Species == "MYLU",]

## Changing the mass to the median of masses within our data set
mylu.params$Mass <- 7.73

## Select the fungal parameters that we want to use
fung.ch <- fung.params["Chaturvedi",]

## Set up the hibernation space to model winter across
env <- buildEnv(temp = c(-4,10), #temperatures in degrees C
                pct.rh = c(46, 100), #precent humidity
                range.res.temp = .5, #resolution of the temperature
                range.res.rh = 1, #resolution of the humidithy
                twinter = 12, #maximal length of winter (in this case I have months)
                winter.res = 1) #resolution of the time vector in vectors

## Hibernation energetics model
mylu.mod <- hibernationModel(env = env,
                             bat.params = mylu.params,
                             fung.params = fung.ch)

## write out
data.table::fwrite(x = mylu.mod,
                   file = file.path(win.dat, "myluDynamicModel.csv"))

## clean up as that's a pretty large calculation
env.hib <- ls()
to.remove <- env.post[env.hib %!in% env.prior]
rm(list=to.remove); rm(env.hib, to.remove); gc()

#### Fat required ####
## mylu hibernation energetics model
mylu.mod <- fread(file.path(win.dat, "myluDynamicModel.csv"))

## winter duration layer
library(raster)
rasterOptions(memfrac = .3); rasterOptions(maxmemory = 1e+08) ## helps
win <- raster(file.path(win.res, "durationRaster_p.tif"))

## create fixed microclimate conditions
rh.fix   <- calc(win, function(x) ifelse(!is.na(x), 98, NA))
temp.fix <- calc(win, function(x) ifelse(!is.na(x), 4, NA))

## Function to do the work
survivalFat <- function(mod.df, pct.rh.rast, temp.rast, win.rast){
  ### Function for converting the amount dynamic hibernation model results into 
  ### grams of fat and appling those againt winter duration
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
 
## Apply the function
fat.req <- survivalFat(mod.df = mylu.mod,
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

#### Best Temperature Avaliable ####
## one more version, this time where we use the best temperature avaliable 
## based on the work of McClure et al, 2020 (in submission, contact me for 
## access)