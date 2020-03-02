### Env for testing
library(tidyverse); library(data.table);library(raster);library(batwintor)

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

# win.rast <- raster(file.path(win.res, "durationRaster_p.tif"))
# ## hibernation model
# mod.df <- data.table::fread(file = "D://Dropbox/winTor_aux/data/myluDynamicModel.csv" )
# 
# temp.rast <- calc(win.rast, function(x) ifelse(!is.na(x), 4, NA))
# pct.rh.rast <- calc(win.rast, function(x) ifelse(!is.na(x), 98, NA))






win <- raster(file.path(win.res, "durationRaster_p.tif"))
## hibernation model
mod <- data.table::fread(file = "D://Dropbox/winTor_aux/data/myluDynamicModel.csv" )

temp.r <- calc(win, function(x) ifelse(!is.na(x), 4, NA))
pct.r <- calc(win, function(x) ifelse(!is.na(x), 98, NA))


test1 <- survivalMetrics(mod.df = mod,
                         pct.rh.rast = pct.r,
                         temp.rast = temp.r,
                         win.rast = win)










fat.aval <- raster(file.path(win.res, "myluCropped__fat.tif"))

survivalDays <- function(mod.df, fat.aval){
  ## function to create the max days survived with based on the fat avaliable
  
  ## creating raster to dump the information out to
  out <- raster(fat.aval); values(out) <- NA
  out.s <- stack(out, out); names(out.s) <- ("days.inf", "days.null")
  
  #Extract data from rasters  to matrix for speed
  fat <- as.matrix(fat.aval,
                    nrow = nrow(fat.aval),
                    ncol = ncol(fat.aval))
  
  ## shifting hours to days
  mod.dif <- mod.df %>%
    mutate_(days = ~hour.to.day(time),
            max.null = ~max(days*surv.null),
            max.inf = ~max(days*surv.inf))
  
  ####Look Up Table ####
  #Vectors for look up table structure
  mNul_vals <- unique(mod.dif$max.null)
  mInf_vals <- unique(mod.dif$max.inf)
  days_vals <- unique(mod.dif$days)
  
  # look up table
  
  
  #### find closest ####
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
  
  
  for(i in 1:nlayers(out.s)){
    #Create output matrix
    out.z <- matrix(ncol = ncol(fat.aval), nrow = nrow(fat.aval))
    for(j in 1:length(fat)){
      if(i %% 1000 == 0){
        cat("Raster layer: ", i, "of", nlayers(out.s), "up to", j, "of", length(pct.rh), "\n")
      }
      
    }
  }
}
