fastFatRast <- function(mod.df, pct.rh.rast, temp.rast, win.rast){
  ## Attempt to create a faster fatRaster function
  #Raster modifications for Kelvin temperatures
  if(summary(temp.rast)[1] > 200){
    temp.c <- temp.rast - 273
  } else{
    temp.c <- temp.rast
  }
  
  ## Creating output raster dimensions
  out <- raster(pct.rh.rast); values(out) <- NA
  out.s <- stack(out,out); names(out.s) <- c("fat.inf", "fat.null")
  
  ## Extract data from rasters  to matrix for speed
  pct.rh <- as.matrix(pct.rh.rast, nrow = nrow(pct.rh.rast), ncol = ncol(pct.rh.rast))
  temp <- as.matrix(temp.c, nrow = nrow(temp.c), ncol = ncol(temp.c))
  win <- as.matrix(win.rast, nrow = nrow(win.rast), ncol = ncol(win.rast))
  
  
  ## shifting hours to days
  mod.dif <- mod.df %>%
    mutate_(days = ~hour.to.day(time)) 
  ## raster vector values
  Ta_vals <- unique(mod.dif$Ta)
  pct.rh_vals <- unique(mod.dif$pct.rh)
  days_vals <- unique(mod.dif$days)
  
  
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
  
  ## subset the model vectors to only values that appear in the raster 
  close.temps <- Ta_vals[unique(sapply(unique(values(temp.c)), FUN = find_closest, y= Ta_vals))]
  close.rh <- pct.rh_vals[unique(sapply(unique(values(pct.rh.rast)), FUN = find_closest, y= pct.rh_vals))]
  close.win <- days_vals[unique(sapply(unique(values(win.rast)), FUN = find_closest, y= days_vals))]
  
  mod.trunc <- mod.dif %>% ## it's big but still smaller
    dplyr::filter(Ta %in% close.temps,
                  pct.rh %in% close.rh,
                  days %in% close.win)
  
  ## Look up table
  lut <- array(NA, dim=c(length(close.temps),
                         length(close.rh),
                         length(close.win),  2)) # 2 for fat.inf and fat.null
  dimnames(lut)[[1]] <- close.temps
  dimnames(lut)[[2]] <- close.rh
  dimnames(lut)[[3]] <- close.win
  dimnames(lut)[[4]] <- c("fat.inf", "fat.null")
  
  ## Fill lut
  for (i in seq_len(nrow(mod.trunc))) {
    d <- mod.trunc[i,]
    if (i %% 10000 == 0) {
      cat("Look up table generation up to", i, "of", nrow(mod.trunc), "\n")
    }
    lut[as.character(d$Ta), as.character(d$pct.rh), as.character(d$days),] <- c(d$g.fat.consumed,
                                                                                d$n.g.fat.consumed)
  }
  
  #Run lookup   NOTE: will not work will rarified table
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
mod.df <- mod.big 
pct.rh.rast <- rh.fix
temp.rast <- mat.fix
win.rast <- win