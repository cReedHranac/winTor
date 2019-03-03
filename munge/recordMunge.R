#### Record munge Re-work
library(tidyverse); library(skimr); library(rgdal); library(raster); library(lubridate)

#### Functions #####
geoManager <- function(x){
  ### Function to clean up, convert and standarize all georeferencing in the data
  
  ## Subsection off the geo components
  geo.dat <- x %>%
    dplyr::select(ID, siteName, zone, northing, easting, lat, long, elevation)
  
  ## Fix all the Long values
  ll.dat <- geo.dat %>%
    dplyr::filter(long != "")
  x$long[match(ll.dat$ID, x$ID)] <- -1 * as.numeric(substr(ll.dat$long, 3, nchar(ll.dat$long)))
  
  
  ## Work with UTM
  utm.dat <- geo.dat %>%
    dplyr::filter(zone != "")
  ## Fix character issues
  utm.dat$northing <- as.numeric(utm.dat$northing)
  utm.dat$easting <- as.numeric(utm.dat$easting)
  
  zones <- unique(utm.dat$zone)
  
  utm.list <- list()
  for( i in 1:length(zones)){
    #Select Subset
    utm.zone <- utm.dat %>%
      dplyr::filter(zone == zones[[i]])
    #define
    coordinates(utm.zone) <- ~ northing +  easting
    wgs84.utm <- paste0("+proj=utm +zone=",zones[[i]]," + datum=WGS84")
    proj4string(utm.zone) <- CRS(wgs84.utm)
    #re-project
    zoneLatLong <- spTransform(utm.zone,  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    #manage for write out
    zone.df <- as.data.frame(zoneLatLong)
    zone.df$long <- zone.df$northing
    zone.df$lat <- zone.df$easting
    
    utm.list[[i]] <- zone.df
  }
  utm.df <- do.call(rbind, utm.list)
  
  x$long[match(utm.df$ID, x$ID)] <- utm.df$long 
  x$lat[match(utm.df$ID, x$ID)] <- utm.df$lat 
  x$long <- as.numeric(x$long)
  return(x)
}

#### paths ####
if (!exists('base.path')) {
  if(.Platform$"OS.type" == "windows"){
    base.path = file.path("D:", "Dropbox", "wintor_aux")
  } else {
    base.path = "~/Dropbox/winTor_aux"
  }
}

win.dat <- file.path(base.path, "data")
win.res <- file.path(base.path, "Results")


#### Recorder data ####
  ##First set
# rec.dirty <- dplyr::tbl_df(read.csv("data/WinterDurationP1.csv", stringsAsFactors = F))
rec.dirty <- dplyr::tbl_df(read.csv("data/WinterDuration_Update.csv", stringsAsFactors = F,
                                    na.strings = c("Insufficient Number Recordings",
                                                   "ND",
                                                   "",
                                                   "-",
                                                   "No bats detected at this location")))
## add ID col
rec.dirty$ID <- paste0("ID_", 1:nrow(rec.dirty))
## clean up spatial issues 
rec.pts <- geoManager(x = rec.dirty)
## remove those without spatial locations
rec.dpts <- rec.pts %>%
  dplyr::filter(!is.na(long))
## clean the excess columns out 
rec.sub <- rec.dpts %>%
  dplyr::select(ID, siteName, yearMonitored, #site variables
                long, lat, elevation, #site location
                firstSpring, lastFall, #first and Last  
                LDH, LDE) #esitmates of Hibernation and Emergence
## set up dates
rec.sub$LDH <-  as.Date(rec.sub$LDH, "%d/%m/%Y") 
rec.sub$LDE <- as.Date(rec.sub$LDE, "%d/%m/%Y")
rec.sub$firstSpring <- as.Date(rec.sub$firstSpring, "%d/%m/%Y")
rec.sub$lastFall <- as.Date(rec.sub$lastFall, "%d/%m/%Y")
rec.sub$winter.duration <- as.numeric(rec.sub$LDE - rec.sub$LDH)
hist(rec.sub$winter.duration)

##Average across years and combine data where applicable.


## final clean
##remove those entries that don't have useable duration data
rec.clean <- rec.sub %>%
  dplyr::filter(!is.na(winter.duration)) %>%
  dplyr::select(ID, lat, long, LDH, LDE, winter.duration)
colnames(rec.clean)[4:5] <- c("Start", "End") # rename to conform
rec.clean$lat <- as.numeric(rec.clean$lat) ## shifting

#### literature data ####
lit.dat <- as_tibble(read.csv("data/durationData.csv"))
lit.dat$ID <- paste0("L", 1:nrow(lit.dat)) ## add ID
colnames(lit.dat)[5] <- "winter.duration"

## determine duration 
lit.dat$Start <- as.Date(lit.dat$Start, '%d-%h')
lit.dat$End <- as.Date(lit.dat$End, '%d-%h')
## add year for end
year(lit.dat$End) <- 2019
year(lit.dat$Start) <- 2018


for(i in 1:nrow(lit.dat)){ # Create duration since there's one already in there
  ifelse(is.na(lit.dat$winter.duration[[i]]), 
         lit.dat$winter.duration[[i]] <- as.numeric(lit.dat$End[[i]]- lit.dat$Start[[i]]),
         lit.dat$winter.duration[[i]] <- lit.dat$winter.duration[[i]])  
}
## final clean
lit.clean <- lit.dat %>%
  dplyr::select(ID, lat, long, Start, End, winter.duration)

#### Complete data frame ####
## Include the MTHP data stream    ## This data sucks
# mthp <- fread("data/MTHP_durationCleaned.csv")
# mthp$Start <- as.Date(mthp$Start, "%Y-%m-%d")
# mthp$End <- as.Date(mthp$End, "%Y-%m-%d")
# dat.comp <- bind_rows(lit.clean, rec.clean, mthp)

dat.comp <- bind_rows(lit.clean, rec.clean)
#create id col for binding with env 
dat.comp$env_ID <- 1:nrow(dat.comp)

#### spatial co-varriates ####

## add spatial orientation
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
coordinates(dat.comp) <- ~ long + lat
proj4string(dat.comp)  <- wgs84

## Co-variates
env.names <- c("NA_dem", "NA_northing", "NA_nFrostyDays",
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_OG1k")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)

## extract data from locations
env.dat <- raster::extract(env.stk, dat.comp, cellnumber = T, df = T)
colnames(env.dat)[1] <- "env_ID"
env.df <- as_tibble(left_join(as.data.frame(dat.comp), env.dat))

## write out complete dataframe to 
write.csv(env.df[,-which(names(env.df)=="env_ID")], file.path("data/", "durationUpdate.csv"), row.names = F)
