#### Recorder Munge ####
library(tidyverse);library(skimr); library(rgdal)

## Dirty data frame
dirt <- read.csv("data/WinterDurationP1.csv", stringsAsFactors = F)
dirt <- tbl_df(dirt)
## add ID collumn
dirt$id <- paste0("id_", 1:nrow(dirt))

## modify ND (No data) into NA
dirt.NA <- dirt %>%
  mutate_all(function(x)gsub(pattern = "Insufficient Number Recordings", replacement = NA, x)) %>%
  mutate_all(function(x)gsub(pattern = "ND", replacement = NA, x))

## Split off the location info and work on it

geoManager <- function(x){
  ### Function to clean up, convert and standarize all georeferencing in the data
  
  ## Subsection off the geo components
  geo.dat <- x %>%
    select(id, siteName, zone, northing, easting, lat, long, elevation)
  
  ## Fix all the Long values
  ll.dat <- geo.dat %>%
    dplyr::filter(long != "")
  x$long[match(ll.dat$id, x$id)] <- -1 * as.numeric(substr(ll.dat$long, 3, nchar(ll.dat$long)))
  
  
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
  
  x$long[match(utm.df$id, x$id)] <- utm.df$long 
  x$lat[match(utm.df$id, x$id)] <- utm.df$lat 
  x$long <- as.numeric(x$long)
  return(x)
}

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

#   ## 2 entries are negative when they should be postitive. 
# dat.sub[which(dat.sub$winter.duration < 0),] # 29 & 76 ## Changed in source file

#   ## Make sure years are around the correct way
# dat.sub[which(year(dat.sub$LDE) - year(dat.sub$LDH) < 0),]


## differences between the firt of spring and LDE
dat.sub$spring.diff <- as.numeric(dat.sub$firstSpring - dat.sub$LDE)
hist(dat.sub$spring.diff)

#   ## Adressing the clear outliers
# dat.sub[which(abs(dat.sub$spring.diff)>100), ] #144 ## Corrected in source

## difference betweeen last of falll and LDH
dat.sub$fall.diff <- as.numeric(dat.sub$LDH - dat.sub$lastFall)
hist(dat.sub$fall.diff)

#   ##addressing outlyiers again
# dat.sub[which(abs(dat.sub$fall.diff)>100),] ## 29 and 76 ## corrected in source

## writeout
write.csv(dat.sub, "data/winDurationClean.csv", row.names = F)



#### Combine with literature data ####
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
               "NA_nonGrowingDays", "NA_nDaysFreeze", "NA_frostFreeze", "NA_OG1k")
env.stk <- raster::subset(stack(list.files(win.dat, pattern = "NA_*", full.names = T)), env.names)

## extract data from locations
env.dat <- raster::extract(env.stk, dat, cellnumber = T, df = T)

env.df <- as_tibble(left_join(as.data.frame(dat), env.dat, by = "ID"))

## write out complete dataframe to 
write.csv(env.df, file.path("data/", "modelingDataFrame.csv"), row.names = F)
