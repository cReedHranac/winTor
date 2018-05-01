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
dat.geo <- geoManager(x = dirt)

## Which don't have any info on spatial
v<- dat.geo %>%
  dplyr::filter(is.na(long)) 

dat.pts <- dat.geo %>%
  filter(!is.na(long))

## Subset to the useful catagories for the analysis
dat.sub <- dat.pts %>%
  select(id, lat, long, nWinterFlights, winterSpecies, LDH, lastFall,
         firstSpring, nMaxEmerging, nMaxSpring, LDE)

## set up dates
library(lubridate)
dat.sub$LDH



## freq table

table(!is.na(dat.sub$LDH) & dat.sub$LDH != "ND", !is.na(dat.sub$LDE) & dat.sub$LDE != "ND")

dat.NA <- dat.sub %>%
  mutate_all(function(x)gsub(pattern = "ND", replacement = NA, x))
skim(dat.NA)
coordinates(dat.pts) <- ~ long + lat
proj4string(dat.pts) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
mapview(dat.pts)
dirt$LDH

rm(list = ls())
skim(dirt$northing)
str