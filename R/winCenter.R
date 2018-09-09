#### Finding the center of witner? ####
  ## intro section lifted from record munge ###
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

## estimate center
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
