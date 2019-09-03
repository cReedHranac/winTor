#### VertNet Data munge ####
library(tidyverse)
library(data.table)

## data 
vert.raw <- fread("data/myluVertNet-b2cd2747965c480fb149df899be14fba.txt",
                 stringsAsFactors = F, 
                 header = T)
str(vert.raw)

## Found error
vert.raw$massing[which(vert.raw$massing == 73)] <- 7.3

# ## cleaning
# skim(vert.raw)
#   
#   #sex
# unique(vert.raw$sex)
# length(which(vert.raw$sex %in% unique(vert.raw$sex)[3:5])) ## 45 records
# 
#   #life stage and repo status
# unique(vert.raw$lifestage)
# unique(vert.raw$reproductivecondition)
#   #both appear to be pretty useless with all the levels, and missing data...
#   #i guess it shouldn't matter going into winter?
# 
#   #month
# hist(vert.raw$month) ## going to be low....

## subsetting
vert.sub <- vert.raw %>%
  dplyr::select( occurrenceid, sex, lifestage, reproductivecondition, month, massing,
          decimallatitude, decimallongitude, geodeticdatum, coordinateuncertaintyinmeters) %>%
  filter(sex %in% unique(sex)[1:2],
         month %in% 9:11,
         massing > 5, 
         geodeticdatum %in% c("NAD27",
                              "North American Datum 1927",
                              "WGS 1984",
                              "WGS84",
                              "World Geodetic System 1972",
                              "World Geodetic System 1984",
                              "not recorded (forced WGS84)",
                              "North American Datum 1983"))

## spatial issues
library(rgdal, sp)
#loading spatial libraries

spConformer <- function(dataframe, outputPRJ){
  ## Function for transforming multipul projections into a single projection based on a dataframe
  ## this is by no means optimal but it should continue to get the work done?
  
  ## list of projections
  prjList <- c(
    WGS84 = "+proj=longlat +ellps=WGS84 +datum=WGS84",
    WGS83 = "+proj=longlat +ellps=WGS83 +datum=WGS83",
    NAD27 = "+init=epsg:4267 +proj=longlat +ellps=clrk66 +datum=NAD27")
  
  ## create index to iterate cross
  prjIndex <- as.numeric(substring(names(prjList), 4,5))
  
  subSpliter <- function( Index, df, prj4List){
    prjQuo <- enquo(Index)
    
    #split
    split <- df %>%
      filter(grepl(!!prjQuo, geodeticdatum))
    #define spatially
    if(nrow(split) >0){
      coordinates(split) <- ~ decimallongitude+decimallatitude
      proj4string(split) <- prj4List[which(grepl(Index, prj4List))]  
      
      return(split)
    }
  }
  
  #create list of spDataFrames
  splitList <- lapply(prjIndex,
                      FUN = subSpliter,
                      df = dataframe, prj4List = prjList)
  
  #Transform
  j <- 1
  splitTrans <- list()
  for( i in 1:length(splitList)){
    if(is.null(splitList[[i]])){
      next
    }
    if(!is.null(splitList[[i]]) & proj4string(splitList[[i]]) != prjList[which(grepl(outputPRJ, prjList))]){
      
      splitTrans[[j]] <- spTransform(splitList[[i]], prjList[which(grepl(outputPRJ, prjList))])
      j <- j+1
    } 
    if(proj4string(splitList[[i]]) == prjList[which(grepl(outputPRJ, prjList))]){
      
      splitTrans[[j]] <- splitList[[i]]
      j <- j+1
    } 
  }
  
  out <- do.call(rbind, splitTrans)
  
  return(out)
}

vert.sp <- spConformer(vert.sub, "WGS84")
### There's a good deal of these are at the same locations need to group by location and summarize
remove.duplicates(vert.sp)

vert.df <- as.data.frame(vert.sp)

vert.loc <- vert.df %>%
  group_by(decimallatitude, decimallongitude) %>%
  dplyr::select(decimallongitude, decimallatitude, massing) %>%
  summarise(avgMass = mean(massing)) 

vert.df <- as.data.frame(vert.loc)
colnames(vert.df)[1:2] <- c("Lat", "Long")
vert.df$Ref <- "VertNet"

coordinates(vert.loc) <- ~ decimallongitude + decimallatitude
proj4string(vert.loc) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# ## looking for mass issues
# vert.dirty <- as.data.frame(vert.sp)
# hist(vert.dirty$massing) ## one is 73, which I'm assuming they mean 7.3
# vert.dirty[which.max(vert.dirty$massing), "massing"] <- 7.3
# 
# ##anything below 5g is really small... take alook at the notes and see if they should be excluded
# mass.probs <- vert.dirty[which(vert.dirty$massing<5),'occurrenceid']
# 
# lookAtThis <- vert.raw[which(vert.raw$occurrenceid %in% mass.probs),]
# View(lookAtThis)
# 
# ## one was desicated, the others are potentially just the weight of the skelliton or samples?
# ## removing all samples with massing less then 5g

library(mapview)
mapview(vert.loc)


#### Canada data munge ####

can.raw <- fread(file.path(win.dat,"junk from repo", "WoodbufflowNWT.csv"))

can.sub <- can.raw %>%
  dplyr::select(Bat_ID, Species, Capture_date, Location, Sex, weight) %>%
  filter( Species == "MYLU",
          Sex %in% c("M", "F"),
          !is.na(weight),
          !grepl("\\*", weight))

can.sub$weight <- as.numeric(can.sub$weight)

can.loc <- can.sub %>%
  group_by(Location) %>%
  dplyr::select(Location, weight) %>%
  summarise(avgMass = mean(weight))


## scanning and creating filter statments for sub
# skim(can.sub)
# unique(can.sub$Species)## only want mylu
# 
# unique(can.sub$Sex) ## need batts that have M or F
# 
# unique(can.sub$weight)
# 
# unique(can.sub$Capture_date)
# !grepl("\\*", can.sub$weight)

## combining with the spatial locations

sitesList <- fread(file.path(win.dat,"junk from repo","WoodbufflowNWT_Locations.csv"))
match(can.loc$Location, sitesList$Survey_Site_Name)

## bind with sub
can.bind <- cbind(can.loc,
                  xcoord = sitesList$X_Coordinate[match(can.loc$Location, sitesList$Survey_Site_Name)],
                  ycoord = sitesList$Y_Coordinate[match(can.loc$Location, sitesList$Survey_Site_Name)])

coordinates(can.bind) <- ~ xcoord +  ycoord
wgs83.utm <- "+proj=utm +zone=12V + datum=WGS84"
proj4string(can.bind) <- CRS(wgs83.utm)
#re-project
can.sp <- spTransform(can.bind,  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

can.df <- as.data.frame(can.sp)
colnames(can.df)[3:4] <- c("Long", "Lat")
can.df$Ref <- "WCS Canada"


#### SERDP Data munge ####

serdp.raw <- fread(file.path(win.dat,"junk from repo","SERDPmorphometricJuly2018.csv"))

serdp.sub <- serdp.raw %>%
  dplyr::select(`Bat ID`, `Bat Species`, `Site Name`, `Date Captured`, Latitude, Longitude, Sex, `Mass (prior resp)`) %>%
  filter(`Bat Species` == "Myotis lucifugus") %>%
  separate(`Date Captured`,  c("d","m","y")) %>%
  filter(m %in% c("09","10","11")) %>%
  group_by(`Site Name`)
  
## redo colnames
colnames(serdp.sub) <- c("Bat_ID", "Species", "Location", "Day", "Month",
                         "Year", "Lat", "Long", "Sex", "Mass")
## scanning to create filter statments
# skim(serdp.raw)
# unique(serdp.raw$`Bat Species`)
# unique(serdp.raw$`Date Captured`)

serdp.loc <- serdp.sub %>%
  group_by(Location) %>%
  dplyr::select(Location, Lat, Long, Mass) %>%
  summarise(avgMass = mean(Mass),
            Lat = unique(Lat),
            Long = unique(Long))

coordinates(serdp.loc) <- ~ Long + Lat
proj4string(serdp.loc) <-  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
mapview(serdp.loc)

serdp.df <- as.data.frame(serdp.loc)
serdp.df$Ref <- "SERDP"

#### Litature data ###
lit.dat <- fread('data/litData.csv')
#rename for merge
lit.df <- lit.dat %>%
  group_by(Location, Reference) %>%
  dplyr::select(Location, Lat, Long, Mass) %>%
  summarise(avgMass = mean(Mass), 
            Lat = unique(Lat),
            Long = unique(Long),
            Ref = unique(Reference))



#### Yukon Data (added 05/11/2018)####
Yk.dat <- fread(file.path(win.dat,"junk from repo", '/YukonClean.csv'))

yk.df <- Yk.dat %>%
  group_by(Location) %>%
  dplyr::select(lat, long, Mass) %>%
  summarise(avgMass = mean(Mass), 
            Lat = unique(lat),
            Long = unique(long))
yk.df$Ref <- "WCS Canada Yukon"

#### Bind and add grams fat ####
full <- rbind(vert.df, can.df[,-1], serdp.df[,-1], lit.df[,-1], yk.df[,-1])

zq1 <- bind_rows(vert.df, can.df, serdp.df, lit.df, yk.df)
write.csv(zq1, file = file.path(win.res, "massDataReferenced.csv"), row.names = F)

# equation lifted from the qmrAnalysis script
full$g.fat <- -2.8400 + 0.5932*full$avgMass


## Summary for lean and fat mass


## write out
write.csv(x = full, file = "data/massLocations.csv", row.names = F)

coordinates(full) <- ~ Long + Lat
proj4string(full) <-  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

mapview(full)
