#### Plotting for WinTor ####

#### Extra Paths ####
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


#### Creating North America Geo-political boundries for backround. ####

##creating North America political boundries 
# can.ext <- c( -140,-104,41, 60)
# canada <- getData("GADM",country="CAN",level=1)
# usa <- getData("GADM",country="USA", level=1)
# mexico <- getData("GADM",country="MEX", level=1)
# North.America <- rbind(canada,usa,mexico)
# plot(North.America)
# writeOGR(North.America,
#          dsn = win.dat, 
#          layer = "NorthAmerica",
#          driver = "ESRI Shapefile")

library(sf);library(rgdal);library(raster)
North.America <- st_read(win.dat, layer="NorthAmerica")
NA.utm <- st_transform(North.America, 2955)

##mylu distribution
mylu.dist <- st_read("D:/Dropbox/batwintor_aux/paramFiles/ShapeFiles", 
                     layer = "myotis_lucifugus")

st_crs(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mylu.utm <- st_transform(mylu.dist, 2955)

#### create canadian polygon ####
# can.df <- data.frame(
#   rbind(
#     c("NW",  "9V", 586518, 7077103),
#     c("NE", "13W", 645544, 7118728),
#     c("SW", "11T", 680262, 4865141),
#     c("SE", "14T", 314095, 497555)),
#   stringsAsFactors = F
# )
# colnames(can.df) <- c("Corner", "Zone", "Northing", "Easting")
# ## make xy numeric
# num.cols <- c("Northing", "Easting")
# can.df[num.cols] <- sapply(can.df[num.cols], as.numeric)
# can.df["Zone"] <- as.character(can.df["Zone"])
# test <- st_as_sf(can.df,
#                    coords = c("Easting", "Northing", "Zone"),
#                    epsg = 2955)


##Try the same thing with lat long
can.ll <- data.frame(
  rbind(
    c("NW", 63.8106, -127.2429),
    c("NE", 64.1641, -102.0060),
    c("SW", 43.9173 , -114.7548),
    c("SE", 44.2975 , -101.3304)),
  stringsAsFactors = F
)
colnames(can.ll) <- c("Corner", "Lat", "Long")
## make xy numeric
num.cols <- c("Lat", "Long")
can.ll[num.cols] <- sapply(can.ll[num.cols], as.numeric)
can.sf <- st_as_sf(can.ll,
                 coords = c("Long","Lat"))
st_crs(can.sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
can.utm <- st_transform(can.sf, 2955)
##remove old objects
rm(North.America, mylu.dist,can.ll, num.cols, can.sf)
#### Create cropped estimates for mapping ####
# raw.layers <- stack(list.files(win.res, pattern = "_p", full.names = T))
# ## Create the fat layer
# raw.layers$fat_p <- calc(raw.layers$massRaster_p, fun = function(x){-2.84 + 0.6*x})
# ## Changing names for simplicity
# names(raw.layers) <- c("win", "mass", "fat")
# rawUTM <- st_transform(raw.layers, 2955)
# 
# ## crop, mask, and write
# rawOut <- mask(crop(rawUTM, mylu.utm), mylu.utm)
# writeRaster(rawOut,
#             file.path(win.res, "myluCropped_.tif"),
#             format = "GTiff",
#             bylayer = T,
#             suffix = "names",
#             overwrite=T)


#### Survival layers####
##fat required
# fatNames <- list.files(win.res, pattern = "myluFatReq", full.names = T)
# fatStk <- stack(fatNames)
# a <- sub(".*?_", "", names(fatStk))
# b <- sub("fat", "", a)
# c <- sub("\\.", "", b)
# names(fatStk) <- paste0("fat_", c)
# ## Appears to be too many layers to work
# projectRaster(fatStk, crs = CRS("+init=epsg:2955"),
#               filename = file.path(win.res,"myluCropped_.tif"),
#               format = "GTiff",
#               bylayer = T,
#               suffix = "names",
#               overwrite = T)
# 
# for( i in 1:nlayers(fatStk)){
#   projectRaster(fatStk[[i]], crs = CRS("+init=epsg:2955"),
#                 filename = file.path(win.res,
#                                      paste0("myluCropped_",
#                                             names(fatStk[[i]]),
#                                             ".tif")),
#                 format = "GTiff",
#                 overwrite = T)
#   cat( i, "Completed at", format(Sys.time(), "%a %b %d %X %Y"))
#   gc()
# }


# createSurv <- function(x){
#   ##function to read in, then create survival layers
#   ## x <- name from fatNames
#   
#   ##read in
#   x.rast <- projectRaster(raster(x),crs = CRS("+init=epsg:2955"))
#   x.utm.surv <- fat.av - x.rast
#   rm(x.rast)
#   
#   writeRaster(x.utm.surv,
#               file.path(win.res, 
#                         paste(c("myluCropped_surv",
#                                 sapply(strsplit(x, "_"),tail,3)),
#                               collapse = "_")),
#               format = "GTiff",
#               overwrite=T)
#   
#   gc()
# }
# 
# lapply(fatNames, createSurv)

## Function will not run with 32gb of ram

# for( i in 1:length(fatNames)){
#   ##read in
#   xx <- raster(fatNames[[i]])
#   x.rast <- projectRaster(xx,crs = CRS("+init=epsg:2955"))
#   rm(xx);gc()
#   ##fat avaliable
#   fat.av <- raster(file.path(win.res, "myluCropped__fat.tif"))
#   x.utm.surv <- fat.av - x.rast
#   rm(x.rast);gc()
#   
#   writeRaster(x.utm.surv,
#               file.path(win.res, 
#                         paste(c("myluCropped_surv",
#                                 sapply(strsplit(fatNames[[i]], "_"),tail,3)),
#                               collapse = "_")),
#               format = "GTiff",
#               overwrite=T)
#   rm(x.utm.surv)
#   gc()
# }


##Creating survival capacities for each of the simulations
  ## Issues with running out of ram on my 32 Gb machine
# surv.layers <- list()
# for(i in 1:nlayers(fatReq.layers)){
# 
#   out <- raw.layers$fat - fatReq.layers[[i]]
#   names(out) <- paste(c("surv",sapply(strsplit(names(fatReq.layers[[i]]), "_"),tail,3)),collapse = "_")
#   surv.layers[[i]] <- out
# }
# surv.stk <- do.call(stack, surv.layers)
# 
# full.stk <- stack(raw.layers, fatReq.layers, surv.stk)
# rm(raw.layers, fatReq.layers,surv.layers, surv.stk)


# crop.stk <- mask(crop(full.stk, mylu.dist), mylu.dist)
#
# writeRaster(crop.stk,
#             file.path(win.res, "myluCropped_.tif"),
#             format = "GTiff",
#             bylayer = T,
#             suffix = "names",
#             overwrite=T)
#
# plotStk <- stack(list.files(win.res, pattern = "myluCropped_*", full.names = T))
# names(plotStk) <- sub(".*?__", "", names(plotStk))

## create new pre-processed UTM items for plotting purposes 
# plotUTM <- projectRaster(plotStk, crs = CRS("+init=epsg:2955"))
# writeRaster(plotUTM,
#             file.path(win.res, "myluCroppedUTM_.tif"),
#             format = "GTiff",
#             bylayer = T,
#             suffix = "names",
#             overwrite=T)
#### Load data ####
library(raster);library(tidyverse); library(metR)
## Mass, Fat and Winter duration
plotStk <- stack(list.files(win.res, pattern = "myluCropped__*", full.names = T)[1:3])
names(plotStk) <- sub(".*?__", "", names(plotStk))
## Survival stack
survStk <- stack(list.files(win.res, pattern = "myluCropped_surv_*", full.names = T))
a <- sub(".*?_", "", names(survStk))
b <- sub("fat", "", a)
c <- sub("\\.", "", b)
names(survStk) <- c
## fat required
fatStk <- stack(list.files(win.res, pattern = "myluCropped_fat_*", full.names = T))
a <- sub(".*?_", "", names(fatStk))
names(fatStk) <- a

plotStk <- stack(plotStk, survStk)

# quick <- tidy(cellStats(plotStk, summary))
# write.csv(x = quick,
#           file = file.path(win.res, "quickSummary.csv"),
#           row.names = F)
#### Functions ####
masterPlotter2 <- function(x,
                           break.size,
                           c.string,
                           res.agg = 20,
                           dist.map = mylu.utm,
                           north.america = NA.utm,
                           canada.focus = NULL,
                           legend.key, text.min =25,
                           save.name = NULL,  device.out = NULL,  ...){
  ##Function for plotting all  wintor spatial figures   
  # x <- item plotting, 
  # c.string <- for colors
  # break.by <- for handling the number of break to appear in the data
  # res.agg <- unit of aggragation (generally for the dev period with large maps)
  # dist.map <- species distribution map of you want it to be included in the plotting
  # north.america <- geo-political boundries to plot on top of
  # canada.focus <- something you can get an extent from to crop from
  # legend.key <- string to place on the legend
  # text.min <- minimum size for text appearance
  # save.name <- string for saving the figures
  # device.out <- choose which driver is used to write out figures
  # ... <- additional arguments to be passed to ggsave
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg)
  }
  else{
    x.ag <- x}
  
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
    north.america <- st_crop(north.america, dist.crop)
    
  }
  else{
    ## Crop background to distribution
    dist.crop <- dist.map
    north.america <- st_crop(north.america, dist.crop)
  }
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("Easting", "Northing", "winter")
  
  ## handing break for the visuals
  break.string <- seq(floor(min(x.df$winter, na.rm=T)),
                      ceiling(max(x.df$winter, na.rm=T)),
                      by = break.size)
  
  g.win <- ggplot() +
    ##Contouring
    geom_contour_fill(data = x.df,
                      aes(x= Easting, y = Northing, z = winter),
                      breaks = break.string,
                      na.fill = -9999)+
    #handling the NA 
    stat_subset(data = x.df, 
                aes(x= Easting, y = Northing, subset = is.na(winter)),
                geom = "raster",
                fill = "#ffffff") +
    
    #oooohhhhh pretty colors
    scale_fill_gradientn(legend.key,
                         colors = c.string,
                         limits=  c(min(break.string),
                                    max(break.string))) +
    
    ##North American political boundries
    geom_sf(data = north.america,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA) +
    
    geom_sf(data = dist.crop,
            aes(group = "SP_ID"),
            colour = "dodgerblue4",
            size = .7,
            fill = NA)   +
    
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= Easting, y = Northing, z = winter),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    
    theme_bw()+
    theme(legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
    scale_x_continuous(limits =  c(extent(dist.crop)[1],extent(dist.crop)[2]))+
    scale_y_continuous(limits = c(extent(dist.crop)[3],extent(dist.crop)[4]))
  
  ## Save Flag  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    
    ggsave(filename = file.path(win.res,"fig", paste0(save.name,".", device.out)),
           g.win, 
           dpi = 400,
           device = dev.ext,
           ...)}
  gc()
  
  return(g.win)
}

pairedPlotting2 <- function(x,
                            parent.data = plotStk,
                            break.size,
                            res.agg = 20,
                            text.min = 25,
                            north.america = NA.utm,
                            canada.focus = NULL,
                            dist.map = mylu.utm,
                            c.string, 
                            legend.key = "Fill this in",
                            save.name = NULL, device.out = NULL,
                            ...){
  ## Subset out the paired layers
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
    north.america <- st_crop(north.america, dist.crop)
    
  }
  else{
    ## Crop background to distribution
    dist.crop <- dist.map
    north.america <- st_crop(north.america, dist.crop)
  }
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df) <- c("Easting", "Northing", "Infected", "Uninfected")
  
  x.df <- x.df %>%
    gather(key = "Status", 
           value = "Value",
           c("Infected", "Uninfected"))
  ##reorder factor levels
  x.df$Status <- factor(x.df$Status, levels = c("Uninfected", "Infected"))
  ##break points for the legend
  break.string <- seq(floor(min(x.df$Value, na.rm = T)),
                      ceiling(max(x.df$Value, na.rm = T)),
                      by = break.size)
  
  g.win <- ggplot()+
    geom_contour_fill(data = x.df,
                      aes(x= Easting, y = Northing, z = Value),
                      breaks = break.string,
                      na.fill = -9999)+
    #handling the NA 
    stat_subset(data = x.df, 
                aes(x= Easting, y = Northing, subset = is.na(Value)),
                geom = "raster",
                fill = "#ffffff")+
    
    #oooohhhhh pretty colors
    scale_fill_gradientn(legend.key,
                         colors = c.string,
                         limits=  c(min(break.string),
                                    max(break.string))) +
    
    ##North American political boundries
    geom_sf(data = north.america,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA)+
    geom_sf(data = dist.crop,
            aes(group = "SP_ID"),
            colour = "dodgerblue4",
            fill = NA)   +
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= Easting, y = Northing, z = Value),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm"))+
    scale_x_continuous(limits =  c(extent(dist.crop)[1],extent(dist.crop)[2]))+
    scale_y_continuous(limits = c(extent(dist.crop)[3],extent(dist.crop)[4]))+
    facet_wrap( .~ Status,
                nrow = 1)
  
  ## Save Flag  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    
    ggsave(filename = file.path(win.res,"fig", paste0(save.name,".", device.out)),
           g.win, 
           dpi = 400,
           device = dev.ext,
           ...)}
  gc()
  return(g.win)
}

quadPlot2 <- function(x,
                     parent.data = plotStk, 
                     res.agg = 20,
                     text.min = 30,
                     north.america = NA.utm,
                     canada.focus = NULL,
                     dist.map = mylu.utm,
                     c.string,
                     legend.key = "Fill this in",
                     save.name = NULL,
                     device.out = NULL,
                     ...){
  
  ### Subset out the paired layers
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
    north.america <- st_crop(north.america, dist.crop)
    
  }
  else{
    ## Crop background to distribution
    dist.crop <- dist.map
    north.america <- st_crop(north.america, dist.crop)
  }
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df)[1:2] <- c("Easting", "Northing")
  
  x.df <- x.df %>%
    gather(key ="Layer",
           value = "Value",
           starts_with(strsplit(names(x.ag),"_")[[1]][[1]])) %>% 
    mutate(Infection_status = case_when(str_detect(Layer, "inf") ~ "Infected",
                                        str_detect(Layer, "null") ~ "Uninfected"),
           Hibernation_Condition = case_when(str_detect(Layer, "2_100") ~ "2x100",
                                             str_detect(Layer, "4_98") ~ "4x98"))
  
  ##reorder factor levels
  x.df$Infection_status <- factor(x.df$Infection_status,
                                  levels = c("Uninfected", "Infected"))
  x.df$Hibernation_Condition <- factor(x.df$Hibernation_Condition,
                                       levels = c("4x98", "2x100"))
  
  ##break points for the legend
  break.string <- seq(floor(min(x.df$Value, na.rm = T)),
                      ceiling(max(x.df$Value, na.rm = T)),
                      by = .5)
  
  (g.win <- ggplot()+
      geom_contour_fill(data = x.df,
                        aes(x= Easting, y = Northing, z = Value),
                        breaks = break.string,
                        na.fill = -9999)+
      #handling the NA 
      stat_subset(data = x.df, 
                  aes(x= Easting, y = Northing, subset = is.na(Value)),
                  geom = "raster",
                  fill = "#ffffff")+
      
      #oooohhhhh pretty colors
      scale_fill_gradientn(legend.key,
                           colors = c.string,
                           limits=  c(min(break.string),
                                      max(break.string))) +
      
      ##North American political boundries
      geom_sf(data = north.america,
              aes(group = "Name_1"),
              color="grey20",
              fill=NA)+
      
      #Lables
      geom_text_contour(data = x.df, 
                        aes(x= Easting, y = Northing, z = Value),
                        stroke = 0.2, min.size = text.min,
                        rotate = F, check_overlap = T)+
      
      theme_bw()+
      scale_x_continuous(limits =  c(extent(dist.crop)[1],extent(dist.crop)[2]))+
      scale_y_continuous(limits = c(extent(dist.crop)[3],extent(dist.crop)[4]))+
      theme(legend.position = "bottom",
            legend.text=element_text(size=7),
            legend.title=element_text(size=9),
            axis.title = element_blank())+
      facet_grid( rows = vars(Hibernation_Condition),
                  cols = vars(Infection_status))
  )
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    dist.crop <- st_crop(dist.map, x.ag)
    
    g.win <- g.win +
      geom_sf(data = dist.crop,
              aes(group = "SP_ID"),
              colour = "black",
              fill = NA) 
  }
  
  ## Save Flag  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    
    ggsave(filename = file.path(win.res,"fig", paste0(save.name,".", device.out)),
           g.win, 
           dpi = 400,
           device = dev.ext,
           ...)}
  gc()
  return(g.win)
}
octoPlot <- function(parent.data = plotStk, 
                     res.agg = 20,
                     text.min = 30,
                     north.america = NA.utm,
                     canada.focus = NULL,
                     dist.map = mylu.utm,
                     c.string,
                     legend.key = "Fill this in",
                     save.name = NULL,
                     device.out = NULL,
                     ...){
  
  ### Subset out the paired layers
  rh.conditions <- c("80", "90", "95", "100")
  names(parent.data)[unlist(lapply(rh.conditions, grep, x = names(parent.data)))]
  
  grep(pattern = , names(parent.data))
  
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
    north.america <- st_crop(north.america, dist.crop)
    
  }
  else{
    ## Crop background to distribution
    dist.crop <- dist.map
    north.america <- st_crop(north.america, dist.crop)
  }
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df)[1:2] <- c("Easting", "Northing")
  
  x.df <- x.df %>%
    gather(key ="Layer",
           value = "Value",
           starts_with(strsplit(names(x.ag),"_")[[1]][[1]])) %>% 
    mutate(Infection_status = case_when(str_detect(Layer, "inf") ~ "Infected",
                                        str_detect(Layer, "null") ~ "Uninfected"),
           Hibernation_Condition = case_when(str_detect(Layer, "2_100") ~ "2x100",
                                             str_detect(Layer, "4_98") ~ "4x98"))
  
  ##reorder factor levels
  x.df$Infection_status <- factor(x.df$Infection_status,
                                  levels = c("Uninfected", "Infected"))
  x.df$Hibernation_Condition <- factor(x.df$Hibernation_Condition,
                                       levels = c("4x98", "2x100"))
  
  ##break points for the legend
  break.string <- seq(floor(min(x.df$Value, na.rm = T)),
                      ceiling(max(x.df$Value, na.rm = T)),
                      by = .5)
  
  (g.win <- ggplot()+
      geom_contour_fill(data = x.df,
                        aes(x= Easting, y = Northing, z = Value),
                        breaks = break.string,
                        na.fill = -9999)+
      #handling the NA 
      stat_subset(data = x.df, 
                  aes(x= Easting, y = Northing, subset = is.na(Value)),
                  geom = "raster",
                  fill = "#ffffff")+
      
      #oooohhhhh pretty colors
      scale_fill_gradientn(legend.key,
                           colors = c.string,
                           limits=  c(min(break.string),
                                      max(break.string))) +
      
      ##North American political boundries
      geom_sf(data = north.america,
              aes(group = "Name_1"),
              color="grey20",
              fill=NA)+
      
      #Lables
      geom_text_contour(data = x.df, 
                        aes(x= Easting, y = Northing, z = Value),
                        stroke = 0.2, min.size = text.min,
                        rotate = F, check_overlap = T)+
      
      theme_bw()+
      scale_x_continuous(limits =  c(extent(dist.crop)[1],extent(dist.crop)[2]))+
      scale_y_continuous(limits = c(extent(dist.crop)[3],extent(dist.crop)[4]))+
      theme(legend.position = "bottom",
            legend.text=element_text(size=7),
            legend.title=element_text(size=9),
            axis.title = element_blank())+
      facet_grid( rows = vars(Hibernation_Condition),
                  cols = vars(Infection_status))
  )
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    dist.crop <- st_crop(dist.map, x.ag)
    
    g.win <- g.win +
      geom_sf(data = dist.crop,
              aes(group = "SP_ID"),
              colour = "black",
              fill = NA) 
  }
  
  ## Save Flag  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    
    ggsave(filename = file.path(win.res,"fig", paste0(save.name,".", device.out)),
           g.win, 
           dpi = 400,
           device = dev.ext,
           ...)}
  gc()
  return(g.win)
}

increasedExpendaturePlot2 <- function(x,
                                      parent.data = plotStk, 
                                      res.agg = 20,
                                      north.america =NA.utm,
                                      canada.focus = NULL,
                                      dist.map = mylu.utm,
                                      text.min = 35,
                                      legend.key = "Fill this in",
                                      save.name = NULL,
                                      device.out = NULL,
                                      ...){
  
  ## Subset out the paired layers
  target.data <- parent.data[[grep(pattern = x, 
                                   names(parent.data))]]
  
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(target.data, res.agg)
  }
  else{
    x.ag <- target.data}
  ## Canada focus flag
  if(!is.null(canada.focus)){
    x.ag <- crop(x.ag, extent(canada.focus))
    dist.crop <- st_crop(dist.map, extent(canada.focus))
    north.america <- st_crop(north.america, dist.crop)
    
  }
  else{
    ## Crop background to distribution
    dist.crop <- dist.map
    north.america <- st_crop(north.america, dist.crop)
  }
  
  ## Convert to df
  x.pts <- cbind(xyFromCell(x.ag, 1:ncell(x.ag)), values(x.ag)) #to points
  x.df <- data.frame(x.pts)
  #Note infected and null are generally in that order 
  colnames(x.df) <- c("Easting", "Northing", "Infected", "Uninfected")
  
  x.df <- x.df %>%
    mutate(precIncrease = (Infected/Uninfected)*100)
  
  ## create breaks
  break.string <- seq(floor(min(x.df$precIncrease, na.rm = T)),
                      ceiling(max(x.df$precIncrease, na.rm = T)),
                      by = 25)
  colourCount = length(break.string)
  getPalette = colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Spectral"))
  
  g.win <- ggplot() +
    ##Contouring
    geom_contour_fill(data = x.df,
                      aes(x= Easting, y = Northing, z = precIncrease),
                      breaks = break.string,
                      na.fill = -9999)+
    #handling the NA 
    stat_subset(data = x.df, 
                aes(x= Easting, y = Northing, subset = is.na(precIncrease)),
                geom = "raster",
                fill = "#ffffff")+
    
    scale_fill_gradientn(legend.key,
                         colors = rev(getPalette(colourCount)),
                         limits=  c(min(break.string),
                                    max(break.string))) +
    ##North American political boundries
    geom_sf(data = north.america,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA)+
    
    geom_sf(data = dist.crop,
            aes(group = "SP_ID"),
            colour = "dodgerblue4",
            size = .7,
            fill = NA)   +
    
    #Lables
    geom_text_contour(data = x.df, 
                      aes(x= Easting, y = Northing, z = precIncrease),
                      stroke = 0.2, min.size = text.min,
                      rotate = F, check_overlap = T)+
    
    theme_bw()+
    scale_x_continuous(limits =  c(extent(dist.crop)[1],extent(dist.crop)[2]))+
    scale_y_continuous(limits = c(extent(dist.crop)[3],extent(dist.crop)[4]))+
    theme(legend.position = "bottom",
          legend.text=element_text(size=7),
          legend.title=element_text(size=9),
          axis.title = element_blank())
  
  ## Save Flag  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    
    ggsave(filename = file.path(win.res,"fig",
                                paste0(save.name,".", device.out)),
           g.win, 
           dpi = 400,
           device = dev.ext,
           ...)}
  gc()
  
  return(g.win)
}


#### Winter duration plots ####

library(gridExtra)
## Plot Function 

#Winter duration raster
winterColors <- colorRampPalette(c("#e0ecf4", "#9ebcda","#8856a7"))
winter.full <- masterPlotter2(x = plotStk$win,
                              c.string = winterColors(5),
                              break.size = 30,
                              canada.focus = NULL,
                              legend.key = "Predicted\nDuration\nWnter\n(Days)",
                              text.min = 50,
                              save.name = "winDuration_Mean_MYLU",
                              device.out = "pdf",
                              width = 6,
                              unit = "in")
 winter.can <- masterPlotter2(x = plotStk$win,
                             c.string = winterColors(5),
                             break.size = 30,
                             canada.focus = can.utm,
                             legend.key = "Predicted\nDuration\nWnter\n(Days)",
                             text.min = 25,
                             save.name = "winDuration_Mean_MYLU_Canada",
                             device.out = "pdf",
                             width = 6,
                             unit = "in")


rm(winter.full, winter.can, winterColors)
gc()

#### Body Mass and Fat Mass Plots####
massColors <- colorRampPalette(c("#f7fcb9", "#31a354"))
mass.full <- masterPlotter2(x = plotStk$mass,
                            c.string = massColors(5),
                            break.size = .5,
                            text.min = 50,
                            canada.focus = NULL,
                            legend.key = "Predicted\nBody\nMass (g)",
                            save.name = "massMean_MYLU",
                            device.out = "pdf",
                            width = 6,
                            unit = "in")

mass.can <- masterPlotter2(x = plotStk$mass,
                           c.string = massColors(5),
                           break.size = .5,
                           text.min = 25,
                           canada.focus = can.utm,
                           legend.key = "Predicted\nBody\nMass (g)",
                           save.name = "massMean_MYLU_Canada",
                           device.out = "pdf",
                           width = 6,
                           unit = "in")


fatColors <- colorRampPalette(c("#fff7bc","#fec44f", "#d95f0e"))
fat.full <- masterPlotter2(x = plotStk$fat,
                           c.string = fatColors(5),
                           break.size = .5,
                           text.min = 50,
                           canada.focus = NULL,
                           legend.key = "Predicted\nBody\nFat (g)",
                           save.name = "fatMean_MYLU",
                           device.out = "pdf",
                           width = 6,
                           unit = "in")

fat.can <- masterPlotter2(x = plotStk$fat,
                          c.string = fatColors(5),
                          break.size = .5,
                          text.min = 25,
                          canada.focus =can.utm,
                          legend.key = "Predicted\nBody\nFat (g)",
                          save.name = "fatMean_MYLU_Canada",
                          device.out = "pdf",
                          width = 6,
                          unit = "in")


## Single figure
library(gridExtra)
fig3 <- grid.arrange(mass.full,
                     fat.full,
                     nrow = 1)
ggsave(file.path(win.res, "fig", "Mass_Fat_MYLU.pdf"),
       fig3,
       device = cairo_pdf,
       width = 8,
       height = 4,
       units = "in")

fig3Canada <- grid.arrange(mass.can,
                           fat.can,
                           nrow = 1)
ggsave(file.path(win.res, "fig", "Mass_Fat_MYLU_Canda.pdf"),
       fig3Canada,
       device = cairo_pdf,
       width = 8,
       height = 4,
       units = "in")
rm(mass.can,mass.full,massColors,fat.can,fat.full,fatColors,fig3Canada,fig3)
gc()

#### Required Fat plots ####
reqColors <- colorRampPalette(c("#ffffff", "#5E3C99"))

fatReq4x98 <- pairedPlotting2(x = "fat_4_98",
                             parent.data = plotStk,
                             c.string = reqColors(3),
                             canada.focus = NULL,
                             legend.key = "Predicted\nBody Fat\nRequired (g)",
                             save.name = "fatRequired4_98_pair",
                             device.out = "pdf",
                             width = 8,
                             height = 4,
                             unit = "in")

fatReq4x98 <- pairedPlotting2(x = "fatReq_4_98",
                              parent.data = plotStk,
                              c.string = reqColors(3),
                              canada.focus = can.utm,
                              legend.key = "Predicted\nBody Fat\nRequired (g)",
                              save.name = "fatRequired4_98_Canada",
                              device.out = "pdf",
                              width = 8,
                              height = 4,
                              unit = "in")

fatReq2x100 <- pairedPlotting2(x = "fat_2_100",
                              parent.data = plotStk,
                              c.string = reqColors(3),
                              legend.key = "Predicted\nBody Fat\nRequired (g)",
                              save.name = "fatRequired2_100_pair",
                              device.out = "pdf",
                              width = 8,
                              height = 4,
                              unit = "in")
fatReq2x100 <- pairedPlotting2(x = "fatReq_2_100",
                              parent.data = plotStk,
                              c.string = reqColors(3),
                              canada.focus = can.utm,
                              legend.key = "Predicted\nBody Fat\nRequired (g)",
                              save.name = "fatRequired2_100_pair_Canada",
                              device.out = "pdf",
                              width = 8,
                              height = 4,
                              unit = "in")
rm(fatReq4x98,fatReq2x100,reqColors)
gc()

#### Survival Mapping ####

## 2x 100
## Colors need to be between -1 and 5
above0 <- colorRampPalette(c("#E8E3F0", "#5E3C99"))

survColors_2x100 <- colorRampPalette(c("#fdb863",## The one below 0
                                       above0(5)))
a <- pairedPlotting2(x = "surv_100_2",
                     res.agg = 25,
                     parent.data = plotStk,
                     c.string = survColors_2x100(6),
                     break.size = .5,
                     legend.key = "Predicted\nBody Fat\nRemaining (g)",
                     save.name = "survival_2x100", 
                     device.out = "pdf",
                     width = 8,
                     height = 4,
                     unit = "in")

# q <- pairedPlotting2(x = "surv_2_100",
#                      parent.data = plotStk,
#                      c.string = survColors_2x100(6),
#                      canada.focus = can.utm,
#                      legend.key = "Predicted\nBody Fat\nRemaining (g)",
#                      save.name = "survival_2x100_Canada", 
#                      device.out = "pdf",
#                      width = 8,
#                      height = 4,
#                      unit = "in")




## 4 x 98 Range is between -.22 - 6.62
  survColors_4x98 <- colorRampPalette(c("#fdb863",
                                        "#E8E3F0", "#BAABD2", "#8C73B5","#5E3C99"))
  
  a <- pairedPlotting2(x = "surv_98_4",
                       parent.data = plotStk,
                       c.string = survColors_4x98(5),
                       legend.key = "Predicted\nBody Fat\nRemaining (g)",
                       save.name = "survival_4x98", 
                       device.out = "pdf",
                       width = 8,
                       height = 4,
                       unit = "in")


# survColors_4x98.can <- colorRampPalette(c("#E8E3F0", "#A38FC4", "#5E3C99"))
# q <- pairedPlotting2(x = "surv_4_98",
#                      parent.data = plotStk,
#                      c.string = survColors_4x98.can(4),
#                      canada.focus = can.utm,
#                      legend.key = "Predicted\nBody Fat\nRemaining (g)",
#                      save.name = "survival_4x98_Canada", 
#                      device.out = "pdf",
#                      width = 8,
#                      height = 4,
#                      unit = "in")

## Quad plot
survColors_4x98 <- colorRampPalette(c("#fdb863",
                                      "#E8E3F0", "#BAABD2", "#8C73B5","#5E3C99"))
doh <- quadPlot2(x = "surv",
                c.string = survColors_4x98(6),
                legend.key = "Predicted\nBody Fat\nRemaining (g)",
                save.name = "survQuad", 
                device.out = "pdf",
                width = 10.5,
                height = 8,
                unit = "in")

rm(a, q, survColors_4x98, survColors_4x98.can, survColors_2x100)
## Precent increase plot

a <- increasedExpendaturePlot2(x <- "fatReq_4_98",
                              parent.data = plotStk,
                              res.agg = 20,
                              text.min = 50,
                              north.america = NA.utm,
                              canada.focus = NULL,
                              legend.key = "Precent\nIncreased\nFat\nRequired",
                              save.name = "precIncrease_4x98",
                              device.out = "pdf",
                              width = 6, 
                              units = "in")
a <- increasedExpendaturePlot2(x <- "fatReq_4_98",
                               parent.data = plotStk,
                               res.agg = 20,
                               text.min = 50,
                               north.america = NA.utm,
                               canada.focus = can.utm,
                               legend.key = "Precent\nIncreased\nFat\nRequired",
                               save.name = "precIncrease_4x98_Canada",
                               device.out = "pdf",
                               width = 6, 
                               units = "in")


b <- increasedExpendaturePlot2(x <- "fatReq_2_100",
                              parent.data = plotStk,
                              res.agg = 20,
                              text.min = 50,
                              north.america = NA.utm,
                              legend.key = "Precent\nIncreased\nFat\nRequired",
                              save.name = "precIncrease_2x100",
                              device.out = "pdf",
                              width = 6, 
                              units = "in")

b <- increasedExpendaturePlot2(x <- "fatReq_2_100",
                               parent.data = plotStk,
                               res.agg = 20,
                               text.min = 50,
                               north.america = NA.utm,
                               canada.focus = can.utm,
                               legend.key = "Precent\nIncreased\nFat\nRequired",
                               save.name = "precIncrease_2x100_Canada",
                               device.out = "pdf",
                               width = 6, 
                               units = "in")





fatSurvivalHistograms <- function(survNULL, survINF, dist.map, c.string,
                                  canada.focus = F, 
                                 save.name = NULL, device.out = NULL){
  stk <- stack(survNULL, survINF); names(stk) <- c("null", "inf")
  stk.ag <- aggregate(stk, 25)
  if(canada.focus == T){
    can.ext <- c(-140,-104,41,60)
    dist.map <- crop(dist.map, can.ext)
  }
  stk.mask <- mask(crop(stk.ag, dist.map), dist.map)
  stk.df <- na.omit(as.data.frame(values(stk.mask))) %>%
    gather("status", "fat", 1:2 ) 
  stk.med <- stk.df %>%
    group_by(status) %>%
    summarise(med = median(fat))
  
  dif.hist <- ggplot(stk.df, aes(x=fat, fill = status, color = status))+
    scale_color_manual(values = c.string) +
    scale_fill_manual(values = c.string) +
    geom_histogram(binwidth = .25, alpha = .5, position = "identity") +
    xlim(-3,3)+
    geom_vline(data = stk.med, aes(xintercept = med, color = status),
               linetype = "dashed", size = 1)+
    geom_vline(xintercept = 0)+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    
    ggsave(filename = file.path(win.res, "fig", paste0(save.name,".", device.out)),
           dif.hist, 
           width = 8, height = 5, unit = "in",
           dpi = 300,
           device = dev.ext)}
  
  
  return(dif.hist)
  
}

(staticHist <- fatSurvivalHistograms(survNULL = plotStk$surv_4_98_null,
                                     survINF =  plotStk$surv_4_98_inf,
                                     mylu.dist,
                                     survColors(2),
                                     canada.focus = F,
                                     save.name = "fatSurvivalHist_4_98",
                                     device.out = "pdf"))
(staticHist <- fatSurvivalHistograms(survNULL = plotStk$surv_2_100_null,
                                     survINF =  plotStk$surv_2_100_inf,
                                     mylu.dist,
                                     survColors(2),
                                     canada.focus = F,
                                     save.name = "fatSurvivalHist_2_100",
                                     device.out = "pdf"))


#### location x Data type ####
library(tidyverse);library(data.table)
dur.raw <- fread("data/durationDataReferenced.csv")
mass.raw <- fread("data/massDataReferenced.csv")

dur.raw$type <- "Duration"
mass.raw$type <- "Mass"

full.dat <- full_join(dur.raw, mass.raw) %>%
  dplyr::select(Lat, Long, type)
colnames(full.dat)[[3]] <- "Data Type"

full.sf <- st_as_sf(full.dat,
                   coords = c("Long","Lat"))
st_crs(full.sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
full.utm <- st_transform(full.sf, 2955)

NA.mylu <- st_crop(NA.utm, mylu.utm)



(dat.map <- ggplot() +
    ##North American political boundries
    geom_sf(data = NA.mylu,
            aes(group = "Name_1"),
            color="grey20",
            fill="grey90")+
    geom_sf(data = mylu.utm,
            aes(group = "SP_ID"),
            color="dodgerblue4",
            fill=NA, 
            size = .7)+
    geom_sf(data = full.utm,
            aes(shape = `Data Type`, color = `Data Type`),
            alpha = .6, position = "dodge",
            size = 2,
            show.legend = "point")+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    scale_color_manual(values=c("#E69F00","#56B4E9")) +
    theme_bw()
    )


ggsave(filename = file.path(win.res, "fig", "dataLocations.pdf"),
       dat.map, device = "pdf",
       height = 4.875,
       width = 7.85,
       units = "in",
       dpi = 400)


######### Sand Box ##########
## highest relative change: largest differences between infection and not
surv.dif <-  fatReq984.inf - fatReq984.null
p.dif <- (surv.dif/fatMean)*100


plot(surv.dif)
fatDiffPlot <- function(x, c.string, res.agg = 25, dist.map = NULL,
                        north.america = North.America, canada.focus = F,
                        save.name = NULL,  device.out = NULL,  ...){
  ## Create DataFrame (aggragation is mainly for the dev period)
  if(!is.null(res.agg)){ #aggratetion bits
    x.ag <- raster::aggregate(x, res.agg, fun=mean)
  }
  else{
    x.ag <- x}
  ## Crop and mask to distribution
  if(!is.null(dist.map)){
    x.ag <- mask(crop(x.ag, dist.map), dist.map)
  }
  if(canada.focus == T){
    can.ext <- extent(-140,-104,41,60)
    x.ag <-crop(x.ag, can.ext)
  }
  
  ## Convert to df
  x.pts <- rasterToPoints(x.ag) #to points
  x.df <- data.frame(x.pts)
  colnames(x.df) <- c("long", "lat", "winter")
  
  g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
    coord_fixed(xlim = extent(x.ag)[1:2], ylim = extent(x.ag)[3:4]) +
    #Raster fill
    geom_raster(aes(fill = winter),  interpolate = T) +
    #oooohhhhh pretty colors
    scale_fill_gradientn("Predicted\ndifference\nin body fat\nrequired (g)",
                         colors = c.string,
                         limits=  c(0,
                                    maxValue(x.ag))) +
    #border lines
    geom_polygon(data= fortify(North.America),
                 aes(long,lat,group=group),
                 color="grey20",
                 fill=NA,
                 inherit.aes = F) +
    #general malarkey
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(plot.title = element_text(hjust = .05))+
    #ggtitle("Predicted Body Mass") + 
    theme_bw()
  
  
  ##Distribution map flag
  if(!is.null(dist.map)){
    g.win <- g.win +
      geom_polygon(data = fortify(dist.map),
                   aes(long,lat, group = group),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F) 
  }
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    if(canada.focus==T){
      aspect.ratio <- (can.ext[[2]] - can.ext[[1]])/(can.ext[[4]] - can.ext[[3]])
    } else{
      aspect.ratio <-(ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])  
    }
    ggsave(filename = file.path(win.res, "fig", paste0(save.name,".", device.out)),
           g.win, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...
    )}
  
  return(g.win)
}
fdiffcolors <- colorRampPalette(c( "#ffffff", "#fdb863", "#e66101" ))

fdiff <- fatDiffPlot(surv.dif, fdiffcolors(3),
                     dist.map = mylu.dist)
thing


## Survival differential 
survDiff <- function(x, y = plotStk$fat){
  #x is survival capacity
  #y is predicted fat content
  if(x<0){
    val <- abs(x)/y
  } else{ val <- 0}
    
  return(val)
    
}

precNull <- calc(plotStk$survNull, survDiff) 
precInf <- 
  
# lil hack hack to get it working  
precNull <- plotStk$survNull  
values(precNull)[which(values(precNull)>=0)] <- 0
b <- values(precNull)[which(values(precNull) != 0)]
summary(b)
surv.pctNull <- (abs(precNull)/plotStk$fat) *100
plot(surv.pctNull)
a <- values(surv.pct)[which(values(surv.pct) != 0)]
hist(a)
summary(a)
writeRaster(surv.pct, filename = file.path(win.res, "survivalPrecentNull.tif"), 
            format = "GTiff", overwrite = T)

precNull <- plotStk$survInf  
values(precNull)[which(values(precNull)>=0)] <- 0
b <- values(precNull)[which(values(precNull) != 0)]
summary(b)
surv.pctInf <- (abs(precNull)/plotStk$fat) *100
plot(surv.pctInf)
a <- values(surv.pct)[which(values(surv.pct) != 0)]
hist(a)
summary(a)
writeRaster(surv.pct, filename = file.path(win.res, "survivalPrecentInf.tif"), 
            format = "GTiff")

## make plots for these?
 colpal <- colorRampPalette(C( "#f1948a" , "#ec7063", "#cb4335"))

 
 (precSurv.plot <- masterPlotter(x = surv.pctNull,
                                c.string = colpal,
                                canada.focus = F,
                                legend.key = "Precent\nfat\nShortfall (g)",
                                dist.map = mylu.dist,
                                save.name = "precSurvNull_Dist",
                                device.out = "pdf"))

 (precSurvInf.plot <- masterPlotter(x = surv.pctInf,
                                 c.string = fdiffcolors(3),
                                 canada.focus = F,
                                 legend.key = "Precent\nfat\nShortfall (g)",
                                 dist.map = mylu.dist,
                                 save.name = "fatShortfallPrecentInf",
                                 device.out = "pdf"))
                                  

####No Longer needed ####
# survColorsPOS <- colorRampPalette(c( "#ffffff", "#5e3c99","#b2abd2")) #"#fdb863", "#e66101"
# Surv.plotPOS <- function(x, c.string, res.agg = 25, dist.map = NULL,
#                      save.name = NULL,  device.out = NULL,  ...){
#   ## Create DataFrame (aggragation is mainly for the dev period)
#   if(!is.null(res.agg)){ #aggratetion bits
#     x.ag <- raster::aggregate(x, res.agg)
#   }
#   else{
#     x.ag <- x}
#   ## Crop and mask to distribution
#   if(!is.null(dist.map)){
#     x.ag <- mask(crop(x.ag, dist.map), dist.map)
#   }
#   
#   ## Convert to df
#   x.pts <- rasterToPoints(x.ag) #to points
#   x.df <- data.frame(x.pts)
#   colnames(x.df) <- c("long", "lat", "winter")
#   
#   g.win <- ggplot(data = x.df, aes(x = long, y = lat, z = winter)) +
#     coord_fixed(xlim = extent(x)[1:2], ylim = extent(x)[3:4]) +
#     #border lines
#     borders("world",
#             xlim=extent(x)[1:2],ylim=extent(x)[3:4],
#             colour = "grey20",
#             fill = "grey80")+
#     #Raster fill
#     geom_raster(aes(fill = winter),  interpolate = T) +
#     #oooohhhhh pretty colors
#     scale_fill_gradientn("Predicted\nBody\nFat\nRemaining (g)",
#                          colors = c.string,
#                          limits=  c(floor(minValue(x.ag)),
#                                     ceiling(maxValue(x.ag)))) +
#     
#     #general malarkey
#     scale_x_continuous(expand = c(0,0))+
#     scale_y_continuous(expand = c(0,0))+
#     theme(plot.title = element_text(hjust = .05))+
#     #ggtitle("Predicted Body Mass") + 
#     theme_bw()
#   
#   ##Distribution map flag
#   if(!is.null(dist.map)){
#     g.win <- g.win +
#       geom_polygon(data = fortify(dist.map),
#                    aes(long,lat, group = group),
#                    colour = "black",
#                    fill = NA,
#                    inherit.aes = F) 
#   }
#   
#   if(!is.null(save.name)){
#     if(device.out == "pdf"){
#       dev.ext <- cairo_pdf
#     } else if (device.out =="eps"){
#       dev.ext <- cairo_ps
#     } else {
#       dev.ext <- device.out
#     }
#     ex <- as.vector(extent(x))
#     aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
#     ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
#            g.win, 
#            width = 9, height = 9/aspect.ratio, unit = "in",
#            dpi = 300,
#            device = dev.ext,
#            ...)}
#   
#   return(g.win)
# }
# 
# (staticNull.plot <- Surv.plotPOS(survStatic.null, survColorsPOS(5)))
# (staticInf.plot <- Surv.plotPOS(survStatic.inf, survColorsPOS(5)))
# 

#### Surface conditions####
fatReq.null <- raster(file.path(win.res, "MYLU_fatRequired_fat.null.tif"))
fatReq.inf <- raster(file.path(win.res, "MYLU_fatRequired_fat.inf.tif"))

surfaceSurv.null <- fatMean - fatReq.null
surfaceSurv.inf <- fatMean - fatReq.inf
 #
(surfaceNull.plot <- Surv.plot(surfaceSurv.null))
(surfaceInf.plot <- Surv.plot(surfaceSurv.inf))

fatSurvivalHistSurface <- function(survNULL, survINF, dist.map, c.string,
                                 save.name = NULL, device.out = NULL){
  stk <- stack(survNULL, survINF); names(stk) <- c("null", "inf")
  stk.ag <- aggregate(stk,25)
  stk.mask <- mask(crop(stk.ag, dist.map), dist.map)
  stk.df <- na.omit(as.data.frame(values(stk.mask))) %>%
    gather("status", "fat", 1:2 ) 
  stk.med <- stk.df %>%
    group_by(status) %>%
    summarise(med = median(fat))
  
  dif.hist <- ggplot(stk.df, aes(x=fat, fill = status, color = status))+
    scale_color_manual(values = c.string) +
    scale_fill_manual(values = c.string) +
    geom_histogram(binwidth = .25, alpha = .5, position = "identity") +
    xlim(-10,2)+
    geom_vline(data = stk.med, aes(xintercept = med, color = status),
               linetype = "dashed", size = 1)+
    geom_vline(xintercept = 0)+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
  
  if(!is.null(save.name)){
    if(device.out == "pdf"){
      dev.ext <- cairo_pdf
    } else if (device.out =="eps"){
      dev.ext <- cairo_ps
    } else {
      dev.ext <- device.out
    }
    ex <- as.vector(extent(x))
    aspect.ratio <- (ex[[2]] - ex[[1]])/(ex[[4]] - ex[[3]])
    ggsave(filename = file.path("fig", pasteo(save.name,".", device.out)),
           dif.hist, 
           width = 9, height = 9/aspect.ratio, unit = "in",
           dpi = 300,
           device = dev.ext,
           ...)}
  
  
  return(dif.hist)
  
}

(surfaceHist <- fatSurvivalHistSurface(surfaceSurv.null,surfaceSurv.inf,mylu.dist, survColors(2)))

#### Onset and End of winter for Meredith) ####

onset <- calc(durationMean, fun = function(x){ifelse(x > 0, 365-(x/2-10), NA)})
writeRaster(onset, file.path(win.res, "winterOnset.tif"), format = "GTiff")
end <- calc(durationMean,fun = function(x){ifelse(x > 0, (x/2)+10, NA)})
writeRaster(end, file.path(win.res, "winterEnd.tif"), format = "GTiff")


writeRaster(surfaceSurv.null, file.path(win.res,"myluSurace_nul.tif"), format = "GTiff")
writeRaster(surfaceSurv.inf, file.path(win.res, "myluSurace_inf.tif"), format = "GTiff")
writeRaster(survStatic.null, file.path(win.res, "myluStatic_null.tif"), format = "GTiff")
writeRaster(survStatic.inf, file.path(win.res, "myluStatic_inf.tif"), format = "GTiff")

#### Aranging and such ####
library(gridExtra)

## figure 2: Top Models 
  ## alternaticly may just be the winMean.plot
topMods <- grid.arrange(mass.fig+ theme(axis.title.y = element_blank()),
                        fat.fig + theme(axis.title.y = element_blank(),
                                             axis.title.x = element_blank()),
                        ncol = 1)

ggsave(file.path(win.res, "fig", "Mass_Fat.pdf"),
       topMods,
       device = cairo_pdf,
       width = 8,
       height = 6.5,
       units = "in")


## Issue: don't know how to get things to be the same sizes (issues come from 
  ## dropping the x axis attributes in the first two instances)

#figure 3 (alt): Top mass and fat 
massMean.Dist <- mass.plot(x = massmean,
                           c.string = massColors(5),
                           dist.map = mylu.dist)
fatMean.Dist <- fat.plot(fatMean, 
                         c.string = fatColors(5),
                         dist.map = mylu.dist)
massFat <- grid.arrange(massMean.Dist, 
                        fatMean.Dist,
                        ncol = 1)






















