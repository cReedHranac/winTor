#################################################
####  Plotting for WinTor Publication        ####
####  Author: C. Reed Hranac                 ####
####  Tested                                 ####
#################################################

## This script provides all the plotting code for the wintor publication
##  Contents:
  ## Main text figures:
  ## figure 1: Data type and location
  ## figure 2: Predicted winter duration across North America
  ## figure 3: Predicted body mass and body fat
  ## figure 4: Predicted hibernation survival 
  ## figure 5: Increased energy expenditure due to WNS infection

  ## Si figures:

#### Set Up ####
env.prior <- ls()

## libraries
library(tidyverse); library(sf); library(raster); library(data.table)

## after these are all generated the first time you can comment out all the 
## previous lines

##creating North America political boundries 
# canada <- getData("GADM",country="CAN",level=1)
# usa <- getData("GADM",country="USA", level=1)
# mexico <- getData("GADM",country="MEX", level=1)
# North.America <- rbind(canada,usa,mexico)
# plot(North.America)
# writeOGR(North.America,
#          dsn = win.dat,
#          layer = "NorthAmerica.WGS",
#          driver = "ESRI Shapefile")

library(sf);library(rgdal);library(raster)
North.America <- st_read(win.dat, layer="NorthAmerica.WGS")
NA.utm <- st_transform(North.America, 2955)

##mylu distribution
mylu.dist <- st_read(file.path(win.dat, "shapeFiles"), 
                     layer = "myotis_lucifugus")

st_crs(mylu.dist) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mylu.utm <- st_transform(mylu.dist, 2955)

## create utm projections and crop to the mylu distribution for all the prods
## create new 

dur.rast <- raster(file.path(win.res, "duration_p.tif"))
m.cropped <- stack(list.files(win.res,
                              pattern = "myluCropped_*",
                              full.names = T))
dur.utm <- projectRaster(dur.rast, CRS(mylu.utm))
#### Functions ####
masterPlotter <- function(x,
                          break.size,
                          c.string,
                          vis.break = NULL,
                          res.agg = 20,
                          dist.map = mylu.utm,
                          north.america = NA.mylu,
                          canada.focus = NULL,
                          legend.key, text.min =25,
                          save.name = NULL,  device.out = NULL,  ...){
  ##Function for plotting single feature wintor spatial figures   
  # x <- item plotting, 
  # c.string <- for colors
  # vis.break <- optional arg 2x1 vector for manual limitation of  legend colors
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
  if(is.null(vis.break)){
    break.string <- seq(floor(min(x.df$winter, na.rm=T)),
                        ceiling(max(x.df$winter, na.rm=T)),
                        by = break.size)
  }
  else{
    break.string <- seq(vis.break[[1]],
                        vis.break[[2]],
                        by = break.size)
  }
  
  
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

####  Figure 1 ####
#### location x Data type 
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

##further crop to the species extent 
NA.mylu <- st_crop(NA.utm, mylu.utm) ## this apparently wont work

## plot
(dat.map <- ggplot() +
    ##North American political boundaries
    geom_sf(data = NA.mylu,
            aes(group = "Name_1"),
            color="grey20",
            fill=NA)+
    geom_sf(data = mylu.utm,
            aes(group = "SP_ID"),
            color="dodgerblue4",
            fill="lightblue1", 
            size = .7,
            alpha = .2)+
    geom_sf(data = full.utm,
            aes(shape = `Data Type`, color = `Data Type`),
            alpha = .7, #position = "dodge",
            size = 2,
            show.legend = "point")+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    scale_color_manual(values=c("black","red")) +
    theme_bw()
)


ggsave(filename = file.path(win.res, "fig", "dataLocations.png"),
       dat.map, device = "png",
       height = 4.875,
       width = 7.85,
       units = "in",
       dpi = 300)

#### Figure 2 ####
## predicted duration of winter

## prediction raster
dur.rast <- raster(file.path(win.res, "duration_p.tif"))
