#### Figures Script ####
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
## The %!in% opperator 
'%!in%' <- function(x,y)!('%in%'(x,y))

library(tidyverse)
library(sf);library(rgdal);library(raster)

#### Figure 1 ####
## Duration Data (needs better names for these files)
dur.dat <- read.csv("data/durationUpdate.csv")
mass.dat <- read.csv("data/massLocations.csv")

## simplify for simplicity sake
dur.sub <- dur.dat %>%
  dplyr::select(lat, long)%>%
  mutate(type = "Duration")
mass.sub <- mass.dat %>%
  dplyr::select(Lat, Long) %>%
  mutate(type = "Mass")

colnames(mass.sub) <- c("lat", "long", "type")

loc.full <- rbind(dur.sub, mass.sub)

coordinates(loc.full) <- ~ long + lat
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(loc.full) <- CRS(wgs.84)
loc.utm <- spTransform(loc.full, 
                       CRS("+proj=utm +zone=11 +ellps=GRS80
                       +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
loc.sf <- st_as_sf(loc.utm)
## Background layers

North.America <- st_read(win.dat, layer="NorthAmerica")
NA.utm <- st_transform(North.America, 2955)

##mylu distribution
mylu.dist <- st_read("D:/Dropbox/batwintor_aux/paramFiles/ShapeFiles", 
                     layer = "myotis_lucifugus")

st_crs(mylu.dist) <- wgs.84
mylu.utm <- st_transform(mylu.dist, 2955)

rm(North.America, mylu.dist)

#### plot ####
loc.plot <- ggplot()+
  ##North American political boundries
  geom_sf(data = NA.utm,
          aes(group = "Name_1"),
          color="grey20",
          fill=NA) +
  ##Mylu distribution
  geom_sf(data = mylu.utm,
          aes(group = "SP_ID"),
          colour = "dodgerblue4",
          size = .7,
          fill = "dodgerblue1",
          alpha = .1) +
  ##fix the extent problems
  scale_x_continuous(limits =  c(extent(mylu.utm)[1],
                                 extent(mylu.utm)[2]))+
  scale_y_continuous(limits = c(extent(mylu.utm)[3],
                                extent(mylu.utm)[4])) +
  ##add the points
  geom_sf(data = loc.sf,
          aes(shape = type,
              color = type),
          alpha = .5,
          size = 2,
          show.legend = F)+
  scale_color_manual(values=c("#E69F00","#56B4E9")) +
  theme_bw()


ggsave(file.path(win.res,"fig/locationDataType.png"),
       loc.plot,
       height = 6,
       width = 8,
       units = "in",
       device = "png",
       dpi = 300)

