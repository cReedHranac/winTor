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


#### Figure 1 ####
## 2 maps, one with the duration data locations, one with fat location data

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


library(raster); library(rgdal)
## dem raster for extent and proj4
na.rast <- raster(file.path(win.dat, "NA_dem.tif"))
na.ext <- extent(na.rast)

mylu.dist <- readOGR(dsn = "D:/Dropbox/batwintor_aux/paramFiles/ShapeFiles", 
                     layer = "myotis_lucifugus")


(loc.plot <- ggplot(data = loc.full, aes(x= long, y = lat))+
  borders("world",
          xlim = na.ext[1:2], ylim = na.ext[3:4],
          color = "grey80",
          fill = "grey90") +
    geom_polygon(data = fortify(mylu.dist),
                 aes(long,lat, group = group),
                 colour = "dodgerblue4",
                 fill = NA,
                 inherit.aes = F) +
    geom_point(aes(shape = type),
               alpha = .5,
               size = 1.3,
               position = "jitter",
               show.legend=FALSE)+
    geom_point(data = as.data.frame(cbind(long= -111.007, lat  = 47.12429)),
               aes(x= long, y = lat),size = 2)+
    scale_color_manual(values=c("#E69F00","#56B4E9")) +
    coord_cartesian(xlim = na.ext[1:2], ylim = na.ext[3:4])+
    scale_x_continuous(limits = na.ext[1:2]) +
    theme_bw()+
    guides(color=guide_legend(title="Data Type"))

)

width_height <- diff(as.vector(na.ext))[c(1,3)]
aspect_map <- width_height[1] / width_height[2]

## Issue:
  ## points where mass and duration data exist will not jitter or alpha
  ## can not tell that there is both there. 

# 
# ggsave("fig/locationDataType.pdf",
#        loc.plot,
#        height = 7,
#        width = 7*aspect_map,
#        device = cairo_pdf,
#        dpi = 900)




ggsave("C:/Users/crhranac/Git/PhD-Thesis/Chapter4/Figs/locationDataType.pdf",
       loc.plot,
       height = 7,
       width = 7*aspect_map,
       device = cairo_pdf,
       dpi = 900)
