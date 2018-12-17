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



library(tidyverse)
library(data.table)
m.dat <- fread("data/weeklyPasses_Species_Site.csv")
loc.m <- fread("data/LongtermSiteLocs.csv")

mylu.dat <- m.dat %>%
  filter(Species == "Mylu") %>%
  inner_join(. ,loc.m, by = "Location") %>%
  mutate(YR = as.factor(YR),
         Week = as.factor(Week))


(date.hist <- ggplot(data = toy, aes(x = Week, y = AvgPasses_perNight, fill = Location)) + 
  geom_bar( stat = "identity") + guides(fill=FALSE)+
  facet_grid(~YR))


toy <- mylu.dat %>%
  filter(Location =="Signal Peak Mine Busse Water Reservoir")

top.dat <- mylu.dat %>%
  group_by(Location) %>%
  mutate(n.row = n()) %>%
  top_n(5, wt = n.row)

max(top.dat$n.row)
top.dat[top.dat["n.row"==188],]
a <-  top.dat[ top.dat$n.row == 188, ]

## See how long our locations go for in the mod
library(raster)
winter <- raster(file.path(win.res, "durationRaster_p.tif"))

coordinates(mylu.dat) <- ~ Longitude + Latitude
proj4string(mylu.dat) <- proj4string(winter)

win.length <- extract(winter, mylu.dat)
summary(win.length)



library(mapview)
mapview(mylu.dat)
