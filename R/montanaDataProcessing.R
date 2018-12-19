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

library(tidyverse)
library(data.table)
library(skimr)
library(broom)
#### Data ####
m.dat <- fread("data/weeklyPasses_Species_Site.csv")
loc.m <- fread("data/LongtermSiteLocs.csv")

mylu.dat <- m.dat %>%
  filter(Species == "Mylu") %>%
  inner_join(. ,loc.m, by = "Location") %>%
  mutate(YR = as.factor(YR),
         bi = 1)

##split to front and back halves
dat.front <- mylu.dat %>%
  filter(Week <= 25)

dat.rear <- mylu.dat %>%
  filter(Week > 25)

## create logistic regression function for site and year 

logReg <- function(x){
  out <- list()
  v <- 1
  for(i in 1:length(unique(x$Location))){
    for(j in 1:length(unique(x$YR))){
      frame <- x %>%
        filter(Location == unique(x$Location)[[i]],
               YR == unique(x$YR)[[j]]) %>%
        dplyr::select(Location, YR, Week, bi) 
      #remove duplicates if they exist
      frame <- frame[!duplicated(frame),]
      
      #create missing weeks
      if(nrow(frame)>0){
        if(max(frame$Week) <= 25){
          weeks <- seq(1,25)
          missing <- weeks[weeks %!in% frame$Week]
          missing.df <- as.data.frame(cbind(unique(x$Location)[[i]], 
                                            levels(x$YR)[unique(x$YR)[[j]]],
                                            missing, 
                                            0))
          names(missing.df) <- names(frame)
          frame.filled <- rbind(frame, missing.df)
        } else{
          weeks <- seq(26,52)
          missing <- weeks[weeks %!in% frame$Week]
          missing.df <- as.data.frame(cbind(unique(x$Location)[[i]], 
                                            levels(x$YR)[unique(x$YR)[[j]]],
                                            missing, 
                                            0))
          names(missing.df) <- names(frame)
          frame.filled <- rbind(frame, missing.df)
        }
      }
      
      
      #Run the glm if the data for that year exists
      if(exists("frame.filled")){
        frame.filled$bi <- as.numeric(frame.filled$bi); frame.filled$Week <- as.numeric(frame.filled$Week)
        mod <- glm(bi ~ Week, family = binomial(link = "logit"), data = frame.filled)
        mod.a <- augment(mod, newdata =  data.frame(Week = weeks), type.predict = "response")
        out[[v]] <- cbind(mod.a, Location = unique(x$Location)[[i]], YR = unique(x$YR)[[j]])
        v <- v + 1 
      }
    }
  }
  out.df <- do.call(rbind, out)
  colnames(out.df) <- c("Week", "fitted", "se.fit", "Location", "YR")
  return(out.df)
  
}

a<- logReg(dat.rear)
b <- logReg(dat.front)


(backend <- ggplot(data = frame.x, aes(x = Week, y = fitted, fill = Location, color = YR)) + 
  geom_line()) 
(frontend <- ggplot(data = b, aes(x = Week, y = fitted, fill = Location, color = YR)) +
    geom_line())


loc.plot <- function(x, y){
  ## x is the return of logReg
  ## y is dataframe into log reg
  out <- list()
  v <- 1
  for(i in 1:length(unique(x$Location))){
    frame.x <-  x %>%
      filter(Location == unique(x$Location)[[i]]) 
    frame.y <-  y %>%
      filter(Location == unique(x$Location)[[i]])
    frame.y <- frame.y[!duplicated(frame.y),]
    out[[v]] <-  ggplot() + 
      geom_bar(data = frame.y,
               aes(x = Week, y = bi, fill = YR), stat = "identity")+
      geom_line(data = frame.x,
                aes(x = Week, y = fitted, color = YR)) + 
      scale_x_continuous(breaks = seq(min(x$Week), max(x$Week)),
                         limits = c(min(x$Week), max(x$Week)))+
      theme_bw() 
    
    
  }

}








(date.hist <- ggplot(data = toy, ) + 
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
