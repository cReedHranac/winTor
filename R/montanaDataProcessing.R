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

## tables 
#Create tables to see which data exists where prior to cleaning methods
table(dat.front$Location, dat.front$YR)
table(dat.rear$Location, dat.rear$YR)

#### Clceaning points ####
## must have at least 3 points in one each year,
## must have at least 2 years,
## must pass through .5

## Take median 

clean.site.years <- function(x){
  ##Function to clean by first 2 criteria
  df <- x %>%
    group_by(Location, YR) %>%
    filter(n() > 2) %>%
    ungroup %>%
    group_by(Location) %>%
    filter(any(length(unique(YR)) >= 2))
  return(df)
}

## create logistic regression function for site and year 

logReg <- function(x){
  out <- list()
  out.est <- list()
  v <- 1
  for(i in 1:length(unique(x$Location))){
    frame.loc <- x %>%
      filter(Location == unique(x$Location)[[i]])
    for(j in 1:length(unique(frame.loc$YR))){
      frame <- frame.loc %>%
        filter(YR == unique(frame.loc$YR)[[j]]) %>%
        dplyr::select(Location, YR, Week, bi) 
      #remove duplicates if they exist
      frame <- frame[!duplicated(frame),]
      
      #create missing weeks
      if(nrow(frame)>0){
        if(max(frame$Week) <= 25){
          weeks <- seq(1,25)
          missing <- weeks[weeks %!in% frame$Week]
          missing.df <- as_tibble(cbind(unique(x$Location)[[i]], 
                                            levels(frame.loc$YR)[unique(frame.loc$YR)[[j]]],
                                            missing, 
                                            0))
          names(missing.df) <- names(frame)
          frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)
        } else{
          weeks <- seq(26,52)
          missing <- weeks[weeks %!in% frame$Week]
          missing.df <- as_tibble(cbind(unique(x$Location)[[i]], 
                                        levels(frame.loc$YR)[unique(frame.loc$YR)[[j]]],
                                        missing, 
                                        0))
          names(missing.df) <- names(frame)
          frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)
        }
      }
      
      
      #Run the glm if the data for that year exists
      if(exists("frame.filled")){
        frame.filled$bi <- as.numeric(frame.filled$bi); frame.filled$Week <- as.numeric(frame.filled$Week)
        mod <- glm(bi ~ Week, family = binomial(link = "logit"), data = frame.filled)
        # fitting curves
        mod.a <- augment(mod, newdata =  data.frame(Week = weeks), type.predict = "response")
        out[[v]] <- cbind(mod.a,
                          Location = unique(x$Location)[[i]],
                          YR = unique(frame.loc$YR)[[j]])
        # finding p=.5
        p <- 0.5
        est <- (log(p/(1-p)) - coef(mod)[1]/coef(mod)[2])
        out.est[[v]] <- cbind(Location = unique(x$Location)[[i]],
                              YR = levels(frame.loc$YR)[unique(frame.loc$YR)[[j]]],
                              est = est)
        v <- v + 1 
      }
    }
  }
  out.df <- do.call(rbind, out)
  out.est.df <- do.call(rbind, out.est)
  colnames(out.df) <- c("Week", "fitted", "se.fit", "Location", "YR")
  ## Fiter for those not within range and without the requirements mentioned above
  est.df <- as.data.frame(out.est.df, row.names = NULL)
  rownames(est.df) <- NULL; est.df$est <- as.numeric(est.df$est)
  out.est.df <- est.df %>%
    group_by(Location, YR) %>%
    filter(est %in% weeks) %>%
    ungroup %>%
    group_by(Location) %>%
    # filter(any(length(unique(YR)) >= 2)) %>%
    summarise(est.med = median(est))
  
  
  out.list <- list(regression.df = out.df, 
                   regression.est = out.est.df)
  return(out.list)
  
}
 logRegSite <- function(x){
  ## same as above function without the year breakout for site averages
  out <- list()
  v <- 1
  for(i in 1:length(unique(x$Location))){
      frame <- x %>%
        filter(Location == unique(x$Location)[[i]]) %>%
        dplyr::select(Location, Week, bi) 
      #remove duplicates if they exist
      frame <- frame[!duplicated(frame),]
      
      #create missing weeks
      if(nrow(frame)>0){
        if(max(frame$Week) <= 25){
          weeks <- seq(1,25)
          missing <- weeks[weeks %!in% frame$Week]
          missing.df <- as_tibble(cbind(unique(x$Location)[[i]], 
                                        levels(frame.loc$YR)[unique(frame.loc$YR)[[j]]],
                                        missing, 
                                        0))
          names(missing.df) <- names(frame)
          frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)
        } else{
          weeks <- seq(26,52)
          missing <- weeks[weeks %!in% frame$Week]
          missing.df <- as_tibble(cbind(unique(x$Location)[[i]], 
                                        levels(frame.loc$YR)[unique(frame.loc$YR)[[j]]],
                                        missing, 
                                        0))
          names(missing.df) <- names(frame)
          frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)
        }
      }
      
      
      #Run the glm if the data for that year exists
      if(exists("frame.filled")){
        frame.filled$bi <- as.numeric(frame.filled$bi); frame.filled$Week <- as.numeric(frame.filled$Week)
        mod <- glm(bi ~ Week, family = binomial(link = "logit"), data = frame.filled)
        mod.a <- augment(mod, newdata =  data.frame(Week = weeks), type.predict = "response")
        out[[v]] <- cbind(mod.a, Location = unique(x$Location)[[i]])
        v <- v + 1 
    }
  }
  out.df <- do.call(rbind, out)
  colnames(out.df) <- c("Week", "fitted", "se.fit", "Location")
  return(out.df)
}
loc.plot <- function(x, y, z = NULL){
  ## x is dataframe into logReg
  ## y is the return of logReg
  ## z is the return of logRegSite
  out <- list()
  v <- 1
  for(i in 1:length(unique(x$Location))){
    frame.x <-  x %>%
      filter(Location == unique(x$Location)[[i]]) 
    frame.y <-  y %>%
      filter(Location == unique(x$Location)[[i]])
    frame.y <- frame.y[!duplicated(frame.y),]
    
    out[[v]] <-  ggplot() + 
      geom_bar(data = frame.x,
               aes(x = Week, y = bi, fill = YR), stat = "identity", alpha = .5)+
      geom_line(data = frame.y,
                aes(x = Week, y = fitted, color = YR), size = 1.5) +
      scale_x_continuous(breaks = seq(min(y$Week), max(y$Week)),
                         limits = c(min(y$Week), max(y$Week)))+
      ggtitle(unique(x$Location)[[i]])+
      theme_bw() 
    if(!is.null(z)){
      frame.z <- z %>%
        filter(Location == unique(x$Location)[[i]])
      frame.z <- frame.z[!duplicated(frame.z),]
      out[[v]] <- out[[v]] +geom_line(data = frame.z, 
                aes(x = Week, y = fitted), size = 1.25) 
    }
    v <- v+1
  }
  return(out)
}
logWrapper <- function(x){
  ## clean input 
  x.cleaned <- clean.site.years(x)
  
  ## run the regressions
  regSiteYear <- logReg(x)
  #regSite <- logRegSite(x.cleaned)
    
  ## create location plots
  locationPlots <- loc.plot(x= x.cleaned,
                            y= regSiteYear$regression.df)
  
  out <- list(cleaned_Site_Year = x.cleaned,
              fitted_Site_Year = regSiteYear$regression.df,
              #fitted_Site = regSite,
              plot_Location = locationPlots,
              estimate.df = regSiteYear$regression.est)
  return(out)
}


frontHalf <- logWrapper(dat.front)
backHalf <- logWrapper(dat.rear)

## Group plots
(backend <- ggplot(data = b, aes(x = Week, y = fitted, fill = Location, color = YR)) + 
  geom_line()) 
(frontend <- ggplot(data = a, aes(x = Week, y = fitted, fill = Location, color = YR)) +
    geom_line())

## Putting together which appear in both sets 
comp.est <- inner_join(backHalf$estimate.df,
                       frontHalf$estimate.df,
                       by = "Location") %>%
  inner_join(., loc.m,
             by = "Location") %>% 
  dplyr::select(Location, est.med.x, est.med.y, Latitude, Longitude) %>%
  distinct %>%
  mutate(start.est = as.Date(paste(2018, trunc(est.med.x), 1, sep = "-"), "%Y-%m-%d"),
         end.est = as.Date(paste(2019, trunc(est.med.y), 1, sep = "-"), "%Y-%m-%d") ,
         duration = as.numeric( end.est - start.est))


## See how long our locations go for in the mod
library(raster)
winter <- raster(file.path(win.res, "durationRaster_p.tif"))

coordinates(comp.est) <- ~ Longitude + Latitude
proj4string(comp.est) <- proj4string(winter)

comp.est$duration.p <- extract(winter, comp.est)
comp.est$duration.diff <- comp.est$duration.p - comp.est$duration
## These are looking real shit
## fuck it we're going to try them any ways

## out dataframe should be
## id lat long Start End winter.duration

df.out <- bind_cols(ID = paste0("MTHP", 1:nrow(comp.est)),
                lat = comp.est$Latitude,
                long = comp.est$Longitude,
                Start = as.Date(comp.est$start.est,"%Y-%m-%d"),
                End = as.Date(comp.est$end.est, "%Y-%m-%d"),
                winter.duration = comp.est$duration)
write.csv(df.out, "data/MTHP_durationCleaned.csv", row.names = F)
