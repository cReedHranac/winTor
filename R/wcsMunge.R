## Munge for WCS data

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
library(segmented)
#### Data ####
## have modified the raw data from the initial sheet by seperating out the year
## from the site name, and removing some of metadata included in the site names
raw.dat <- fread(file.path(win.dat,"winAcousticsWCS_May2020.csv"),
                 na.strings= "")

#### preliminary cleaning and munge ####
## set up names
colnames(raw.dat)[[1]] <- "Location"

library(lubridate)
## set up some cleanings
dat.1 <- raw.dat %>%
  filter(!is.na(Number)) %>%## remove dates with no numbers (idk why they exist)
  mutate(Night = as.Date(Night, ##format dates
                          format = "%d/%m/%Y"),
         YR = as.factor(year(Night)), ## pull years out
         Week = week(Night)) %>% ## potenital for fitting by week
  group_by(Location) %>%
  filter(n() > 2,## this helped to remove all the names without years
         !is.na(Night)) %>% ## remove number with no date (again idk why they exist)
  group_by(Week) %>%
  mutate(sumWeek = sum(Number))%>%
  ungroup %>%
  mutate(DayofYear = lubridate::yday(Night),
         bi = 1) %>%## colapse to day of year so we can look across years
  select(!starts_with("V")) %>% ## remove the 2 empty columns
  group_by(Night, Location) %>% ## fix the Nightly sum of calls column
  mutate(`Nightly Sum of all calls` = sum(Number)) %>%
  ungroup


## Create modified spring dates to see if that works
  ## idea <- work around so I can handel the date wrapping around 0 each year
dat.1$DayMod <- ifelse(dat.1$DayofYear <= 182, dat.1$DayofYear + 365, dat.1$DayofYear)


## Handling the Location name and year issue
## will not work in mutate statments for some reason
for(i in 1:nrow(dat.1)){
  
  name <- paste0(unlist(strsplit(dat.1$Location[i], " "))[-length(unlist(strsplit(dat.1$Location[i], " ")))],
                                                  collapse = " ")
  yr <- last(unlist(strsplit(dat.1$Location[i], " ")))
  dat.1$Location[i] <- name
  dat.1$YRnamed[i] <- yr
}
## setting as factor
dat.1$Location <- as.factor(dat.1$Location)

## maualy handeling the date issues
## ISSUE: years in the night column can still be messed up. shouldn't need to fix
## if I don't used that but stick to the YR column
levels(dat.1$YR)
dat.1$YR[which(dat.1$YR == 14)] <- 2014
dat.1$YR[which(dat.1$YR == 15)] <- 2015
dat.1 <- droplevels(dat.1)

## Data frame to work from 
a.dat <- dat.1

## prelim data visulization and checking for cleaning quality
# skim(a.dat)
# table(a.dat$Location)


##split to front and back halves
dat.spring <- a.dat %>%
  filter(DayofYear <= 182)

dat.fall <- a.dat %>%
  filter(DayofYear > 182)

## creating tables to see how she looks
# table(dat.fall$Location, dat.fall$YR)
# table(dat.spring$Location, dat.spring$YR)

#### Clceaning points ####
## must pass through .5

## Take median 

clean.site.years <- function(x){
  ##Function to clean by first 2 criteria
  df <- x %>%
    group_by(Location, YR) %>%
    filter(n() > 2) %>%
    ungroup %>%
    group_by(Location)
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
        dplyr::select(Location, YR, DayofYear, bi) 
      #remove duplicates if they exist
      frame <- frame[!duplicated(frame),]
      
      #create missing days
      if(nrow(frame)>0){
        if(max(frame$DayofYear) <= 182){
          days <- 1:182
          if(any(days %!in% frame$DayofYear)){
            missing <- days[days %!in% frame$DayofYear]
            missing.df <- as_tibble(cbind(unique(x$Location)[[i]], 
                                          levels(frame.loc$YR)[unique(frame.loc$YR)[[j]]],
                                          missing, 
                                          0))
            names(missing.df) <- names(frame)
            frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)  
          } else {
            fame.filled <- frame
          }
        } else{
          days <- 182:365
          if(any(days %!in% frame$DayofYear)){
            missing <- dayss[days %!in% frame$DayofYear]
            missing.df <- data.frame(cbind(unique(x$Location)[[i]], 
                                           levels(frame.loc$YR)[unique(frame.loc$YR)[[j]]],
                                           missing, 
                                           0))
            names(missing.df) <- names(frame)
            frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)
          } else {
            fame.filled <- frame
          }
        }
      }
      
      
      #Run the glm if the data for that year exists
      if(exists("frame.filled")){
        frame.filled$bi <- as.numeric(frame.filled$bi); frame.filled$DayofYear <- as.numeric(frame.filled$DayofYear)
        ##fit glm
        mod <- glm(bi ~ DayofYear, family = binomial(link = "logit"), data = frame.filled)
        ##fit break point
        fit.seg<-segmented(mod, seg.Z= ~DayofYear)
        summary(fit.seg) # gives the breakpoint
        plot(fit.seg)
        
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


springHalf <- logWrapper(x = dat.spring)
fallHalf <- logWrapper(dat.fall)

## Group plots
(backend <- ggplot(data = b, aes(x = Week, y = fitted, fill = Location, color = YR)) + 
    geom_line()) 
(frontend <- ggplot(data = springHalf, aes(x = Week, y = fitted, fill = Location, color = YR)) +
    geom_line())



### Testing some new ideas
##### idea 1: #####
## collapse all years in 1, 
##    Sum binary column and fit regression to that
a <- clean.site.years(a.dat)
x <- a

logReg.1 <- function(x){
  out <- list()
  out.est <- list()
  v <- 1
  for(i in 1:length(unique(x$Location))){
    frame.loc <- x %>%
      filter(Location == unique(x$Location)[[i]]) %>%
      dplyr::select(Location, YR, DayofYear, bi) %>%
      group_by(DayofYear) %>%
      mutate(biSum = n()) %>%
      ungroup()
    frame <- frame.loc[!duplicated(frame.loc),]
    
    
    ## create missing days for the annual analysis
    if(nrow(frame)>0){
      days <- 1:365
      if(any(days %!in% frame$DayofYear)){
        missing <- days[days %!in% frame$DayofYear]
        missing.df <- as_tibble(cbind(unique(x$Location)[[i]], 
                                      "fill",
                                      missing, 
                                      0, #bi
                                      0)) ##biSum
        names(missing.df) <- names(frame)
        frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)  
      } else {
        frame.filled <- frame
      }
    }
    
    
    
    ## create missing days
    if(nrow(frame)>0){
      days <- 1:365
      
      if(max(frame$DayofYear) <= 182){
        days <- 1:182
        if(any(days %!in% frame$DayofYear)){
          missing <- days[days %!in% frame$DayofYear]
          missing.df <- as_tibble(cbind(unique(x$Location)[[i]], 
                                        levels(frame.loc$YR)[unique(frame.loc$YR)[[j]]],
                                        missing, 
                                        0))
          names(missing.df) <- names(frame)
          frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)  
        } else {
          fame.filled <- frame
        }
      }
    
    
    #Run the glm if the data exists
      if(exists("frame.filled")){
        ## switch structure
        frame.filled[,3:5] <- sapply(frame.filled[,3:5], as.numeric)
          ##fit glm
        mod <- glm(bi ~ DayofYear, family = binomial(link = "logit"), data = frame.filled)#
        ##fit break point
        fit.seg<-segmented(mod, seg.Z= ~DayofYear)
        summary(fit.seg) # gives the breakpoint
        plot(fit.seg)
        
        str(fit.seg)
        
        # fitting curves
        mod.a <- augment(mod, newdata =  data.frame(days = days), type.predict = "response")
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


as.Date(paste(258,2015), format="%j %Y")

#### idea 2: ####
## collapse all years into one
## use date and sum of all calls to try to fit each half of the data set
  ## may need to fiddle with the range of dates avaliable to not fit double bends
a <- clean.site.years(dat.spring)
x <- a

logReg.2- list()
  out.est <- list()
  v <- 1
  for(i in 1:length(unique(x$Location))){
    frame.loc <- x %>%
      filter(Location == unique(x$Location)[[i]]) %>%
      dplyr::select(Location, YR, DayofYear, bi, `Nightly Sum of all calls`) %>%
      group_by(DayofYear) %>%
      mutate(biSum = n(),
             callSum = sum(`Nightly Sum of all calls`)) %>%
      ungroup()
    frame <- frame.loc[!duplicated(frame.loc),]
    
    ## spring compenent
    if(max(frame$DayofYear) <= 182 && nrow(frame)>0){
      ## create missing dates
      days <- 1:150
      if(any(days %!in% frame$DayofYear)){
        missing <- days[days %!in% frame$DayofYear]
        missing.df <- as_tibble(cbind(unique(x$Location)[[i]], 
                                      "fill",
                                      missing, 
                                      0, #bi
                                      0, #Nighly Sum
                                      0, #biSum
                                      0)) ##callSum
        names(missing.df) <- names(frame)
        frame.filled <- bind_rows(mutate_all(frame, as.character), missing.df)  
        
        ## fill after max date in calls
        max.in.frame <- max(frame$DayofYear)
        frame.filled$bi[frame.filled$DayofYear > max.in.frame] <- 1
        
      } else {
        frame.filled <- frame
      }
      
    }
    
      
      #Run the glm if the data exists
      if(exists("frame.filled")){
        ## switch structure
        frame.filled[,3:7] <- sapply(frame.filled[,3:7], as.numeric)
        ##fit glm on the binomial
        mod <- glm(bi ~ DayofYear, family = binomial(link = "logit"), data = frame.filled)#
        ##fit break point
        fit.seg<-segmented(mod, seg.Z= ~DayofYear)
        
        bi.est <- as.data.frame(cbind(fit.seg$psi[2], ## remove estimate
                        fit.seg$psi[3], ## se of est
                        fit.seg$aic))
        names(bi.est) <- c("bi.psi",
                           "bi.se",
                           "bi.aic")
        
        
        ##fit glm on the sumcalls
        mod <- glm(biSum ~ DayofYear, family = gaussian, data = frame.filled)#
        ##fit break point
        fit.seg<-segmented(mod, seg.Z= ~DayofYear)
        
        ga.est <- cbind(fit.seg$psi[2], ## remove estimate
                        fit.seg$psi[3], ## se of est
                        fit.seg$aic)
        plot(fit.seg)
        
        
        
        
        
        
        
        
        # fitting curves
        mod.a <- augment(mod, newdata =  data.frame(days = days), type.predict = "response")
        out[[v]] <- cbind(mod.a,
                          Location = unique(x$Location)[[i]],
                          YR = unique(frame.loc$YR)[[j]])
        # finding p=.5
        p <- 0.5
        est <- (log(p/(1-p)) - coef(mod.a)[1]/coef(mod.a)[2])
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
