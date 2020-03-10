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
#### Data ####
## have modified the raw data from the initial sheet by seperating out the year
## from the site name, and removing some of metadata included in the site names
raw.dat <- fread(file.path(win.dat,"winAcousticsWCS_May2020.csv"))


## set up names
colnames(raw.dat)[[1]] <- "Location"

library(lubridate)
## set up some cleanings
dat.1 <- raw.dat %>%
  mutate(Night = as.Date(Night,
                          format = "%d/%m/%Y"),
         YR = as.factor(year(Night)),
         Week = week(Night)) %>%
  group_by(Location) %>%
  filter(n() > 2) %>% ## this helped to remove all the names without years
  group_by(Week) %>%
  mutate(sumWeek = sum(Number))%>%
  ungroup %>%
  mutate(DayofYear = lubridate::yday(Night),
         bi = 1) ## colapse to day of year so we can look across years

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

##split to front and back halves
dat.spring <- dat.1 %>%
  filter(Week <= 25)

dat.fall <- dat.1 %>%
  filter(Week > 25)

## creating tables to see how she looks
table(dat.fall$Location, dat.fall$YR)
table(dat.spring$Location, dat.spring$YR)

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
          if(any(weeks %!in% frame$Week)){
            missing <- weeks[weeks %!in% frame$Week]
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
          weeks <- seq(26,52)
          if(any(weeks %!in% frame$Week)){
            missing <- weeks[weeks %!in% frame$Week]
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


springHalf <- logWrapper(x = dat.spring)
fallHalf <- logWrapper(dat.fall)

## Group plots
(backend <- ggplot(data = b, aes(x = Week, y = fitted, fill = Location, color = YR)) + 
    geom_line()) 
(frontend <- ggplot(data = springHalf, aes(x = Week, y = fitted, fill = Location, color = YR)) +
    geom_line())

