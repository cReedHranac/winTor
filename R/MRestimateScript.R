#### Checking and testing parameters ####
library(tidyverse); library(data.table)
## libraries
library(lubridate); library(broom)
library(batwintor)


## Extra Paths
if (!exists('can.path')) {
  if(.Platform$"OS.type" == "windows"){
    can.path = file.path("D:", "Dropbox", "CanadaY3_2018")
  } else {
    can.path = "~/Dropbox/CanadaY3_2018"
  }
}

can.mr <- file.path(can.path, "Metabolics")
can.ar <- file.path(can.path, "Arousal")
can.mi <- file.path(can.path, "Microclimate")
can.res <- file.path(can.path,"Results")
if (!exists('win.path')) {
  if(.Platform$"OS.type" == "windows"){
    win.path = file.path("D:", "Dropbox", "winTor_aux")
  } else {
    win.path = "~/Dropbox/winTor_aux"
  }
}

win.dat <- file.path(win.path, "data")
win.res <- file.path(win.path, "Results")

if (!exists('serdp.path')) {
  if(.Platform$"OS.type" == "windows"){
    serdp.path = file.path("D:", "Dropbox", "SERDP")
  } else {
    serdp.path = "~/Dropbox/SERDP"
  }
}
serdp.src <- file.path(serdp.path, "data_source")
serdp.dat <- file.path(serdp.path, "data") 

## The %!in% opperator 
'%!in%' <- function(x,y)!('%in%'(x,y))


#### Looking at the differnet estimates
mylu.params
## This one looks really fishy

## Canadian est
  ##From workingScriptMetabolics for the CanadaY3 report
mylu.can.dat <- read.csv(file.path(can.mr, "myluMetabolics_2018_Cleaned.csv" ))
## set up variables again
mylu.can.dat$date <- as.Date(mylu.can.dat$date, format = "%d-%b-%y")
mylu.can.dat$site <- as.factor(mylu.can.dat$site)
mylu.can.dat$id <- as.factor(mylu.can.dat$id)
mylu.can.dat$Ta <- as.factor(mylu.can.dat$Ta)

## create estimates for each temperature (ignoring site for now)
(mylu.canada <- mylu.can.dat %>%
  group_by( Ta)%>%
  summarize(med.MR = median(VO2.ml.h.g),
            max.MR = max(VO2.ml.h.g),
            min.MR = min(VO2.ml.h.g)))

#### serdp estimates ####
serdp.MR <- read.csv(file.path(serdp.dat, "Respirometry_AllSites_US.csv"))
head(serdp.MR)
## set up variables
serdp.mylu <- serdp.MR %>%
  filter(spp == "mylu")
serdp.mylu$site <- droplevels(as.factor(serdp.mylu$site))
serdp.mylu$Ta <- as.factor(serdp.mylu$nom_temp)
serdp.mylu$id <- as.factor(serdp.mylu$batid)

serdp.est <- serdp.mylu %>%
  mutate(VO2.ml.h.g = mw* (0.1793/mass)) %>%
  dplyr::select(site, id, Ta, VO2.ml.h.g, mass, mlO2hr.g)

## Clean the data sets
## Step 1: split between sites
## Step 2: fit non-parametric models for each temperature
## Step 3: use posteriors to select which records are removed

#Strip out individuals that are clearly not in torpor (greater then)
# add site information
mylu.tor <- serdp.est %>%
  filter(VO2.ml.h.g < 1)

## check how many we still have
table(mylu.tor$site, mylu.tor$Ta)

# ## by removoing the far upper esimates we no longer need to take the internal log of the observations
spClean <- function(x){
  ##function for seperationg torpid/non-topid bats
  temps <- levels(as.factor(x$Ta))
  df.list <- list()
  v <- 1
  for(i in 1:length(unique(x$site))){
    for(j in 1:length(temps)){
      df.i.j <- x %>%
        dplyr::filter(site == levels(x$site)[[i]],
                      Ta == temps[[j]])

      spEM <- mixtools::spEMsymloc(df.i.j$VO2.ml.h.g,
                                   mu0=quantile(df.i.j$VO2.ml.h.g,
                                                c(.25,.75)),
                                   stochastic=TRUE)
      df.list[[v]] <- df.i.j %>%
        cbind(data.frame(d1 = spEM$posteriors[,1], d2 = spEM$posteriors[,2])) %>%
        dplyr::filter(d1 > 0.5)
      v <- v+1
    }
  }
  df.out <- do.call(rbind, df.list)
  return(df.out)
}

mylu.cleaned <- spClean(x= mylu.tor)
table(mylu.cleaned$site, mylu.cleaned$Ta)


ggplot(mylu.cleaned)+geom_histogram(aes(x=VO2.ml.h.g))+facet_wrap(~Ta +site, ncol = 2)

write.csv(mylu.cleaned,
          file.path(serdp.dat, "SERDP_myluMetabolics_2018_Cleaned.csv"),
          row.names = F)

serdp.MR <- read.csv(file.path(serdp.dat, "SERDP_myluMetabolics_2018_Cleaned.csv"))
serdp.MR$Ta <- as.factor(serdp.MR$Ta)
(mylu.serdp <- serdp.MR %>%
    group_by(Ta) %>%
    summarize(med.MR = median(VO2.ml.h.g),
              max.MR = max(VO2.ml.h.g),
              min.MR = min(VO2.ml.h.g)))
