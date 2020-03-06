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

## set up factors
colnames(raw.dat)[[1]] <- "Site"
for(i in 1:nrow(raw.dat)){
  raw.dat$Site[[i]] = as.factor(paste0(unlist(strsplit(raw.dat$Site[[i]], " "))[-length(unlist(strsplit(raw.dat$Site[[i]], " ")))],
                          collapse = " "))  
}

library(lubridate)
## set up some cleanings
dat.1 <- raw.dat %>%
  mutate(Night = as.Date(Night,
                          format = "%d/%m/%Y")) %>%
  group_by(Site) %>%
  filter(n() > 2) %>% ## this helped to remove all the names without years
  ungroup %>%
  mutate(Site = paste0(unlist(strsplit(Site, " "))[-length(unlist(strsplit(Site, " ")))],
                                 collapse = " ")) %>%
  mutate(YearN =  last(unlist(strsplit(Site, " "))),  ## Create a year of detection
         DayofYear = lubridate::yday(Night)) ## colapse to day of year so we can look across years

## plotting the data by nights
plot(dat.1$DayofYear, dat.1$Number)
skim(dat.1)
Site = as.factor(paste0(unlist(strsplit(raw.dat$Site[[1]], " "))[-length(unlist(strsplit(raw.dat$Site[[1]], " ")))],
                        collapse = " "))
