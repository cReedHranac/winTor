#### Fat expendature validation  ####
## Script to attemtp to create validation of the metabolic model


library(data.table); library(tidyverse)
library(batwintor)

#### Extra Paths####
if (!exists('base.path')) {
  if(.Platform$"OS.type" == "windows"){
    base.path = file.path("D:", "Dropbox", "wintor_aux")
  } else {
    base.path = "~/Dropbox/winTor_aux"
  }
}

win.dat <- file.path(base.path, "data")
win.res <- file.path(base.path, "Results")

## Load data for validation from literature
jon.dat <- fread(file.path(win.dat, "massData", "jonassonValidation.csv"))
jon.dat <- jon.dat %>%
  mutate(source = "jon")
fen.dat <- fread(file.path(win.dat, "massData", "fentonValidation.csv"))
fen.dat <- fen.dat %>%
  mutate(source = "fen")

##join
all.dat <- jon.dat %>%
  bind_rows(fen.dat) %>%
  select( -c(ID, Location))

all.sub <- all.dat %>%
  select(DiffDate, DiffMass, source)
all.sub <- rbind(all.sub,
                 c(as.numeric(128), as.numeric(.4129), "cori"))
all.sub[,1:2] <- sapply(all.sub[,1:2], as.numeric)
all.sub$source <- as.factor(all.sub$source)

## extract data from the mylu model
## Current model reuslts
mylu.mod <- fread(file.path(win.dat, "myluDynamicModel.csv"))

mylu.sub <- mylu.mod %>%
  filter(Ta %in% c(2, 4, 6, 8),
         pct.rh %in% seq(90, 100, by =2), 
         time %in% day.to.hour(unique(all.sub$DiffDate))) %>%
  mutate(DiffDate = hour.to.day(time),
         DiffMass = n.g.fat.consumed,
         Temp = as.factor(Ta),
         RH = as.factor(pct.rh))



#### Make some plots ####

(lit.plot <- ggplot() +
   ##literature poitns
   geom_point(data = all.sub,
              aes(x= DiffDate,
                  y= DiffMass,
                  shape = source)) + 
   ##model lines
   geom_line(data = mylu.sub,
             aes(x= DiffDate,
                 y = DiffMass,
                 linetype = Temp,
                 color = RH))+
   theme_bw()
 
)


