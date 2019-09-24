#### Lean Mass Annalysis ####

library(data.table); library(tidyverse)

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
## Serdp new data
serdp.newDat <- file.path("D:", "Dropbox", 
                          "SERDP", "data_source", "NewestData")

## The %!in% opperator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# ##Serdp data 
# qmr.dat <- fread("data/QMR_all.csv")
# 
# qmr.sub <- qmr.dat %>%
#   dplyr::filter(`Bat Species` == "Myotis lucifugus") %>%
#   separate(`Date Captured`,  c("d","m","y")) %>%
#   filter(m %in% c("09","10","11")) %>%
#   filter(Age == "Adult") %>%
#   dplyr::select(`Bat ID`, `Site ID`, `Age`, `Sex`, `Forearm Length`,
#                 `Mass (prior resp)`, `Before Fat`, `Before Lean`,
#                 `Before Free Water`, `Before Total Water`, m)
# 
# # standarize col names
# colnames(qmr.sub) <- c("batID", "siteName", "age", "sex", "forearm", "mass", "fat", "lean", 'freeWater',
#          "totalWater", "month")
# # add state
# qmr.sub$state <- "MT"
# 
# ## are there duplicates
# duplicated(qmr.sub$batID) #no
# 
# 
# ##Data from body condition paper
# bcp <- fread("data/QMR_Data_All_3Sept2017.csv")
# bcp$Date1 <- as.Date(bcp$Date1, "%d-%b-%y")
# 
# bcp.sub <- bcp %>%
#   filter(Species == "Myotis lucifugus",
#          !is.na(Fat)) %>% 
#   separate(Date1, c("y", "m", "d")) %>%
#   filter(m %in% c("09","10","11")) %>%
#   select(ID, `Site Name`, Age, Sex, Forearm,
#          Mass, Fat, Lean, Free_Water,
#          Total_Body_Water, m, State)
# 
# #standrize col names
# colnames(bcp.sub) <- c("batID", "siteName", "age", "sex", "forearm", "mass", "fat", "lean", 'freeWater',
#                        "totalWater", "month", "state")
# ## are there duplicates
# which(duplicated(bcp.sub$batID)) #none
# unique(bcp.sub$state)
# 
# ## Bind
# dat <- rbind(qmr.sub, bcp.sub)
# library(skimr)
# skim(dat)
# 
# ## Clean some more
# dat.clean <- dat %>%
#   mutate(age = substring(age, 1, 1), ## create consistent naming for these 
#          sex = substring(sex, 1, 1)) %>%
#   filter(age == "A",  #remove the sub adult
#          state != "ON") # remove ON because it only has 2 instances
# str(dat.clean)  
# 
# ##Checkpoint
# write.csv(dat.clean,
#           file = "data/qmrCleaned.csv", row.names = F)

dat.clean <- fread("data/qmrCleaned.csv")
## set factors
dat.clean$sex <- as.factor(dat.clean$sex)
dat.clean$state <- as.factor(dat.clean$state)

#### Start modleing ####

## Effect of sex

lean.sex <- lm(lean~sex, dat.clean)
summary(lean.sex)

lean.state.sex <- lm(lean~state*sex, dat.clean)
summary(lean.state.sex)
lean.state.sex1 <- update(lean.state.sex, .~. - state:sex)
summary(lean.state.sex1)

##hsd for differences 
library(multcomp)
ph <- glht(lean.state.sex1, linfct=mcp(state="Tukey"))
summary(ph)
## There is a significant differnce between Eastern and Western states in this instance

state.lean.plot <- ggplot(data = dat.clean) +
  geom_boxplot(aes(x = state, y = lean, color = state))+
  theme_bw()

# ggsave("fig/StateLean.pdf",
#        state.lean.plot)


## Predicting fat from body mass
fat.body <- lm(fat ~ mass, dat.clean)
summary(fat.body)

## Create plot

fat.mass.plot <- ggplot(dat.clean) + 
  geom_point(aes(x = mass, y = fat, color = state), show.legend = F) + 
  geom_abline(aes(intercept = fat.body$coefficients[[1]],
                  slope = fat.body$coefficients[[2]])) +
  annotate("text",
           x=7, y = 3.5,
           label = paste0("Fat Mass = ",round(fat.body$coefficients[[1]],2)," + ",
                          round(fat.body$coefficients[[2]],2)," * Mass")) +
  xlab("Mass (g)") +
  ylab("Fat mass (g)") + 
  theme_bw()


# ggsave("fig/FatMassLM.pdf",
#        fat.mass.plot)


fat.plots <- gridExtra::grid.arrange(state.lean.plot, fat.mass.plot,
                                     nrow = 1)
ggsave(file.path(win.res, "fig", "stateXfat.pdf"),
       fat.plots,
       device = cairo_pdf,
       width = 9,
       height = 4,
       units = "in")

## predict fat mass from our data
new.df <- data.frame(mass = dat.clean$mass)
dat.clean$pred.fat <- predict.lm(object = fat.body, newdata = new.df)

