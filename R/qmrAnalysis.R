#### Lean Mass Annalysis ####

library(data.table); library(tidyverse)

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

#### Start modleing ####
lean.state.sex <- lm(lean~state*sex, dat.clean)
drop1(lean.state.sex)

