#################################################
####  Analysis of body mass to body fat      ####
#### Authors: C. Reed Hranac, Liam McGuire   ####
#### Tested 13 June 2020                     ####
#################################################

## This script provides the analysis to relate body mass of pre-hibernating
## bats and their component body fat.
## includes plot generation
env.prior <- ls()


library(data.table); library(tidyverse)

##data
dat.clean <- fread("data/qmrCleaned.csv")
## filter outliers
dat.clean <- dat.clean %>%
  filter(fat > 0.85)
head(dat.clean)

## plot the data for quick visulizations
ggplot(data = dat.clean, ##mass
       aes(x= siteName, y = mass)) +
  geom_point()

ggplot(data = dat.clean, ##fat
       aes(x= siteName, y = fat)) +
  geom_point()

ggplot(data = dat.clean, ## lean
       aes(x= siteName, y = lean)) +
  geom_point()

ggplot(data = dat.clean, ## forearm
       aes(x= siteName, y = forearm)) +
  geom_point()

## proportional QMR mass
dat.clean  <- dat.clean %>%
  mutate(fl = fat + lean,
         flp = fl/mass)

##plot this
ggplot(data = dat.clean, ## flp
       aes(x= siteName, y = flp)) +
  geom_point()
## LCC data looks suspect af
plot(cor(dat.clean[,c(5:8,11:12)]))

## set factors
dat.clean$sex <- as.factor(dat.clean$sex)
dat.clean$state <- as.factor(dat.clean$state)

## create thesholding for
other <- dat.clean %>%
  filter(siteName != "LCC")
ggplot(data = other, ## ohter
       aes(x= siteName, y = flp)) +
  geom_point()

dat.1 <- dat.clean %>%
  filter(flp > .87, 
         flp < .96)

ggplot(data = dat.1, ##flp post clean
       aes(x= siteName, y = flp)) +
  geom_point()

## visualize with sex
ggplot(data = dat.clean, ##mass
       aes(x= siteName, y = mass, color = sex)) +
  geom_point()

ggplot(data = dat.clean, ##fat
       aes(x= siteName, y = fat, color = sex)) +
  geom_point()

ggplot(data = dat.clean, ## lean
       aes(x= siteName, y = lean, color = sex)) +
  geom_point()

ggplot(data = dat.clean, ## forearm
       aes(x= siteName, y = forearm, color = sex)) +
  geom_point()

#### modeling ####
fat.pred <- lm(fat ~ mass, dat.clean)
summary(fat.pred)

# Call:
#   lm(formula = fat ~ mass, data = dat.clean)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.73276 -0.17936  0.01594  0.20529  0.71562 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.83901    0.17643  -16.09   <2e-16 ***
#   mass         0.59675    0.02076   28.75   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2799 on 171 degrees of freedom
# Multiple R-squared:  0.8286,	Adjusted R-squared:  0.8276 
# F-statistic: 826.7 on 1 and 171 DF,  p-value: < 2.2e-16

#### plotting ####
state.lean.plot <- ggplot(data = dat.clean) +
  geom_boxplot(aes(x = state, y = lean, color = state))+
  ylab("Lean Mass (g)") + xlab("State") +
  theme_bw()

ggsave(filename = file.path(win.res, "fig", "stateLean.png"),
       state.lean.plot,
       width = 4,
       height = 4,
       units = "in",
       dpi = 300)

## linear prediction figure
fat.mass.plot <- ggplot(dat.clean) + 
  geom_point(aes(x = mass, y = fat, color = state), show.legend = F) + 
  geom_abline(aes(intercept = fat.pred$coefficients[[1]],
                  slope = fat.pred$coefficients[[2]])) +
  annotate("text",
           x=7.9, y = 3.5,
           label = paste0("Fat Mass = ",round(fat.pred$coefficients[[1]],2)," + ",
                          round(fat.pred$coefficients[[2]],2)," * Mass")) +
  xlab("Body Mass (g)") +
  ylab("Fat mass (g)") + 
  theme_bw()


ggsave(filename = file.path(win.res, "fig", "fatPrediction.png"),
       fat.mass.plot,
       width = 4,
       height = 4,
       units = "in",
       dpi = 300)

fat.plots <- gridExtra::grid.arrange(state.lean.plot, fat.mass.plot,
                                     nrow = 1)
ggsave(file.path(win.res, "fig", "siFatFig.png"),
       fat.plots,
       device = "png",
       width =7.5,
       height = 3.5,
       units = "in",
       dpi = 300)
#### Clean ####
#### Clean up script items ####
env.post <- ls()
to.remove <- env.post[env.post %!in% env.prior]
rm(list=to.remove); rm(env.post, to.remove)


