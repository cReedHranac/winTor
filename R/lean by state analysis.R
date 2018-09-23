####################################
# load and cleanup SERDP data 
####################################
qmr <- read.csv("C:\\Users\\Liam\\OneDrive\\Liam\\Project Files\\2018\\SERDP\\QMR2.csv",header=T)
var.names <-tolower(colnames(qmr))
colnames(qmr)<-var.names

qmr$body.mass <- qmr$mass..prior.resp.
qmr <- subset(qmr, select=c(-mass..prior.resp.))
qmr$species <- qmr$bat.species
qmr <- subset(qmr, select=c(-bat.species))
qmr$forearm <- qmr$forearm.length
qmr <- subset(qmr, select=c(-forearm.length))

qmr <- subset(qmr, suspect == 0)
qmr <- subset(qmr, before.after.respirometry == "Before")
qmr <- droplevels(qmr)
qmr <- qmr[!duplicated(qmr$bat.id),]
plot(bat.id~1,qmr)

################################
### load and clean up data from body condition paper
################################

all.data <- read.csv("C:\\Users\\Liam\\OneDrive\\R\\QMR Body Comp w Lewis\\QMR_Data_All_3Sept2017.csv",header=T)
head(all.data)
str(all.data)
all.data <- subset(all.data, Fat != "NA")

mylu <- subset(all.data, Species == "Myotis lucifugus")

################################
### now merge the two and start working with it
################################

names(mylu) # from body condition paper, just mylu
head(mylu)
mylu <- subset(mylu, select=c(Date1, Sex, Age, Mass, Forearm, Fat, Lean, Total_Body_Water, Season, Repro.Status, State, Site.Name))
var.names <-tolower(colnames(mylu))
colnames(mylu)<-var.names
names(mylu)
mylu$date <- mylu$date1
mylu <- subset(mylu, select=c(-date1))
mylu <- droplevels(mylu)
str(mylu)
mylu$date <- as.Date(mylu$date, format = "%d-%b-%y")
tapply(mylu$mass, mylu$state, length)

names(qmr) # from SERDP, all species
serdpmylu <- subset(qmr, species == "Myotis lucifugus")
serdpmylu <- subset(qmr, species=="Myotis lucifugus")
serdpmylu <- subset(serdpmylu, select=c(date.captured,sex, age, body.mass, forearm, fat, lean, totalwater, season, reproductive.condition, site.id))
serdpmylu <- droplevels(serdpmylu)
levels(serdpmylu$site.id)
tapply(serdpmylu$body.mass, serdpmylu$site.id, length)
serdpmylu <- subset(serdpmylu, site.id == "LCC")
serdpmylu <- droplevels(serdpmylu)
levels(serdpmylu$site.id)
serdpmylu$state <- "Montana"
serdpmylu$date <- serdpmylu$date.captured
serdpmylu <- subset(serdpmylu, select=c(-date.captured))
str(serdpmylu)
serdpmylu$date <- as.Date(serdpmylu$date, format = "%m/%d/%Y")
str(serdpmylu)

names(mylu)
names(serdpmylu)

serdpmylu$mass <- serdpmylu$body.mass
serdpmylu$total_body_water <- serdpmylu$totalwater
serdpmylu$repro.status <- serdpmylu$reproductive.condition
serdpmylu$site.name <- serdpmylu$site.id

serdpmylu <- subset(serdpmylu, select = c(-body.mass, -totalwater, -reproductive.condition, -site.id))

names(mylu)
names(serdpmylu)

allmylu <- rbind(mylu, serdpmylu)
str(allmylu)
levels(allmylu$season)
allmylu$month <- months(allmylu$date)

mallmylu <- subset(allmylu, sex == "M")
	tapply(mallmylu$mass, mallmylu$sex, length)
maleallmylu <- subset(allmylu, sex == "Male")
	maleallmylu$sex <- "M"
	tapply(maleallmylu$mass, maleallmylu$sex, length)
fallmylu <- subset(allmylu, sex == "F")
	tapply(fallmylu$mass, fallmylu$sex, length)
femaleallmylu <- subset(allmylu, sex == "Female")
	femaleallmylu$sex <- "F"
	tapply(femaleallmylu$mass, femaleallmylu$sex, length)
allmylu <- rbind(mallmylu, maleallmylu, fallmylu, femaleallmylu)
allmylu <- droplevels(allmylu)
str(allmylu)
tapply(allmylu$mass, allmylu$sex, length)

par(mfrow=c(1,1))
hist(allmylu$lean) # some of those values are surely machine error. No way anything other than a neonate only has 2-3g lean mass
tapply(allmylu$lean, allmylu$season, length)
tapply(allmylu$lean, allmylu$month, length)

allmylu$pqmr <- (allmylu$lean + allmylu$fat)/allmylu$mass #qmr measures everything that isn't fur, teeth, skeleton. So sum of fat and lean mass should be a pretty consistent proportion of total body mass
hist(allmylu$pqmr) # indicates that qmr data accounts for as little as 40%, and sometimes >100%, of total body mass. Again, surely not realistic

allmylu <- subset(allmylu, pqmr > 0.7 & pqmr < 1)
hist(allmylu$pqmr) # better, but still seems to be an unusual small second peak around 0.75
allmylu <- subset(allmylu, pqmr > 0.8 & pqmr < 1)
hist(allmylu$pqmr) # looks pretty good now

hist(allmylu$lean) # I buy this.


tapply(allmylu$lean, list(allmylu$state, allmylu$month),length) 
# many of those datapoints are from mid to late winter in areas affect by WNS (Ontario, NY, Vermont). The lean mass values in those cases may not be representative of healthy fall lean mass

fallmylu <- subset(allmylu, month != "April" & month != "February" & month != "March")
tapply(fallmylu$lean, list(fallmylu$state, fallmylu$month),length)

# Ontario is left with only 7 bats. Cut.

fallmylu <- subset(fallmylu, state != "ON")
fallmylu <- droplevels(fallmylu)
tapply(fallmylu$lean, list(fallmylu$state, fallmylu$month),length)


str(fallmylu)
fallmylu$month <- as.factor(fallmylu$month)
str(fallmylu)
m0 <- lm(lean~state*sex, fallmylu)
drop1(m0, test="F")
m1 <- update(m0, .~. - state:sex)
drop1(m1, test = "F")
summary(m1)

# males have less lean mass than females (by about 0.2 g)

library(multcomp)
ph <- glht(m1, linfct=mcp(state="Tukey"))
summary(ph)

# Montana bats have less lean mass than NY or VT bats, by about 0.4 or 0.5g

par(mfrow=c(1,2))
plot(lean~state, fallmylu, ylab="Lean mass (g)")

plot(fallmylu$lean, ylab= "Lean mass (g)")
rect(-10,5.176, 150, 5.899, col="light grey", lty=0)
points(fallmylu$lean,pch=16)
abline(h=5.6,lwd=2)

par(mfrow=c(2,2), cex.lab=1.5, cex.axis=1.5, mar=c(5,5,4,2)+0.1)
plot(lean~mass, fallmylu,pch=16, ylab="Lean mass (g)", xlab= "Body mass (g)")
	m0 <- lm(lean~mass, fallmylu)
	summary(m0)
	abline(2.4884, 0.3615,lwd=2)
	text(8,6.75,"lean = 2.4884 + 0.3615*mass",cex=1.2)
	text(8,6.55,"line = fit line",cex=1.2)
fallmylu$predlean <- 2.4884+0.3615*fallmylu$mass
plot(predlean~lean, fallmylu,ylab="Predicted lean mass (g)", xlab = "Lean mass (g)",pch=16)
	abline(0,1,lwd=2)
	text(5,6.5,"line = equality",cex=1.2)
fallmylu$leanpredfat <- fallmylu$mass - fallmylu$predlean
plot(leanpredfat ~ fat, fallmylu, pch=16, ylab="Predicted fat mass (g)", xlab = "Fat mass (g)")
	summary(lm(leanpredfat~fat,fallmylu))
	abline(0,1,lwd=2)
	text(1.75, 4.5, "fat predicted from lean",cex=1.2)
	text(1.75,4.25, "line = equality",cex=1.2)

par(mfrow=c(1,1), cex.lab=1.5, cex.axis=1.5, mar=c(5,5,4,2)+0.1)

par(mfrow=c(1,3), cex.lab=1.5, cex.axis=1.5, mar=c(5,5,4,2)+0.1)
plot(fat~mass, fallmylu,pch=16, ylab="Fat mass (g)", xlab="Body mass (g)")
	summary(lm(fat~mass, fallmylu))
	abline(-2.94895,0.61117,lwd=2)
	text(8,3.75,"fat = -2.94895 + 0.61117*mass",cex=1.2)
	text(7.5,3.5, "line = fit line",cex=1.2)
fallmylu$fatpredfat <- -2.94895+0.61117*fallmylu$mass
plot(fatpredfat~fat, fallmylu, pch=16, ylab="Predicted fat mass (g)", xlab = "Fat mass (g)")
	abline(0,1, lwd=2)
	text(1.95,3.5, "line = equality",cex=1.2)
	text(1.95,3.75, "dashed lines = equality ? 0.5g",cex=1.2)
	abline(0.5,1, lwd=2,lty=2)
	abline(-0.5,1, lwd=2,lty=2)
plot((mass*0.3)~fat, fallmylu, pch=16, ylab="Body mass * 0.3", xlab = "Fat mass (g)")
	abline(0,1, lwd=2)
	text(1.95,3.25, "line = equality",cex=1.2)
	text(1.95,3.75, "dashed lines = equality ? 0.5g",cex=1.2)


## checking to see if including sex in the predictive model helps. Not much.
m0 <- lm(fat~mass + sex, fallmylu)
summary(m0)

mfallmylu <- subset(fallmylu, sex == "M")
ffallmylu <- subset(fallmylu, sex == "F")
mfallmylu$sexpredfat <- -3.07825 + 0.09680 + 0.61877*mfallmylu$mass
ffallmylu$sexpredfat <- -3.07825 + 0.61877*ffallmylu$mass
fallmylu <- rbind(mfallmylu, ffallmylu)

plot(sexpredfat~fat, fallmylu, pch=16, ylab="Predicted fat mass (g)", xlab = "Fat mass (g)")
	abline(0,1, lwd=2)
	text(1.95,3.5, "line = equality",cex=1.2)
	text(1.95,3.75, "dashed lines = equality ? 0.5g",cex=1.2)
	abline(0.5,1, lwd=2,lty=2)
	abline(-0.5,1, lwd=2,lty=2)

