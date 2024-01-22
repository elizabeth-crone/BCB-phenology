library(quantreg)
library(MASS)
library(plotrix)


#### read in and format transect data ####

#read in transect data
#Transects=read.csv(file="/Users/junearriens/Dropbox/BCB Nectar/Field Data/nectar field data as csv/transects.csv",header=TRUE, dec=".")
Transects=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/Field Data/nectar field data as csv/transects.csv",header=TRUE, dec=".")

Transects$Site = as.factor(Transects$Site)
Transects$Date = as.factor(Transects$Date)
Transects$PlantID = as.factor(Transects$PlantID)


#check survey dates
table(Transects$Date, Transects$Site)

#change individual surveys that were done over a few days into one date
Transects[Transects$Date=="6/22/19",]$Date="6/23/19"
Transects[Transects$Date=="6/17/19",]$Date="6/18/19"
Transects[Transects$Date=="6/19/19",]$Date="6/18/19"
Transects[Transects$Date=="6/10/19",]$Date="6/12/19"
Transects[Transects$Date=="6/26/19",]$Date="6/24/19"

#remove transects that didn't have flowers
Transects=Transects[!is.na(Transects$Count),]

#change ones where species is listed as "NA" to "unknown"
Transects$PlantID = factor(Transects$PlantID, levels = c(levels(Transects$PlantID), "Unknown"))
Transects[is.na(Transects$PlantID),]$PlantID="Unknown"


## set species count to 0 if it's not in the transect ##

#set up a data frame with all possible combinations of factors
flowerdata=expand.grid(Site1 = levels(Transects$Site), Date1 = levels(Transects$Date), PlantID1 = levels(Transects$PlantID))

#limit it to site and date combos that exist in the data
Transects$dateID=paste(Transects$Site, Transects$Date)
flowerdata$dateID=paste(flowerdata$Site1, flowerdata$Date1)
flowerdata$dateID=factor(flowerdata$dateID,levels=c(levels(as.factor(Transects$dateID))))
flowerdata=flowerdata[!is.na(flowerdata$dateID),]

#also limit it to site and species combos that exist in the data
Transects$spID=paste(Transects$Site, Transects$PlantID)
flowerdata$spID=paste(flowerdata$Site1, flowerdata$PlantID1)
flowerdata$spID=factor(flowerdata$spID,levels=c(levels(as.factor(Transects$spID))))
flowerdata=flowerdata[!is.na(flowerdata$spID),]

#merge to transects data
flowerdata$code=paste(flowerdata$Site1, flowerdata$Date1, flowerdata$PlantID1)
Transects$code=paste(Transects$Site, Transects$Date, Transects$PlantID)
Transects2=merge(Transects,flowerdata[c("Site1","Date1","PlantID1","code")], by="code", all.y=TRUE )
Transects2[is.na(Transects2$Site),]$Site=Transects2[is.na(Transects2$Site),]$Site1
Transects2[is.na(Transects2$PlantID),]$PlantID=Transects2[is.na(Transects2$PlantID),]$PlantID1
Transects2[is.na(Transects2$Date),]$Date=Transects2[is.na(Transects2$Date),]$Date1

#set count to 0 for all times that the plant wasn't seen
Transects2[is.na(Transects2$Count),]$Count=0




## calculate flowering units per m2 ##

#add up flowering units per species per date
Transects2$code=paste(Transects2$Site, Transects2$Date, Transects2$PlantID)
DailySums=aggregate(list(Total=Transects2$Count), Transects2['code'], sum, na.rm=TRUE)
DailySums=merge(Transects2[c("Site","Date","PlantID","code")], DailySums, by="code")
DailySums=DailySums[!duplicated(DailySums$code),]

#divide total by area of a transect (20 m2) x number of transects
#gives the number per m2 
#account for first Appleton survey missing a transect
DailySums$Density=DailySums$Total
DailySums[DailySums$Site=="Alesia" | DailySums$Site=="Harvard" | DailySums$Site=="Nuetzel" | DailySums$Site=="Upton",]$Density=DailySums[DailySums$Site=="Alesia" | DailySums$Site=="Harvard" | DailySums$Site=="Nuetzel" | DailySums$Site=="Upton",]$Total/(20*10)
DailySums[DailySums$Site=="Appleton" & DailySums$Date=="2019-06-23",]$Density=DailySums[DailySums$Site=="Appleton" & DailySums$Date=="2019-06-23",]$Total/(20*9)
DailySums[DailySums$Site=="Appleton" & DailySums$Date!="2019-06-23",]$Density=DailySums[DailySums$Site=="Appleton" & DailySums$Date!="2019-06-23",,]$Total/(20*10)
DailySums[DailySums$Site=="Appleton2",]$Density=DailySums[DailySums$Site=="Appleton2",]$Total/(20*1)
DailySums[DailySums$Site=="EdgewoodAPG",]$Density=DailySums[DailySums$Site=="EdgewoodAPG",]$Total/(20*7)
DailySums[DailySums$Site=="McElwain",]$Density=DailySums[DailySums$Site=="McElwain",]$Total/(20*9)
DailySums[DailySums$Site=="SmallAPG",]$Density=DailySums[DailySums$Site=="SmallAPG",]$Total/(20*2)

# adding a column for sampled area
DailySums$Area = NA
DailySums[DailySums$Site=="Alesia" | DailySums$Site=="Harvard" | DailySums$Site=="Nuetzel" | DailySums$Site=="Upton",]$Area = (20*10)
DailySums[DailySums$Site=="Appleton" & DailySums$Date=="2019-06-23",]$Area = (20*9)
DailySums[DailySums$Site=="Appleton" & DailySums$Date!="2019-06-23",]$Area=(20*10)
DailySums[DailySums$Site=="Appleton2",]$Area = (20*1)
DailySums[DailySums$Site=="EdgewoodAPG",]$Area = (20*7)
DailySums[DailySums$Site=="McElwain",]$Area = (20*9)
DailySums[DailySums$Site=="SmallAPG",]$Area = (20*2)


head(DailySums)

## formatting ##

#put date into date format 
DailySums$Date <- as.Date(DailySums$Date,format = "%m/%d/%y")

#add day of year column
DailySums$DOY=as.numeric(strftime(DailySums$Date, format = "%j"))

#label each site and plant combo
DailySums$Var1=paste(DailySums$Site,DailySums$PlantID )

head(DailySums)

unique(DailySums$Site)
tmp.states = c("MD", "MA", "MA", "MD", "MA", "MD", "MD", "MD", "MA")
state.dat = data.frame(Site = unique(DailySums$Site), State = tmp.states)

DailySums = merge(DailySums, state.dat)
head(DailySums)

# note - I tried adding additional data from 2017 here, and it didn't add much - too few observations of these plant species
# since the methods were somewhat different, I chose to take this out of the study

#### read in and format bcb field data ####

#read in bcb data
BCB=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Fieldwork 2018-2019/Baltimore checkerspots/All Baltimore checkerspot data/BCB data as csv/all_adults.csv",header=TRUE, dec=".")

head(BCB)
names(BCB)[1] = "Date"

#add up butterflies per date
BCB$code=paste(BCB$Site, BCB$Date)
BCB$Count=1
DailyBCBSums=aggregate(list(Total=BCB$Count), BCB['code'], sum, na.rm=TRUE)
DailyBCBSums=merge(DailyBCBSums, BCB[c("State","Site","Date","code","Year" )], by="code")
DailyBCBSums=DailyBCBSums[!duplicated(DailyBCBSums$code),]

#limit it to 2019 and remove Appleton2
#DailyBCBSums=DailyBCBSums[DailyBCBSums$Year=="2019",]
DailyBCBSums=DailyBCBSums[DailyBCBSums$Site!="Appleton2",]

#remove empty site levels
DailyBCBSums$Site = as.character(DailyBCBSums$Site)
DailyBCBSums$Site = as.factor(DailyBCBSums$Site)

#put date into date format 
DailyBCBSums$Date <- as.Date(DailyBCBSums$Date,format = "%m/%d/%y")

#add day of year column
DailyBCBSums$DOY=as.numeric(strftime(DailyBCBSums$Date, format = "%j"))
DailyBCBSums$SiteYear = paste(DailyBCBSums$Site, DailyBCBSums$Year)

#### read in herbarium and specimen data ####

## herbarium data ##
herbarium=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/JA Analyses/herbarium formatted.csv",header=TRUE, dec=".")

## bcb data ##
bcb=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/JA Analyses/bcb formatted.csv",header=TRUE, dec=".")


taus = pnorm(seq(-2,2,0.5), mean = 0, sd = 1) # this set of quantiles is evenly distributed over the normal PDF, which makes some subsequent steps neater
minYear = 1979

#take the average latitude of all sites in the state
MDlat=mean(39.682843,39.707787,39.673087,     39.337354,39.400865) #last 2 are APG sites
MAlat=mean(42.652001,42.485739,42.207190)


intersect(herbarium$scientificName, DailySums$PlantID)


useplants = intersect(herbarium$scientificName, DailySums$PlantID)
useplants = useplants[order(useplants)]
useplants = useplants[c(1:3, 5:13)]
j = 4
useplants[j]
usedat = DailySums[DailySums$PlantID == useplants[j],]
table(usedat$State) # do not use if < 5 observations
with(usedat[usedat$Total > 0,], table(State)) # do not use if < 3 observations
with(usedat[usedat$State == "MD", ], plot(DOY, Total))

# usable plant species
# Achillea millefolium - both states
# Asclepias syriaca - neither state
# Erigeron annuus - both states
# Galium mollugo - yes MA, no MD
# Lonicera japonica - no MA, yes MD
# Lythrum salicaria - yes MA, no MD
# Rosa multifloa - both states
# Rubus allegheniensis - no MA, yes MD
# Rudbeckia hirta - neither state
# Solidago altissima - yes MA, no MD
# Viburnum dentatum - neither state
# Vicia cracca - yes MA, no MD

useMA = c(1,3,4,6,7,10,12)
useMD = c(1,3, 4, 5,7,8)
useBoth = c(1,3,4,7)

# color ramp for nectar plants
hue2 = seq(0, 1, length.out = 13)
sat2 = rep(0.5, 13)
val2 = rep(1, 13)
hsv.ramp <- hsv(h = hue2, s = sat2, v = val2)

#############################
# within site relative phenologies
# Massachusetts
#############################
MAout = array(NA, dim = c(length(useMA)+1, 4))

# checkerspots
mod1 = rq(startDayOfYear ~ year+decimalLatitude, data = bcb[bcb$year > minYear,], tau = taus)
# June had an additive model, though it looks like the interaction is better based on AIC
  days = t(predict(mod1, newdata = data.frame(year = 2018.5, decimalLatitude = MAlat))) 
  Lcoef = coefficients(lm(qlogis(taus)~days))
  mu1 = -Lcoef[1]/Lcoef[2]
  sig1 = sqrt((pi^2)/(3*(Lcoef[2]^2)))
mod2 = glm.nb(Total ~ SiteYear + DOY + I(DOY*DOY),data = DailyBCBSums[DailyBCBSums$State == "Massachusetts",])
  Gcoef = coefficients(mod2)
  mu2 = -Gcoef[7]/(2*Gcoef[8])
  sig2 = sqrt(-1/(2*Gcoef[8]))
MAout[1,] = c(mu1, sig1, mu2, sig2)
  
for(j in 1:length(useMA)){
  usedat = herbarium[herbarium$scientificName == useplants[useMA[j]],]
  mod1 = rq(startDayOfYear ~ year+decimalLatitude, data = usedat[usedat$year > minYear,], tau = taus)
    days = t(predict(mod1, newdata = data.frame(year = 2018.5, decimalLatitude = MAlat))) 
    Lcoef = coefficients(lm(qlogis(taus)~days))
    mu1 = -Lcoef[1]/Lcoef[2]
    sig1 = sqrt((pi^2)/(3*(Lcoef[2]^2)))
  # these next couple lines are a hack to get the models to work - adding 0's two weeks before the 1st and two weeks before the last observation
  use.nect = DailySums[DailySums$PlantID == useplants[useMA[j]] & DailySums$State == "MA",c("Total", "DOY", "Area")]
  use.nect = rbind(use.nect, c(0, min(use.nect$DOY)-14, 200))
  use.nect = rbind(use.nect, c(0, max(use.nect$DOY)+14, 200))
   mod2 = try(glm.nb(Total ~ DOY + I(DOY*DOY) + offset(log(Area)),data = use.nect))
  if(class(mod2)[1] == "try-error") { mod2 = glm(Total ~ DOY + I(DOY*DOY), offset = log(Area), family = poisson, data = use.nect)}
    Gcoef = coefficients(mod2)
    mu2 = -Gcoef[2]/(2*Gcoef[3])
    sig2 = sqrt(-1/(2*Gcoef[3]))
  MAout[(j+1),] = c(mu1, sig1, mu2, sig2)
}
MAout
MAout.data = data.frame(MAout)
names(MAout.data) = c("muL", "sigL", "muG", "sigG")
MAout.data$species = c("Euphydryas phaeton", useplants[useMA])

plotCI(MAout.data$muG, MAout.data$muL, uiw = MAout.data$sigL, pch = c(23, rep(19,length(useMA))), col = c("black", hsv.ramp[useMA]), lwd = 2, xlab = "", ylab = "", xlim = c(145, 225), ylim = c(130,280))
plotCI(MAout.data$muG, MAout.data$muL, uiw = MAout.data$sigG, pch = c(23, rep(19,length(useMA))), err = "x", col = c("black", hsv.ramp[useMA]), lwd = 2, add = T)
abline(a = 0, b = 1, lty = "dotted")
mtext(side = 1, line = 2, "activity period from field data (DOY)")
mtext(side = 2, line = 2, "activity period from historical data (DOY)")
mtext(side = 3, line = 1, "B. Species in Massachusetts")
legend("topleft",col = c("black", hsv.ramp[useMA]), lwd = 3, legend = (MAout.data$species), cex = 0.5)

# some tests of patterns
with(MAout.data, cor.test(muL, muG)) # good correlation; r = 0.84, P = 0.009
with(MAout.data, t.test(muG-muL)) # mean date tends to be earlier in field data; diff = 13 days, P = 0.055
with(MAout.data, t.test(sigG-sigL)) # sd is lower in field data; diff = 21.4 days, P < 0.001


#############################
# within site relative phenologies
# Maryland
#############################
MDout = array(NA, dim = c(length(useMD)+1, 4))

# checkerspots
mod1 = rq(startDayOfYear ~ year+decimalLatitude, data = bcb[bcb$year > minYear,], tau = taus)
# June had an additive model, though it looks like the interaction is better based on AIC
  days = t(predict(mod1, newdata = data.frame(year = 2018.5, decimalLatitude = MDlat))) 
  Lcoef = coefficients(lm(qlogis(taus)~days))
  mu1 = -Lcoef[1]/Lcoef[2]
  sig1 = sqrt((pi^2)/(3*(Lcoef[2]^2)))
mod2 = glm.nb(Total ~ SiteYear + DOY + I(DOY*DOY),data = DailyBCBSums[DailyBCBSums$State == "Maryland",])
Gcoef = coefficients(mod2)
mu2 = -Gcoef[8]/(2*Gcoef[9])
sig2 = sqrt(-1/(2*Gcoef[9]))
MDout[1,] = c(mu1, sig1, mu2, sig2)

for(j in 1:length(useMD)){
  usedat = herbarium[herbarium$scientificName == useplants[useMD[j]],]
  mod1 = rq(startDayOfYear ~ year+decimalLatitude, data = usedat[usedat$year > minYear,], tau = taus)
  days = t(predict(mod1, newdata = data.frame(year = 2018.5, decimalLatitude = MDlat))) 
  Lcoef = coefficients(lm(qlogis(taus)~days))
  mu1 = -Lcoef[1]/Lcoef[2]
  sig1 = sqrt((pi^2)/(3*(Lcoef[2]^2)))
  use.nect = DailySums[DailySums$PlantID == useplants[useMD[j]] & DailySums$State == "MD",c("Total", "DOY", "Area")]
  use.nect = rbind(use.nect, c(0, min(use.nect$DOY)-14, 200))
  use.nect = rbind(use.nect, c(0, max(use.nect$DOY)+14, 200))
  mod2 = try(glm.nb(Total ~ DOY + I(DOY*DOY) + offset(log(Area)),data = use.nect))
  if(class(mod2)[1] == "try-error") { mod2 = glm(Total ~ DOY + I(DOY*DOY), offset = log(Area), family = poisson, data = use.nect)}
  Gcoef = coefficients(mod2)
  mu2 = -Gcoef[2]/(2*Gcoef[3])
  sig2 = sqrt(-1/(2*Gcoef[3]))
  MDout[(j+1),] = c(mu1, sig1, mu2, sig2)
}

MDout.data = data.frame(MDout)
names(MDout.data) = c("muL", "sigL", "muG", "sigG")
MDout.data$species = c("Euphydryas phaeton", useplants[useMD])

# 4.25 x 4.75
plotCI(MDout.data$muG, MDout.data$muL, uiw = MDout.data$sigL, pch = c(23, rep(19,length(useMD))), col = c("black", hsv.ramp[useMD]), lwd = 2, xlab = "", ylab = "", xlim = c(145, 225), ylim = c(130, 280))
plotCI(MDout.data$muG, MDout.data$muL, uiw = MDout.data$sigG, pch = c(23, rep(19,length(useMD))), err = "x", col = c("black", hsv.ramp[useMD]), lwd = 2, add = T)
abline(a = 0, b = 1, lty = "dotted")
mtext(side = 1, line = 2, "activity period from field data (DOY)")
mtext(side = 2, line = 2, "activity period from historical data (DOY)")
mtext(side = 3, line = 1, "A. Species in Maryland")
legend("bottomright",col = c("black", hsv.ramp[useMD]), lwd = 3, legend = (MDout.data$species), cex = 0.5)

# some tests of patterns
with(MDout.data, cor.test(muL, muG)) # good correlation; r = 0.80, P = 0.032
with(MDout.data, t.test(muG-muL)) # mean date tends to be earlier in field data; diff = 11.2 days, P = 0.068
with(MDout.data, t.test(sigG-sigL)) # sd is lower in field data; diff = 26.1 days, P = 0.009

#################################3
# species in both regions
##################################

MDsub = MDout.data[is.element(MDout.data$species, useplants[useBoth]) == T,]
MDsub = rbind(MDout.data[1,], MDsub)
MDsub$state = "MD"
MAsub = MAout.data[is.element(MAout.data$species, useplants[useBoth]) == T,]
MAsub = rbind(MAout.data[1,], MAsub)
MAsub$state = "MA"

both.data = rbind(MDsub, MAsub)

# plot of difference in means
plot(both.data$muG, both.data$muL, pch = c(23, rep(19,4)), col = c("black", hsv.ramp[useBoth]), xlab  = "", ylab = "", xlim = c(145, 225), ylim = c(130, 280))
for(i in 1:5){
  usedat = both.data[c(i, i+5),]
  points(usedat$muG, usedat$muL, col = c("black", hsv.ramp[useBoth])[i], type = "l", lwd = 3)
}
points(both.data$muG, both.data$muL, lwd = 3, pch = c(23, rep(19,4)), col = c("black", hsv.ramp[useBoth]))
abline(a = 0, b = 1, lty = "dotted")
mtext(side = 1, line = 2, "mean date from field data")
mtext(side = 2, line = 3, "mean date projected")
mtext(side = 2, line = 2, "from historical data")
mtext(side = 3, line = 1, "C. Latitudinal difference in means")
legend("topleft", col = c("black", hsv.ramp[useBoth]), lwd = 3, legend = (both.data$species[1:5]), cex = 0.5)

# plot of difference in SDs
plot(both.data$sigG, both.data$sigL, pch = c(23, rep(19,4)), col = c("black", hsv.ramp[useBoth]), xlab  = "", ylab = "", xlim = c(0,50), ylim = c(0,50))
for(i in 1:5){
  usedat = both.data[c(i, i+5),]
  points(usedat$sigG, usedat$sigL, col = c("black", hsv.ramp[useBoth])[i], type = "l", lwd = 3)
}
points(both.data$sigG, both.data$sigL, lwd = 3, pch = c(23, rep(19,4)), col = c("black", hsv.ramp[useBoth]))
abline(a = 0, b = 1, lty = "dotted")
mtext(side = 1, line = 2, "activity sd from field data")
mtext(side = 2, line = 3, "activity sd projected")
mtext(side = 2, line = 2, "from historical data")
mtext(side = 3, line = 1, "D. Latitudinal difference in standard deviations")
abline(a = 0, b = 1, lty = "dotted")
legend("bottomright", col = c("black", hsv.ramp[useBoth]), lwd = 3, legend = (both.data$species[1:5]), cex = 0.5)

# tests of slopes - are they equal to one?
t.test((MAsub$muL-MDsub$muL)/(MAsub$muG - MDsub$muG), mu = 1) # is the estimated slope = 1?
t.test((MAsub$sigL-MDsub$sigL)/(MAsub$sigG - MDsub$sigG), mu = 1) # is the estimated slope = 1?
# not different ,as far as you can tell from a single sample

# are there other species we could include if we pulled more herbarium data?
plant.counts1 = table(DailySums$PlantID, DailySums$State)
plant.counts2 = with(DailySums[DailySums$Total > 0,], table(PlantID, State))
MAall = intersect(rownames(plant.counts1)[plant.counts1[,1] > 5], rownames(plant.counts2)[plant.counts2[,1] > 3])
MAnew = setdiff(MAall, useplants)
MAnew
MDall = intersect(rownames(plant.counts1)[plant.counts1[,2] > 5], rownames(plant.counts2)[plant.counts2[,2] > 3])
MDnew = setdiff(MDall, useplants)
MDnew
intersect(MAnew, MDnew)

unique(herbarium$scientificName)
