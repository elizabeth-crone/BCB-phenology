library(reshape)
library(lme4)
library(car)

# starting with measures of species richness
#read in transect data
Transects=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/Field Data/nectar field data as csv/transects.csv",header=TRUE, dec=".")

unique(Transects$Site)

# remove sites with no Baltimore checkerspot butterflies
Transects = Transects[Transects$Site %in% c("Alesia", "Appleton", "Harvard", "McElwain", "Nuetzel", "Upton"),]

#check survey dates
table(Transects$Date, Transects$Site)

#change individual surveys that were done over a few days into one date
Transects[Transects$Date=="6/22/19",]$Date="6/23/19"
Transects[Transects$Date=="6/17/19",]$Date="6/18/19"
Transects[Transects$Date=="6/19/19",]$Date="6/18/19"
Transects[Transects$Date=="6/10/19",]$Date="6/12/19"
Transects[Transects$Date=="6/26/19",]$Date="6/24/19"

#remove transects that didn't have flowers
#Transects[is.na(Transects$Count),]
#Transects=Transects[!is.na(Transects$Count),]
# eec is not sure we should do this - 0's are data

#change ones where species is listed as "NA" to "unknown"
Transects[is.na(Transects$PlantID),]$PlantID="Unknown"
Transects$PlantID = as.factor(Transects$PlantID)


names(Transects)
head(Transects)
Transects$present = as.numeric(Transects$Count > 1)
Transects$present[is.na(Transects$present)] = 0
# checking that each species is only listed once at each transect
Transects$ID = with(Transects, paste(Date, Site, Transect))
max(table(Transects$ID, Transects$PlantID))

richness = melt(with(Transects, tapply(present, list(State, Site, Date, Transect), sum, na.rm = T)), varnames = c("State", "Site", "Date", "Transect"))
names(richness)[5] = "nspp"
richness = richness[!is.na(richness$nspp),]
richness

m.rich = glmer.nb(nspp ~ State + (1|Site) + (1|Date), data = richness, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
hist(resid(m.rich)) # checking that log-scale residuals are approximately normal and symmetric
plot(predict(m.rich), resid(m.rich)) # no glaring trends [the "bands" result because only certain values are possible with count data, which is visible with low counts]

Anova(m.rich)
summary(m.rich)
m.rich.b = glmer.nb(nspp ~ 0+State + (1|Site) + (1|Date:State), data = richness, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
(rich.coef = exp(fixef(m.rich.b)))
(rich.cis = exp(confint(m.rich.b)[3:4,]))


# coefficients for each site
(MD.rich = exp(coef(m.rich)$Site[c(1,4,5),1]))
(MA.rich = exp(coef(m.rich)$Site[c(2,3,6),1] + coef(m.rich)$Site[1,2]))

SiteSums=aggregate(list(Total=Transects$present), by = list(Transects$Site, Transects$PlantID), sum, na.rm=TRUE)
names(SiteSums) = c("Site", "PlantID", "nobs")
SiteSums$present = as.numeric(SiteSums$nobs > 0)
with(SiteSums, tapply(present, Site, sum))

StateSums=aggregate(list(Total=Transects$present), by = list(Transects$State, Transects$PlantID), sum, na.rm=TRUE)
names(StateSums) = c("State", "PlantID", "nobs")
StateSums$present = as.numeric(StateSums$nobs > 0)
with(StateSums, tapply(present, State, sum))

# analysis of flowering units per site per transect
# first, need to adjust for annoying Galium mollugo being counted differently in each region
Transects$Count2 = Transects$Count
Transects$Count2[Transects$PlantID == "Galium mollugo" & Transects$State == "Maryland"] = round(0.3175076*Transects$Count[Transects$PlantID == "Galium mollugo" & Transects$State == "Maryland"],0)
TransectSumU=with(Transects,aggregate(list(SumCount=Count2), by = list(Site, Date, State, Transect), sum, na.rm=TRUE))
TransectSumU
names(TransectSumU) = c("Site", "Date", "State", "Transect", "SumCount")

m.FU = glmer.nb(SumCount ~ State + (1|Site) + (1|Date), data = TransectSumU)
summary(m.FU)
Anova(m.FU)
hist(resid(m.FU)) # approximately normal and symmetric?
plot(predict(m.FU), resid(m.FU)) # no glaring trends
m.FU.b = glmer.nb(SumCount ~ 0+State + (1|Site) + (1|Date), data = TransectSumU)
(FU.coef = exp(fixef(m.FU.b)))
(FU.cis = exp(confint(m.FU.b)[3:4,]))
predict(m.FU.b, newdata = data.frame(State = "Maryland", Site = "Alesia", Date = 9999), allow.new.levels = T, type = "response")
predict(m.FU.b, newdata = data.frame(State = "Maryland", Site = "Nuetzel", Date = 9999), allow.new.levels = T, type = "response")
predict(m.FU.b, newdata = data.frame(State = "Maryland", Site = "McElwain", Date = 9999), allow.new.levels = T, type = "response")
predict(m.FU.b, newdata = data.frame(State = "Massachusetts", Site = "Appleton", Date = 9999), allow.new.levels = T, type = "response")
predict(m.FU.b, newdata = data.frame(State = "Massachusetts", Site = "Harvard", Date = 9999), allow.new.levels = T, type = "response")
predict(m.FU.b, newdata = data.frame(State = "Massachusetts", Site = "Upton", Date = 9999), allow.new.levels = T, type = "response")

# analysis to check whether differences hold when G. mollugo is excluded
TransectSumU2=with(Transects[Transects$PlantID != "Galium mollugo",],aggregate(list(SumCount=Count2), by = list(Site, Date, State, Transect), sum, na.rm=TRUE))
names(TransectSumU2) = c("Site", "Date", "State", "Transect", "SumCount")

m.FU2 = glmer.nb(SumCount ~ State + (1|Site) + (1|Date), data = TransectSumU2)
summary(m.FU2)
Anova(m.FU2)
m.FU2.b = glmer.nb(SumCount ~ 0+State + (1|Site) + (1|Date), data = TransectSumU2)
(FU2.coef = exp(fixef(m.FU2.b)))
(FU2.cis = exp(confint(m.FU2.b)[3:4,]))


#######################
# next, multiply flowering units by nectar sugar

#######################
# Si, nectar per flower
#read in nectar data
Nectar=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/Field Data/nectar field data as csv/nectar.csv",header=TRUE, dec=".")
names(Nectar)[1] = "PlantID"
#anything with a negative concentration is basically 0, so change these to 0
Nectar$Concentration[Nectar$Concentration<0]=0
#can't extrapolate beyond the highest standard (400). Change any that are higher than 400 to 400
Nectar$Concentration[Nectar$Concentration>400]=400
#recalculate concentration per flower (ConcFlower column already in data) with new numbers
Nectar$ConcFlower = Nectar$Concentration/Nectar$NumberWashed
#convert nectar from ug/mL to mg (5 mL sample, divide by 1000 )
Nectar$ConcFlowerMG = (Nectar$ConcFlower * 5) / 1000 

#get the average nectar for each species
NectarPerFlower=aggregate(list(AvgConcFlower=Nectar$ConcFlowerMG), Nectar['PlantID'], mean, na.rm=TRUE)

############################
# Di, number of days each flower produces nectar
#read in longevity data
Longevity=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/Field Data/nectar field data as csv/longevity.csv",header=TRUE, dec=".")

#get the average longevity for each species
LongevityPerFlower=aggregate(list(AvgDaysOpen=Longevity$DaysOpen), Longevity['PlantID'], mean, na.rm=TRUE)

###########################
# Fi, flowers per sampling unit
#read in units data
Units=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/Field Data/nectar field data as csv/units.csv",header=TRUE, dec=".")

#get the average units for each species
FlowersPerUnit=aggregate(list(AvgCountPerUnit=Units$CountPerUnit), Units['PlantID'], mean, na.rm=TRUE)

#note: Galium mollugo units were counted separately in both MD and MA. These state averages will get added on to the main data file later on
galMD=mean(Units[Units$State=="Maryland" & Units$PlantID=="Galium mollugo",]$CountPerUnit)
galMA=mean(Units[Units$State=="Massachusetts" & Units$PlantID=="Galium mollugo",]$CountPerUnit)

#add all parts of nectar equation into one file
NectarMain=merge(Transects[c("Date", "PlantID","CommonName","FloweringUnit","State","Site","Count", "Count2", "Transect")], NectarPerFlower, by="PlantID", all.x = T)
NectarMain=merge(NectarMain, LongevityPerFlower, by="PlantID", all.x = T)
NectarMain=merge(NectarMain, FlowersPerUnit, by="PlantID", all.x = T)

#fix Gallium mollugo unit counts per state
NectarMain[NectarMain$State=="Maryland" & NectarMain$PlantID=="Galium mollugo",]$AvgCountPerUnit=galMD
NectarMain[NectarMain$State=="Massachusetts" & NectarMain$PlantID=="Galium mollugo",]$AvgCountPerUnit=galMA

dat1 = data.frame(with(NectarMain, tapply(Count2, list(PlantID, State), sum)))
dat1$PlantID = rownames(dat1)
dat2 = data.frame(with(NectarMain, tapply(AvgConcFlower, PlantID, sum)))
dat2$PlantID = rownames(dat2)
names(dat2)[1] = "SugarConc"
StateSum = merge(dat1, dat2, all.x = T, all.y=T)
StateSum[is.na(StateSum$SugarConc),]

# replace all Galiums with Galium mollugo
NectarMain[substr(NectarMain$PlantID, start = 1, stop = 6) =="Galium",]$AvgConcFlower = NectarPerFlower[NectarPerFlower$PlantID=="Galium mollugo",]$AvgConcFlower

NectarMain[substr(NectarMain$PlantID, start = 1, stop = 6) =="Galium",]$AvgDaysOpen = LongevityPerFlower[LongevityPerFlower$PlantID=="Galium mollugo",]$AvgDaysOpen

# replace all Rubus with Rubus allegeniensis
NectarMain[substr(NectarMain$PlantID, start = 1, stop = 5) =="Rubus",]$AvgConcFlower = NectarPerFlower[NectarPerFlower$PlantID=="Rubus allegheniensis",]$AvgConcFlower

NectarMain[substr(NectarMain$PlantID, start = 1, stop = 5) =="Rubus",]$AvgDaysOpen = LongevityPerFlower[LongevityPerFlower$PlantID=="Rubus allegheniensis",]$AvgDaysOpen

summary(NectarMain)
NectarMain[is.na(NectarMain$AvgConcFlower),]
# exclude plant species with no nectar data
NectarMain = NectarMain[!is.na(NectarMain$AvgConcFlower),]
head(NectarMain)
NectarMain$Uj = NectarMain$Count*NectarMain$AvgCountPerUnit
NectarMain$TSj = NectarMain$Uj*NectarMain$AvgConcFlower*NectarMain$AvgDaysOpen
head(NectarMain)

NectarSum=with(NectarMain,aggregate(list(SumFU=Count2, SumTS = TSj), by = list(Site, Date, State, Transect), sum, na.rm=TRUE))

NectarSum
names(NectarSum) = c("Site", "Date", "State", "Transect", "SumFU", "SumTS")

m.TS = lmer(SumTS ~ State + (1|Site) + (1|Date), data = NectarSum)
hist(resid(m.TS))# suggests ln-transformation
Anova(m.TS)
summary(m.TS)
m.TS = lmer(log(SumTS+1) ~ State + (1|Site) + (1|Date), data = NectarSum)
hist(resid(m.TS)) # a little better
plot(predict(m.TS), resid(m.TS)) # plot to test for obvious trends
Anova(m.TS)
m.TS.b = lmer(log(SumTS+1) ~ 0+State + (1|Site) + (1|Date), data = NectarSum)
(TS.coef = exp((fixef(m.TS.b))))
summary(m.TS.b)
(TS.cis = exp(confint(m.TS.b)[4:5,]))
exp(predict(m.TS.b, newdata = data.frame(State = "Maryland", Site = "Alesia", Date = 9999), allow.new.levels = T))
exp(predict(m.TS.b, newdata = data.frame(State = "Maryland", Site = "Nuetzel", Date = 9999), allow.new.levels = T))
exp(predict(m.TS.b, newdata = data.frame(State = "Maryland", Site = "McElwain", Date = 9999), allow.new.levels = T))
exp(predict(m.TS.b, newdata = data.frame(State = "Massachusetts", Site = "Appleton", Date = 9999), allow.new.levels = T))
exp(predict(m.TS.b, newdata = data.frame(State = "Massachusetts", Site = "Harvard", Date = 9999), allow.new.levels = T))
exp(predict(m.TS.b, newdata = data.frame(State = "Massachusetts", Site = "Upton", Date = 9999), allow.new.levels = T))


# checking the proportion of FU included in each state's nectar count
sum(NectarSum$SumFU)
sum(TransectSumU$SumCount)
sum(NectarSum$SumFU)/ sum(TransectSumU$SumCount)
(p.MD = sum(NectarSum$SumFU[NectarSum$State == "Maryland"])/ sum(TransectSumU$SumCount[TransectSumU$State == "Maryland"]))
(p.MA = sum(NectarSum$SumFU[NectarSum$State == "Massachusetts"])/ sum(TransectSumU$SumCount[TransectSumU$State == "Massachusetts"]))

NectarSum$SumTSadj = NectarSum$SumTS/p.MD
NectarSum$SumTSadj[NectarSum$State == "Massachusetts"] = NectarSum$SumTS[NectarSum$State == "Massachusetts"]/p.MA

m.TS2 = lmer(log(SumTSadj+1) ~ State + (1|Site) + (1|Date), data = NectarSum)
hist(resid(m.TS))# suggests ln-transformation
Anova(m.TS2)
m.TS2.b = lmer(log(SumTSadj+1) ~ 0+State + (1|Site) + (1|Date), data = NectarSum)

(TS.coef2 = exp(fixef(m.TS2.b)))
summary(m.TS.b)
(TS.cis2 = exp(confint(m.TS2.b)[4:5,]))



useplants = c("Lythrum salicaria", "Asclepias syriaca", "Lonicera japonica", "Erigeron annuus","Rudbeckia hirta", "Solidago altissima", "Achillea millefolium", "Rosa multiflora", "Galium mollugo", "Viburnum dentatum", "Vicia cracca", "Rubus allegheniensis")
NectarSub = NectarMain[NectarMain$PlantID %in% useplants,]
unique(NectarSub$PlantID)
NectarSubSum=with(NectarSub,aggregate(list(SumFU=Count2, SumTS = TSj), by = list(Site, Date, State, Transect), sum, na.rm=TRUE))

sum(NectarSubSum$SumFU)
sum(TransectSumU$SumCount)
sum(NectarSubSum$SumFU)/ sum(TransectSumU$SumCount) # 84% of all FUs in our focal species!
sum(NectarSubSum$SumTS)/ sum(NectarSum$SumTS) # 97% of nectar was by our focal species!


(p.MD1 = sum(NectarSubSum$SumFU[NectarSubSum$Group.3 == "Maryland"])/ sum(TransectSumU$SumCount[TransectSumU$State == "Maryland"])) #59% of flowers in MD
(p.MA1 = sum(NectarSubSum$SumFU[NectarSubSum$Group.3 == "Massachusetts"])/ sum(TransectSumU$SumCount[TransectSumU$State == "Massachusetts"])) #85% of flowers in MA

(p.MD2 = sum(NectarSubSum$SumTS[NectarSubSum$Group.3 == "Maryland"])/ sum(NectarSum$SumTS[NectarSum$State == "Maryland"])) #94% of measured sugar in MD
(p.MA2 = sum(NectarSubSum$SumTS[NectarSubSum$Group.3 == "Massachusetts"])/ sum(NectarSum$SumTS[NectarSum$State == "Massachusetts"])) #98% of measured sugar in MA


unique(Transects$PlantID)[order(unique(Transects$PlantID))]
# 74 taxa total, including "unknown", and 5 "Genus sp" taxa - I'd say we have 68 species total


library(plotrix)
# pdf 2.49 x 4.09
plotCI(1:2, rich.coef, ui = rich.cis[,2], li = rich.cis[,1], xlim = c(0.5,2.5), ylim = c(0, 6), xlab = "", ylab = "", xaxt = "n", pch = 19, col = "darkolivegreen", cex = 1.5, lwd = 2)
mtext(side = 1, line = 2, "Region")
axis(side = 1, at = 1:2, c("MD", "MA"))
mtext(side = 2, line = 2, "species richness")

plotCI(1:2, FU.coef, ui = FU.cis[,2], li = FU.cis[,1], xlim = c(0.5,2.5), xlab = "", ylab = "", xaxt = "n", pch = 19, col = "darkorchid", cex = 1.5, lwd = 2)
mtext(side = 1, line = 2, "Region")
axis(side = 1, at = 1:2, c("MD", "MA"))
mtext(side = 2, line = 2, "flowering units")

plotCI(1:2, TS.coef, ui = TS.cis[,2], li = TS.cis[,1], xlim = c(0.5,2.5), xlab = "", ylab = "", xaxt = "n", pch = 19, col = "goldenrod", cex = 1.5, lwd = 2)
mtext(side = 1, line = 2, "Region")
axis(side = 1, at = 1:2, c("MD", "MA"))
mtext(side = 2, line = 2, "nectar sugar")

