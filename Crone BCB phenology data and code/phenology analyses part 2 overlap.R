library(quantreg)
library(MASS)
library(plotrix)
library(reshape)
library(car)
library(dplyr)



################################
# question 2 - phenological overlap
################################

# first, average phenological overlap
# checkerspot and herbarium data
herbarium = read.csv("C:/Users/ecrone01/Dropbox/BCB Nectar/JA Analyses/herbarium formatted.csv",header=TRUE, dec=".")
## bcb data ##
bcb=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/JA Analyses/bcb formatted.csv",header=TRUE, dec=".")
minYear = 1979
bcbdat = bcb[bcb$year > minYear,]
unique(herbarium$scientificName)
plants = unique(herbarium$scientificName)
usenums = c(1,3,4,5,6,8,10,11,13,14,15,16)
useplants = plants[usenums]
useplants = sort(as.character(useplants))
useplants


# paramters for fine-scaled quantile analysis
taus2 = pnorm(seq(-2,2,0.5), mean = 0, sd = 1) # this set of quantiles is evenly distributed over the normal PDF, which makes some subsequent steps neater


#set up a function to calculate the pdf
pdf_function = function(model, year, lat, taus){
  
  days1 = t(predict(model, newdata = data.frame(year = year, decimalLatitude = lat))) 
  xvals1 = predict(lm(qlogis(taus)~days1), newdata = list(days1 = as.matrix(100:300)))
  dat1 = exp(-xvals1)/((1+exp(-xvals1))^2)
  dat1 = dat1/sum(dat1) 
  dat1
  
}

BCB1 = rq(startDayOfYear ~ year+decimalLatitude, data = bcbdat, tau = taus2)


#### run all plants in loop ####
# with more fine scaled taus

#limit data to just more recent years
herbdat = herbarium[herbarium$year > minYear,]



# this loop creates "overlap" a data frame with calculations of average nectar per day for each plant species at each latitude x year combination
overlap = NULL
for(j in c(1985, 2015)){
  for(k in c(39.7, 42.7)){
    for(i in 1:length(useplants)){
      usedat2 = herbdat[herbdat$scientificName == useplants[i],]
      year = j
      lat = k
      m2 = rq(startDayOfYear ~ decimalLatitude+year, tau = taus2, data = usedat2)
      PLpdf=pdf_function(model=m2, year=year, lat=lat, taus=taus2)
      BCBpdf=pdf_function(model=BCB1, year=year, lat=lat, taus=taus2)
      avail=sum(BCBpdf*PLpdf)
      species=useplants[i]
      overlap = rbind(overlap, data.frame(species, avail, year, lat))
    }
  }
}


overlap


#### read in and format transect data ####

#read in transect data
#Transects=read.csv(file="/Users/junearriens/Dropbox/BCB Nectar/Field Data/nectar field data as csv/transects.csv",header=TRUE, dec=".")
Transects=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/Field Data/nectar field data as csv/transects.csv",header=TRUE, dec=".")

# remove transects at sites with few/no Baltimore checkerspot butterflies
unique(Transects$Site)
Transects = Transects[Transects$Site != "EdgewoodAPG",]
Transects = Transects[Transects$Site != "SmallAPG",]
Transects = Transects[Transects$Site != "Appleton2",]
unique(Transects$Site)

# limit analyses to 12 focal nectar plant species
useplants = c("Lythrum salicaria", "Asclepias syriaca", "Lonicera japonica", "Erigeron annuus","Rudbeckia hirta", "Solidago altissima", "Achillea millefolium", "Rosa multiflora", "Galium mollugo", "Viburnum dentatum", "Vicia cracca", "Rubus allegheniensis")
Transects = Transects[is.element(Transects$PlantID, useplants) == T,]
unique(Transects$PlantID)

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

table(Transects$Date, Transects$Site)



#remove transects that didn't have counts
Transects=Transects[!is.na(Transects$Count),] # this removed one row of data for Gallium mollugo at Harvard in transect 2 - not sure why it wasn't counted, but will count as 0 to be conservative; other plants were counted in this transect

head(Transects)

#add up the number of flowers for each survey
Transects$ID=paste(Transects$Site,Transects$Date, Transects$PlantID)
TransectSums=aggregate(list(Total=Transects$Count), Transects['ID'], sum, na.rm=TRUE)
TransectCounts=merge(Transects[c("State","Site","Date","PlantID","CommonName","FloweringUnit","ID")], TransectSums, by="ID")
TransectCounts=TransectCounts[!duplicated(TransectCounts$ID),]




#divide total by area of a transect (20 m2) x number of transects to get density

TransectCounts$Density=TransectCounts$Total

TransectCounts[TransectCounts$Site %in% c("Alesia", "Harvard","Nuetzel","Upton"),]$Density = TransectCounts[TransectCounts$Site %in% c("Alesia", "Harvard","Nuetzel","Upton"),]$Total/(20*10)

TransectCounts[TransectCounts$Site=="McElwain",]$Density=TransectCounts[TransectCounts$Site=="McElwain",]$Total/(20*9)


#Appleton has an extra survey that is missing a transect
TransectCounts[TransectCounts$Site=="Appleton" & TransectCounts$Date=="6/23/19",]$Density = TransectCounts[TransectCounts$Site=="Appleton" & TransectCounts$Date=="6/23/19",]$Total/(20*9)
TransectCounts[TransectCounts$Site=="Appleton" & TransectCounts$Date!="6/23/19",]$Density = TransectCounts[TransectCounts$Site=="Appleton" & TransectCounts$Date!="6/23/19",,]$Total/(20*10)


#get the maximum density at each site
TransectCounts$ID2=paste(TransectCounts$Site, TransectCounts$PlantID)
TransectMax=aggregate(list(MaxDensity=TransectCounts$Density), TransectCounts['ID2'], max, na.rm=TRUE)
TransectMaximums=merge(TransectCounts[c("State","Site","PlantID","CommonName","FloweringUnit","ID2")], TransectMax, by="ID2")
TransectMaximums=TransectMaximums[!duplicated(TransectMaximums$ID2),]

# make a data frame including 0's
Ujs = data.frame(melt(tapply(TransectMaximums$MaxDensity, list(TransectMaximums$Site, TransectMaximums$PlantID), mean), varnames = c("Site", "PlantID")))
Ujs$value[is.na(Ujs$value)] = 0
Ujs$State = "MA"
Ujs$State[Ujs$Site %in% c("Alesia","Nuetzel","McElwain")] = "MD"

############################
#Detour: Galium mollugo units were counted separately in both MD and MA. 
#This bit of code adjusts the numbers appropriately
Units=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/Field Data/nectar field data as csv/units.csv",header=TRUE, dec=".")

#get the average units for each species
FlowersPerUnit=aggregate(list(AvgCountPerUnit=Units$CountPerUnit), Units['PlantID'], mean, na.rm=TRUE)

galMD=mean(Units[Units$State=="Maryland" & Units$PlantID=="Galium mollugo",]$CountPerUnit)
galMA=mean(Units[Units$State=="Massachusetts" & Units$PlantID=="Galium mollugo",]$CountPerUnit)
gal.mult = galMA/galMD

Ujs$FUm2 = Ujs$value
Ujs$FUm2[Ujs$PlantID == "Galium mollugo" & Ujs$State == "MD"] = Ujs$value[Ujs$PlantID == "Galium mollugo" & Ujs$State == "MD"]*gal.mult
# end of Detour
############################

tmp2 = melt(with(Ujs, tapply(FUm2, list(State, PlantID), mean)), varnames = c("State", "PlantID"))
names(tmp2)[3] = "FUm2"
tmp3 = melt(with(Ujs, tapply(value, list(State, PlantID), mean)), varnames = c("State", "PlantID"))
meanUjs = merge(tmp2, tmp3)

#############
# merge densities and overlap
#############
overlap$State = "MA"
overlap$State[overlap$lat == 39.7] = "MD"
names(overlap)[1] = "PlantID"

overlap2 = merge(overlap, meanUjs)
overlap2
overlap2$FO = overlap2$avail*overlap2$FUm2

######################
# now, add nectar sugar
######################
#Equation for total nectar at a site: TS_i = S_i * D_i * F_i * U_i
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

#read in longevity data
Longevity=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/Field Data/nectar field data as csv/longevity.csv",header=TRUE, dec=".")

#get the average longevity for each species
LongevityPerFlower=aggregate(list(AvgDaysOpen=Longevity$DaysOpen), Longevity['PlantID'], mean, na.rm=TRUE)

tmp1 = merge(NectarPerFlower, LongevityPerFlower)
NectarMain = merge(tmp1, FlowersPerUnit)
overlap3 = merge(overlap2, NectarMain, all.x = T, all.Y = F)
overlap3$AvgCountPerUnit[overlap3$State=="MD" & overlap3$PlantID=="Galium mollugo"]=galMD
overlap3$AvgCountPerUnit[overlap3$State=="MA" & overlap3$PlantID=="Galium mollugo"]=galMA
overlap3[overlap3$PlantID == "Galium mollugo",]
overlap3$SO = with(overlap3, avail*value*AvgConcFlower*AvgDaysOpen*AvgCountPerUnit) # using "value" not adjusted for differnce in flowers per unit of G. mollugo

# mean changes for each region
(POmeans = with(overlap3, tapply(avail, list(year, State), mean)))
(1-POmeans[2,]/POmeans[1,]) # percent decrease in each state through time
(POmeans[,1]/POmeans[,2]) # differences among states - MA is 1.1x higher than MD

(FOmeans = with(overlap3, tapply(FO, list(year, State), mean)))
(1-FOmeans[2,]/FOmeans[1,]) # percent decrease in each state through time
(FOmeans[,1]/FOmeans[,2]) # differences among states - MA is ~20x higher than MD

(SOmeans = with(overlap3, tapply(SO, list(year, State), mean)))
(1-SOmeans[2,]/SOmeans[1,]) # percent decrease in each state through time
(SOmeans[,1]/SOmeans[,2]) # differences among states - MA is ~4x higher than MD


MDchange.PO = 1 - overlap3$avail[overlap3$State == "MD" & overlap3$year == 2015]/overlap3$avail[overlap3$State == "MD" & overlap3$year == 1985]
MDchange.FO = 1 - overlap3$FO[overlap3$State == "MD" & overlap3$year == 2015]/overlap3$FO[overlap3$State == "MD" & overlap3$year == 1985]
MDchange.SO = 1 - overlap3$SO[overlap3$State == "MD" & overlap3$year == 2015]/overlap3$SO[overlap3$State == "MD" & overlap3$year == 1985]
MDchange = data.frame(useplants, MDchange.PO, MDchange.FO, MDchange.SO)
MDchange$useplants[which(MDchange$MDchange.PO > 0.1)] # species that have decreased by more than 10%
MDchange$useplants[which(MDchange$MDchange.PO < -0.1)] # species that have increased by more than 10%
MDchange$useplants[which(MDchange$MDchange.PO > 0)] # species that have decreased 
MDchange$useplants[which(MDchange$MDchange.PO < 0)] # species that have increased 

MAchange.PO = 1 - overlap3$avail[overlap3$State == "MA" & overlap3$year == 2015]/overlap3$avail[overlap3$State == "MA" & overlap3$year == 1985]
MAchange.FO = 1 - overlap3$FO[overlap3$State == "MA" & overlap3$year == 2015]/overlap3$FO[overlap3$State == "MA" & overlap3$year == 1985]
MAchange.SO = 1 - overlap3$SO[overlap3$State == "MA" & overlap3$year == 2015]/overlap3$SO[overlap3$State == "MA" & overlap3$year == 1985]
MAchange = data.frame(useplants, MAchange.PO, MAchange.FO, MAchange.SO)
MAchange$useplants[which(MAchange$MAchange.PO > 0.1)] # species that have decreased by more than 10%
MAchange$useplants[which(MAchange$MAchange.PO < -0.1)] # species that have increased by more than 10%
MAchange$useplants[which(MAchange$MAchange.PO > 0)] # species that have decreased 
MAchange$useplants[which(MAchange$MAchange.PO < 0)] # species that have increased 

# graphing multiple species
# parameters common to many graphs
hue2 = seq(0, 1, length.out = 13)
sat2 = rep(0.5, 13)
val2 = rep(1, 13)
hsv.ramp <- hsv(h = hue2, s = sat2, v = val2)
jitters = seq(-0.2, 0.2, 0.4/11)
set.seed(13)
jitters2 = sample(jitters, 12, replace = F)

useplants = useplants[order(useplants)]
mult = 7

# supplemental figure with lines for each species
# function for making graphs
overlap.graphs = function(uselat, metric){
  usedat = overlap3[overlap3$PlantID == useplants[1] & overlap3$State == uselat,]
#260 x 404
  ylims = c(0, max(select(overlap3,all_of(metric))))
  plotdat = cbind(usedat$year, select(usedat,all_of(metric)))
  plot(plotdat[,1]+mult*jitters2[1], plotdat[,2], xlim = c(1975, 2025), ylim = ylims, col = hsv.ramp[1], pch = 19, type = "o", cex = 1.25, xlab = "", ylab = "", lwd = 3)

  for(i in 2:length(useplants)){
    usedat = overlap3[overlap3$PlantID == useplants[i] & overlap3$State == uselat,]
    plotdat = cbind(usedat$year, select(usedat,all_of(metric)))
    points(plotdat[,1]+mult*jitters2[i], plotdat[,2], xlim = c(1975, 2025), ylim = ylims, col = hsv.ramp[i], pch = 19, type = "o", cex = 1.25, xlab = "", ylab = "", lwd = 3)
  }
  if(uselat == "MD") j = 2 else j = 1
  if(metric == "avail") points(c(1985, 2015), POmeans[,j], lwd = 3, pch = 19, cex = 1.5, type = "o")
  if(metric == "FO") points(c(1985, 2015), FOmeans[,j], lwd = 3, pch = 19, cex = 1.5, type = "o")
  if(metric == "SO") points(c(1985, 2015), SOmeans[,j], lwd = 3, pch = 19, cex = 1.5, type = "o")
mtext(side = 1, line = 2, "year")
  if(metric == "avail") mtext(side = 2, line = 2, "phenological overlap, PO")
  if(metric == "FO") mtext(side = 2, line = 2, "flower-weighted overlap, FO")
  if(metric == "SO") mtext(side = 2, line = 2, "nectar-weighted overlap, SO")
  if(uselat == "MD") mtext(side = 3, line = 0.5, "Maryland latitude")
  if(uselat == "MA") mtext(side = 3, line = 0.5, "Massachusetts latitude")
}

# Figure 4
# 3 x 5 in pdf
overlap.graphs(uselat = "MA", metric = "avail") #S1B
overlap.graphs(uselat = "MD", metric = "avail") #S1A
overlap.graphs(uselat = "MA", metric = "FO") #S1D
overlap.graphs(uselat = "MD", metric = "FO") #S1C
overlap.graphs(uselat = "MA", metric = "SO") #S1F
overlap.graphs(uselat = "MD", metric = "SO") #S1E


#######################
# Figure 3 - phenology curves
# phenology curves for all species at each site and year of interest
# new pdf size = 
for(k in c(39.7, 42.7)){
  for(j in c(1985, 2015)){
    for(i in 1:length(useplants)){
      usedat2 = herbdat[herbdat$scientificName == useplants[i],]
      year = j
      lat = k
      m2 = rq(startDayOfYear ~ decimalLatitude+year, tau = taus2, data = usedat2)
      if(i == 1) PLpdf=pdf_function(model=m2, year=year, lat=lat, taus=taus2)
      if(i > 1) PLpdf = PLpdf + pdf_function(model=m2, year=year, lat=lat, taus=taus2)
    }
    BCBpdf=pdf_function(model=BCB1, year=year, lat=lat, taus=taus2)
    BCBpdf = BCBpdf/sum(BCBpdf)
    my.lty = "dashed"
    if(j == 1985) my.lty = "solid"
    if(j == 1985) plot(100:300, 100*BCBpdf, type = "l", col = "black", main = paste(j,k), lwd = 2, xlab = "", ylab = "", ylim = c(0, 4), lty = my.lty)
    if(j == 2015) points(100:300, 100*BCBpdf, type = "l", col = "black", lwd = 2, lty = my.lty)
    PLpdf = PLpdf/sum(PLpdf)
    points(100:300, 100*PLpdf, type = "l", col = "goldenrod", lwd = 2, lty = my.lty)
    mtext(side = 1, line = 2, "day of year")
    mtext(side = 2, line = 2, "phenology %")
  }
}

# phenology curves weighted by FU
for(k in c(39.7, 42.7)){
  for(j in c(1985, 2015)){
    for(i in 1:length(useplants)){
      usedat2 = herbdat[herbdat$scientificName == useplants[i],]
      year = j
      lat = k
      state = "MD"
      if(lat == 42.7) state = "MA"
      weight = overlap3$FUm2[overlap3$State == state & overlap3$year == year & overlap3$PlantID == useplants[i]]
      m2 = rq(startDayOfYear ~ decimalLatitude+year, tau = taus2, data = usedat2)
      if(i == 1) PLpdf=weight*pdf_function(model=m2, year=year, lat=lat, taus=taus2)
      if(i > 1) PLpdf = PLpdf + weight*pdf_function(model=m2, year=year, lat=lat, taus=taus2)
    }
    print(sum(PLpdf))
    BCBpdf=pdf_function(model=BCB1, year=year, lat=lat, taus=taus2)
    BCBpdf = BCBpdf/sum(BCBpdf)
    my.lty = "dashed"
    if(j == 1985) my.lty = "solid"
    if(j == 1985) plot(100:300, 100*BCBpdf, type = "l", col = "black", main = paste(j,k), lwd = 2, xlab = "", ylab = "", ylim = c(0, 4), lty = my.lty)
    if(j == 2015) points(100:300, 100*BCBpdf, type = "l", col = "black", lwd = 2, lty = my.lty)
    PLpdf = PLpdf/sum(PLpdf)
    points(100:300, 100*PLpdf, type = "l", col = "goldenrod", lwd = 2, lty = my.lty)
    mtext(side = 1, line = 2, "day of year")
    mtext(side = 2, line = 2, "flowering unit %")
  }
}

# phenology curves weighted by nectar sugar
for(k in c(39.7, 42.7)){
  for(j in c(1985, 2015)){
    for(i in 1:length(useplants)){
      usedat2 = herbdat[herbdat$scientificName == useplants[i],]
      year = j
      lat = k
      state = "MD"
      if(lat == 42.7) state = "MA"
      usedat3 = overlap3[overlap3$State == state & overlap3$year == year & overlap3$PlantID == useplants[i],]
      weight = with(usedat3, FUm2*AvgConcFlower*AvgDaysOpen*AvgCountPerUnit)
      m2 = rq(startDayOfYear ~ decimalLatitude+year, tau = taus2, data = usedat2)
      if(i == 1) PLpdf=weight*pdf_function(model=m2, year=year, lat=lat, taus=taus2)
      if(i > 1) PLpdf = PLpdf + weight*pdf_function(model=m2, year=year, lat=lat, taus=taus2)
    }
    print(sum(PLpdf))
    BCBpdf=pdf_function(model=BCB1, year=year, lat=lat, taus=taus2)
    BCBpdf = BCBpdf/sum(BCBpdf)
    my.lty = "dashed"
    if(j == 1985) my.lty = "solid"
    if(j == 1985) plot(100:300, 100*BCBpdf, type = "l", col = "black", main = paste(j,k), lwd = 2, xlab = "", ylab = "", ylim = c(0, 4), lty = my.lty)
    if(j == 2015) points(100:300, 100*BCBpdf, type = "l", col = "black", lwd = 2, lty = my.lty)
    PLpdf = PLpdf/sum(PLpdf)
    points(100:300, 100*PLpdf, type = "l", col = "goldenrod", lwd = 2, lty = my.lty)
    mtext(side = 1, line = 2, "day of year")
    mtext(side = 2, line = 2, "nectar sugar %")
  }
}


# legend for use in assembling figure
plot(0,0, ylim = c(1,10), xlim = c(1,10), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n") # blank frame

legend("topright", legend = c("1985", "2015", "", "butterfly", "nectar"), lwd = 2, lty = c("solid", "dashed", "solid", "solid", "solid"), col = c("antiquewhite4", "antiquewhite4", "white", "black", "goldenrod"), cex = 0.8)


