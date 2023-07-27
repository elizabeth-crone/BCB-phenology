library(quantreg)
library(car)
library(plotrix)
library(emmeans)



#### read in data ####

## herbarium data ##
#herbarium=read.csv(file="/Users/junearriens/Dropbox/BCB Nectar/JA Analyses/herbarium formatted.csv",header=TRUE, dec=".")
herbarium = read.csv("C:/Users/ecrone01/Dropbox/BCB Nectar/JA Analyses/herbarium formatted.csv",header=TRUE, dec=".")
## bcb data ##
bcb=read.csv(file="C:/Users/ecrone01/Dropbox/BCB Nectar/JA Analyses/bcb formatted.csv",header=TRUE, dec=".")
###########################################
#### parameters common to all analyses ####
###########################################
taus = c(0.1, 0.5, 0.9)
minYear = 1979

# coefficients for standardizing latitude for all observations
allLat = c(herbarium$decimalLatitude, bcb$decimalLatitude)
mean(allLat) # 40.3 - near Princeton NJ
sd(allLat) # 3.99

###########################################
#### basic summary analyses and statistics ####
###########################################

## set up bcb data ##
bcbdat = bcb[bcb$year > minYear,]
bcbdat$lats2 = round(100*(bcbdat$decimalLatitude-35)/10,0)
bcbdat$lats2[bcbdat$lats2 < 1] = 1 # < 35 deg lat
bcbdat$lats2[bcbdat$lats2 > 100] = 100 # > 45 deg lat

bcbdat$year2 = bcbdat$year - 1980
bcbdat$lat2 = as.numeric(scale(bcbdat$decimalLatitude))
mean(bcbdat$lat2)
sd(bcbdat$lat2)

# basic statistics
summary(bcbdat)
mean(bcbdat$decimalLatitude)
sd(bcbdat$decimalLatitude)
min(bcbdat$decimalLatitude)
max(bcbdat$decimalLatitude)
mean(bcbdat$year)
sd(bcbdat$year)
min(bcbdat$year)
max(bcbdat$year)

## set up plant data ##
unique(herbarium$scientificName)
plants = unique(herbarium$scientificName)
usenums = c(1,3,4,5,6,8,10,11,13,14,15,16)
useplants = plants[usenums]
useplants = sort(as.character(useplants))
useplants

# summary statistics: selecting one species to analyze
j = 8 # number of selected species
useplants[j]
usedat = herbarium[herbarium$scientificName == useplants[j] & herbarium$year > minYear,]

usedat$year2 = usedat$year - 1980
usedat$lat2 = scale(usedat$decimalLatitude)

# basic statistics
summary(usedat)
mean(usedat$decimalLatitude)
sd(usedat$decimalLatitude)
min(usedat$decimalLatitude)
max(usedat$decimalLatitude)
mean(usedat$year)
sd(usedat$year)
min(usedat$year)
max(usedat$year)


###########################################
#### question 1 quantile regression in space and time
###########################################

# for baltimore checkerspots:
z1 = rq(startDayOfYear ~ year2*lat2, data = bcbdat, tau = taus)
summary(z1, se = "boot", R = 2000)
z1.stats = summary(z1, se = "boot", R = 2000)
z1.coefs = data.frame(rbind(z1.stats[[1]]$coefficients, z1.stats[[2]]$coefficients, z1.stats[[3]]$coefficients))

z1.coefs$response = rep(c("Int", "Year", "Lat", "YxL"), 3)


# for nectar plants:
for(j in 1:length(useplants)){
  usedat = herbarium[herbarium$scientificName == useplants[j] & herbarium$year > minYear,]
  usedat$year2 = usedat$year - 1980
  usedat$lat2 = scale(usedat$decimalLatitude)
  x.tmp = summary(rq(startDayOfYear ~ year2*lat2, data = usedat, tau = taus), se = "boot", R = 2000)
  if(j == 1){nectar.qrs = data.frame(rbind(x.tmp[[1]]$coefficients, x.tmp[[2]]$coefficients, x.tmp[[3]]$coefficients))}
  if(j > 1){nectar.qrs = rbind(nectar.qrs, data.frame(rbind(x.tmp[[1]]$coefficients, x.tmp[[2]]$coefficients, x.tmp[[3]]$coefficients)))}
}
dim(nectar.qrs)
nectar.qrs$species = rep(useplants, each = 12)
nectar.qrs$taus = rep(rep(c(0.1, 0.5, 0.9), each = 4), length(useplants))
nectar.qrs$response = rep(c("Int", "Year", "Lat", "YxL"), 3*length(useplants))

head(nectar.qrs)

# omnibus tests for statistical significance
# from "meta-analysis section of Sokal & Rohlf

nectar.qrs$Pr...t..[nectar.qrs$Pr...t.. == 0] = min(nectar.qrs$Pr...t..[nectar.qrs$Pr...t.. > 0])

# test for main effects of year
(chistat.nectar = -2*with(nectar.qrs[nectar.qrs$response == "Year",], sum(log(Pr...t..))))
(N.nectar = 2*3*length(useplants))
(chistat.bcb = -2*with(z1.coefs[z1.coefs$response == "Year",], sum(log(Pr...t..))))
(N.bcb = 2*3)

(chistat = chistat.bcb + chistat.nectar)
(N = N.nectar + N.bcb)
1 - pchisq(chistat, N)

# test for main effects of latitude
(chistat.nectar = -2*with(nectar.qrs[nectar.qrs$response == "Lat",], sum(log(Pr...t..))))
(N.nectar = 2*3*length(useplants))
(chistat.bcb = -2*with(z1.coefs[z1.coefs$response == "Lat",], sum(log(Pr...t..))))
(N.bcb = 2*3)

(chistat = chistat.bcb + chistat.nectar)
(N = N.nectar + N.bcb)
1 - pchisq(chistat, N)

# test for year x latitude interaction
(chistat.nectar = -2*with(nectar.qrs[nectar.qrs$response == "YxL",], sum(log(Pr...t..))))
(N.nectar = 2*3*length(useplants))
(chistat.bcb = -2*with(z1.coefs[z1.coefs$response == "YxL",], sum(log(Pr...t..))))
(N.bcb = 2*3)

(chistat = chistat.bcb + chistat.nectar)
(N = N.nectar + N.bcb)
1 - pchisq(chistat, N)



# rerun models with no interaction
# NB This code renames objects from the first analysis to their new names
z1 = rq(startDayOfYear ~ year2+lat2, data = bcbdat, tau = taus)
summary(z1, se = "boot", R = 2000)

for(j in 1:length(useplants)){
  usedat = herbarium[herbarium$scientificName == useplants[j] & herbarium$year > minYear,]
  usedat$year2 = usedat$year - 1980
  usedat$lat2 = scale(usedat$decimalLatitude)
  x.tmp = summary(rq(startDayOfYear ~ year2+lat2, data = usedat, tau = taus), se = "boot", R = 2000)
  if(j == 1){nectar.qrs = data.frame(rbind(x.tmp[[1]]$coefficients, x.tmp[[2]]$coefficients, x.tmp[[3]]$coefficients))}
  if(j > 1){nectar.qrs = rbind(nectar.qrs, data.frame(rbind(x.tmp[[1]]$coefficients, x.tmp[[2]]$coefficients, x.tmp[[3]]$coefficients)))}
}
dim(nectar.qrs)
nectar.qrs$species = rep(useplants, each = 9)
nectar.qrs$taus = rep(rep(c(0.1, 0.5, 0.9), each = 3), length(useplants))
nectar.qrs$response = rep(c("Int", "Year", "Lat"), 3*length(useplants))

head(nectar.qrs)
useplants


# tests comparing slopes of different quantiles
m0a = lm(Value ~ 0+ as.factor(taus), data = nectar.qrs[nectar.qrs$response == "Year",], weights = 1/Std..Error) # means parameterization for coefficients
YearCoefs = coef(m0a)
(YearLims = confint(m0a))
summary(m0a) # these p-values are interest - testing whether each coefficient differs from 0

m1a = lm(Value ~ 0+as.factor(taus), data = nectar.qrs[nectar.qrs$response == "Lat",], weights = 1/Std..Error)
(LatCoefs = coef(m1a))
(LatLims = confint(m1a))
summary(m1a)

# graphing multiple species
# parameters common to many graphs
hue2 = seq(0, 1, length.out = 13)
sat2 = rep(0.5, 13)
val2 = rep(1, 13)
hsv.ramp <- hsv(h = hue2, s = sat2, v = val2)
jitters = seq(-0.2, 0.2, 0.4/11)
set.seed(13)
jitters2 = sample(jitters, 12, replace = F)
nreps = 1000

#########################
# graph for year
########################## 
# pdf: 4 wide x 5 tall
# median
usedat = nectar.qrs[nectar.qrs$response == "Year" & nectar.qrs$taus == 0.5,]
myvals = array()
for(i in 1:nreps){
  myvals[i] = mean(rnorm(12, usedat$Value, usedat$Std..Error))
}
bcb.vals = summary(z1)[[2]]$coefficients[2,] # 1st index is the quantile (0.1, 0.5, 0.9); 2nd is the slope (int, year, lat, yxl)
# 391 x 435
plotCI(2.75+jitters2, usedat$Value, uiw = usedat$Std..Error, pch = 19, cex = 1.2, lwd = 2, col = hsv.ramp[as.factor(usedat$species)], xlim = c(0,5.5), ylim = c(-1.5, 2), xaxt = "n", ylab = "", xlab = "")
mtext (side = 2, line = 2, "slope of collection date (days/year)")
plotCI(2.75, YearCoefs[2], ui = YearLims[2,2], li = YearLims[2,1], col = "black", pch = 19, cex = 1.5, lwd = 2, add = T)
plotCI(2.25, bcb.vals[1], ui = bcb.vals[3], li = bcb.vals[2], col = "black", pch = 23, cex = 1.2, lwd = 2, add = T)

# onset
usedat = nectar.qrs[nectar.qrs$response == "Year" & nectar.qrs$taus == 0.1,]
myvals = array()
for(i in 1:nreps){
  myvals[i] = mean(rnorm(12, usedat$Value, usedat$Std..Error))
}
bcb.vals = summary(z1)[[1]]$coefficients[2,] # 1st index is the quantile (0.1, 0.5, 0.9); 2nd is the slope (int, year, lat, yxl)
plotCI(1+jitters2, usedat$Value, uiw = usedat$Std..Error, pch = 19, cex = 1.2, lwd = 2, col = hsv.ramp[as.factor(usedat$species)], add = T)
plotCI(1, YearCoefs[1], ui = YearLims[1,2], li = YearLims[1,1], col = "black", pch = 19, cex = 1.5, lwd = 2, add = T)
plotCI(0.5, bcb.vals[1], ui = bcb.vals[3], li = bcb.vals[2], col = "black", pch = 23, cex = 1.2, lwd = 2, add = T)

# end
usedat = nectar.qrs[nectar.qrs$response == "Year" & nectar.qrs$taus == 0.9,]
myvals = array()
for(i in 1:nreps){
  myvals[i] = mean(rnorm(12, usedat$Value, usedat$Std..Error))
}
bcb.vals = summary(z1)[[3]]$coefficients[2,] # 1st index is the quantile (0.1, 0.5, 0.9); 2nd is the slope (int, year, lat, yxl)
plotCI(4.5+jitters2, usedat$Value, uiw = usedat$Std..Error, pch = 19, cex = 1.2, lwd = 2, col = hsv.ramp[as.factor(usedat$species)], add = T)
plotCI(4.5, YearCoefs[3], ui = YearLims[3,2], li = YearLims[3,1], col = "black", pch = 19, cex = 1.5, lwd = 2, add = T)
plotCI(4, bcb.vals[1], ui = bcb.vals[3], li = bcb.vals[2], col = "black", pch = 23, cex = 1.2, lwd = 2, add = T)
axis(side = 1, at = c(1,2.75,4.5), labels = c("onset", "median", "end"))
mtext(side = 1, line = 2, "sighting date quantile")
abline(h = 0, lty = "dotted")
text(x = 0, y = 2, "getting later", adj = c(0, 0.5))
text(x = 0, y = -1.45, "getting earlier", adj = c(0, 0.5))

#legend("bottomright", pch = c(23, 19), lwd = 2, col = "black", legend = c("checkerspots", "nectar"), bty = "n", pt.cex = 1, cex = 0.8, pt.bg = "white")

#########################
# graph for Latitude
#########################
# pdf: 4 wide x 5 tall
# median
usedat = nectar.qrs[nectar.qrs$response == "Lat" & nectar.qrs$taus == 0.5,]
myvals = array()
for(i in 1:nreps){
  myvals[i] = mean(rnorm(12, usedat$Value, usedat$Std..Error))
}
bcb.vals = summary(z1)[[2]]$coefficients[3,] # 1st index is the quantile (0.1, 0.5, 0.9); 2nd is the slope (int, year, lat, yxl)
# 391 x 435
plotCI(2.75+jitters2, usedat$Value, uiw = usedat$Std..Error, pch = 19, cex = 1.2, lwd = 2, col = hsv.ramp[as.factor(usedat$species)], xlim = c(0,5.5), ylim = c(-20, 30), xaxt = "n", ylab = "", xlab = "")
mtext (side = 2, line = 2, "slope of collection date (days/\u00B0Lat)")
plotCI(2.75, LatCoefs[2], ui = LatLims[2,2], li = LatLims[2,1], col = "black", pch = 19, cex = 1.5, lwd = 2, add = T)
plotCI(2.25, bcb.vals[1], ui = bcb.vals[3], li = bcb.vals[2], col = "black", pch = 23, cex = 1.2, lwd = 2, add = T, pt.bg = "white")

# onset
usedat = nectar.qrs[nectar.qrs$response == "Lat" & nectar.qrs$taus == 0.1,]
myvals = array()
for(i in 1:nreps){
  myvals[i] = mean(rnorm(12, usedat$Value, usedat$Std..Error))
}
bcb.vals = summary(z1)[[1]]$coefficients[3,] # 1st index is the quantile (0.1, 0.5, 0.9); 2nd is the slope (int, year, lat, yxl)
plotCI(1+jitters2, usedat$Value, uiw = usedat$Std..Error, pch = 19, cex = 1.2, lwd = 2, col = hsv.ramp[as.factor(usedat$species)], add = T)
plotCI(1, LatCoefs[1], ui = LatLims[1,2], li = LatLims[1,1], col = "black", pch = 19, cex = 1.5, lwd = 2, add = T)
plotCI(0.5, bcb.vals[1], ui = bcb.vals[3], li = bcb.vals[2], col = "black", pch = 23, cex = 1.2, lwd = 2, add = T, pt.bg = "white")

# end
usedat = nectar.qrs[nectar.qrs$response == "Lat" & nectar.qrs$taus == 0.9,]
myvals = array()
for(i in 1:nreps){
  myvals[i] = mean(rnorm(12, usedat$Value, usedat$Std..Error))
}
bcb.vals = summary(z1)[[3]]$coefficients[3,] # 1st index is the quantile (0.1, 0.5, 0.9); 2nd is the slope (int, year, lat, yxl)
plotCI(4.5+jitters2, usedat$Value, uiw = usedat$Std..Error, pch = 19, cex = 1.2, lwd = 2, col = hsv.ramp[as.factor(usedat$species)], add = T)
plotCI(4.5, LatCoefs[3], ui = LatLims[3,2], li = LatLims[3,1], col = "black", pch = 19, cex = 1.5, lwd = 2, add = T)
plotCI(4, bcb.vals[1], ui = bcb.vals[3], li = bcb.vals[2], col = "black", pch = 23, cex = 1.2, lwd = 2, add = T, pt.bg = "white")
axis(side = 1, at = c(1,2.75,4.5), labels = c("median", "onset", "end"))
mtext(side = 1, line = 2, "sighting date quantile")
abline(h = 0, lty = "dotted")
text(x = 0, y = 30, "earlier in south", adj = c(0, 0.5))
text(x = 0, y = -19, "later in south", adj = c(0, 0.5))

########################
# question 2: nectar plant trends vs. traits
########################
# are species with different traits responding differently?

# trait # 1 - early vs. late flowering season
# extract median date of flowering; this is the date in 1980 because years were scaled to have the intercept in 1980
meds = nectar.qrs[nectar.qrs$taus == 0.5 & nectar.qrs$response == "Int", c("species", "Value") ]
names(meds)[2] = "median.int"
meds
nectar.qrs = merge(nectar.qrs, meds)
nectar.qrs

# statistical models with slope of phenology vs. flowering season
m4= lm(Value ~ as.factor(taus)*median.int, data = nectar.qrs[nectar.qrs$response == "Year",], weights = 1/Std..Error)
Anova(m4)
summary(m4)
m5 = lm(Value ~ as.factor(taus)*median.int, data = nectar.qrs[nectar.qrs$response == "Lat",], weights = 1/Std..Error)
Anova(m5)
summary(m5)


############################
# figure legend for nectar
############################
myspp = levels(as.factor(nectar.qrs$species))
# 3.9 x 4.51 in pdf
plot(0,0, ylim = c(1,10), xlim = c(1,10), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n") # blank frame
legend("topleft", legend = myspp, pch = 19, lwd = 3, col = hsv.ramp[as.factor(myspp)], cex = 0.8, pt.cex = 1.1, title = "Nectar plant species")

