library(tidyverse)
library(nlme)
library(car)

setwd("C:/Users/ecrone01/Box/SERDP Grant/SERDP_Final_Report/Other/turtlehead phenology")
dat = read.csv("all_turtlehead_clean.csv")


dat.fv = dat[which(dat$flowers == "Y" | dat$vegetative == "Y"),]
nrow(dat.fv)
dat.fv$PhenCat = paste(dat.fv$flowers, dat.fv$vegetative)
table(dat.fv$PhenCat)
unique(dat.fv$PhenCat)
dat.fv$PhenCat[dat.fv$PhenCat == "maybe Y"] = "veg"
dat.fv$PhenCat[dat.fv$PhenCat == "N Y"] = "veg"
dat.fv$PhenCat[dat.fv$PhenCat == "Y  "] = "flwr"
dat.fv$PhenCat = as.factor(dat.fv$PhenCat)
table(dat.fv$PhenCat)

#######################
# filtering since 1970
#######################
dat.fv_1970 = dat.fv[dat.fv$year > 1969,]
nrow(dat.fv_1970) # 860 obs
table(dat.fv_1970$PhenCat)


# Scaling variables - i.e., make year 0 = 1970
dat.fv_1970$year0 = dat.fv_1970$year - 1970

# Scaling latitude and longitude
dat.fv_1970$Lat0 = as.numeric(scale(dat.fv_1970$decimalLatitude))
dat.fv_1970$Lon0 = as.numeric(scale(dat.fv_1970$decimalLongitude))

gls_1 <- gls(
  startDayOfYear ~ year0*PhenCat*Lat0,
  data = dat.fv_1970,
  weights = varIdent(form = ~1|PhenCat),
  na.action = na.omit
)
Anova(gls_1)
summary(gls_1)

# removing NS interactions for better predictions
gls_2 <- gls(
  startDayOfYear ~ year0+PhenCat + Lat0,
  data = dat.fv_1970,
  weights = varIdent(form = ~1|PhenCat),
  na.action = na.omit
)
Anova(gls_2)
summary(gls_2)
# over this time period, there is no significant change in phenology of either life stage, 
# but there are very few vegetative specimens


# plot since 1970

years = rep(1970:2016 - 1970, 2) 
length(years)/2
Lats = rep(0, length(years))
flwrs = rep("flwr", length(years))
vegs = rep("veg", length(years))
preds2 = as.numeric(predict(gls_2, newdata = data.frame(year0 = years, Lat0 = Lats, PhenCat = c(vegs, flwrs))))
preds2
myshapes = c(19, 4)
mysizes = c(1, 1.5)
mycolors = c("rosybrown1", "darkolivegreen")
varInfl = 3.115464 # from summary - can't figure out how to extract automatically
varVeg = gls_2$sigma*varInfl

with(dat.fv_1970[order(dat.fv_1970$PhenCat),], plot(year, startDayOfYear, col = mycolors[as.numeric(PhenCat)], pch = myshapes[as.numeric(PhenCat)], cex = mysizes[as.numeric(PhenCat)], lwd = 2, xlab = "", ylab = ""))
mtext(side = 1, line = 2, "year")
mtext(side = 2, line = 2, "specimen date")
points(1970:2016, preds2[1:47], type = "l", lwd = 2)
points(1970:2016, preds2[1:47]+varVeg, type = "l", lty = "dotted")
points(1970:2016, preds2[1:47]-varVeg, type = "l", lty = "dotted")
legend("bottom", legend = c("veg", "flwr"), pch = myshapes[2:1], col = mycolors[2:1], cex = 0.8, horiz = T, pt.lwd = c(2,1))

preds2[10] # day of year in 1980
preds2[46] # day of year in 2016
preds2[46] - preds2[10]

scale(dat.fv_1970$decimalLatitude)

MD_Lat =(39.7 - 40.43836)/3.877852
preds = as.numeric(predict(gls_2, newdata = data.frame(year0 = rep((2018-1970),2), Lat0 = rep(MD_Lat,2), PhenCat = c("veg", "flwr"))))
preds
preds1 = as.numeric(predict(gls_2, newdata = data.frame(year0 = rep(10,2), Lat0 = rep(MD_Lat,2), PhenCat = c("veg", "flwr"))))
DOYs = 90:310
preds4 = dnorm(DOYs, mean = preds1[1], sd = varVeg) #1980
preds3 = dnorm(DOYs, mean = preds[1], sd = varVeg) #2018
#344x331
plot(DOYs, preds4, type = "l", col = "darkolivegreen3", lwd = 4, xlab = "", ylab = "")
mtext(side = 2, line = 3, "predicted distribution")
mtext(side = 2, line = 2, "(vegetative plants)")
mtext(side = 1, line = 2, "day of year")
mtext(side = 3, line = 0.5, "solved for Maryland latitude")
points(DOYs, preds3, type = "l", col = "darkolivegreen", lwd = 4)
legend("topright", legend = c(1980, 2018), lwd = 3, col = c("darkolivegreen3", "darkolivegreen"), cex = 0.7)
# overlaying field dates of caterpillars
# same dates for both time periods
# post-diapause caterpillars in spring
# pre-diapause caterpillars in summer 
arrows(x0 = 106, x1=  178, y0 = 0.002, angle = 90, code = 2, length = 0.05, lwd = 3) # april 16 - June 27
arrows(x0 = 106, x1=  178, y0 = 0.002, angle = 90, code = 1, length = 0.05, lwd = 3)
arrows(x0 = 184, x1=  230, y0 = 0.002, angle = 90, code = 2, length = 0.05, lwd = 3) # april 16 - June 27
arrows(x0 = 184, x1=  230, y0 = 0.002, angle = 90, code = 1, length = 0.05, lwd = 3)
legend("topright", legend = c(1980, 2018, "larvae"), lwd = 3, col = c("darkolivegreen3", "darkolivegreen", "black"), cex = 0.7)
(sum(preds4[16:88])- sum(preds3[16:88]))/sum(preds4[16:88])
(sum(preds4[94:140])- sum(preds3[94:140]))/sum(preds4[94:140])



####################
# misc figure from James' data
# very preliminary - for IPR only
###################
library(plotrix)
means = c(0.001, 0.480)
lows = c(0, 0.09)
ups = c(0.0242, 0.847)
plotCI( 1:2, means, li = lows, ui = ups, xlim = c(0.5, 2.5), xaxt = "n", xlab = "", ylab = "", pch = 21, cex = 1.25, pt.bg = "darkorange")
axis(side = 1, at = 1:2, labels = c("Maryland", "Massachusetts"))
mtext(side = 1, line = 2, "Location of populations")
mtext(side = 2, line = 3, "proportion using Plantago")
mtext(side = 2, line = 2, "(alternate host plant)")
mtext(side = 3, line = 0.5, "oviposition trials")
