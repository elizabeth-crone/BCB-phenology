
mean1 = 200
SD1 = 20
mean2 = 200
SD2 = 20
vals = 100:300
PDF1 = dnorm(vals, mean = mean1, sd = SD1)
PDF1 = PDF1/sum(PDF1)
PDF2 = dnorm(vals, mean = mean2, sd = SD2)
PDF2 = PDF2/sum(PDF2)
(PO = round(sum(PDF1 * PDF2),3))
plot(100:300, PDF1, type = "l", lwd = 3, col = "gray50", xlab = "", ylab = "", main = "")
mtext(side = 2, line = 2, "proportion of activity")
mtext(side = 1, line = 2, "day of year")
mtext(side = 3, line = 0.5, paste("PO = ", PO), cex = 1.5)
points(100:300, PDF2, type = "l", lwd = 3, lty = "dotted", col = "black")


mean1 = 200
SD1 = 10
mean2 = 200
SD2 = 10
vals = 100:300
PDF1 = dnorm(vals, mean = mean1, sd = SD1)
PDF1 = PDF1/sum(PDF1)
PDF2 = dnorm(vals, mean = mean2, sd = SD2)
PDF2 = PDF2/sum(PDF2)
(PO = round(sum(PDF1 * PDF2),3))
plot(100:300, PDF1, type = "l", lwd = 3, col = "gray50", xlab = "", ylab = "", main = "")
mtext(side = 2, line = 2, "proportion of activity")
mtext(side = 1, line = 2, "day of year")
mtext(side = 3, line = 0.5, paste("PO = ", PO), cex = 1.5)
points(100:300, PDF2, type = "l", lwd = 3, lty = "dotted", col = "black")

vals = 100:300
means = seq(175,225,25)
sds = seq(10,25, 5)

pdf("C:/Users/ecrone01/Box/Documents/checkerspot phenology/2023 manuscript/main checkerspot phenology paper/penultimate for submission July 2023/PO.pdf")
par(mfrow = c(3,4))
for(i in 1:1){
  for(j in 1:1){
    for(ii in 1:3){
      for(jj in 1:4){
        mean1 = means[i]
        SD1 = sds[j]
        mean2 = means[ii]
        SD2 = sds[jj]
        PDF1 = dnorm(vals, mean = mean1, sd = SD1)
        PDF1 = PDF1/sum(PDF1)
        PDF2 = dnorm(vals, mean = mean2, sd = SD2)
        PDF2 = PDF2/sum(PDF2)
        (PO = round(sum(PDF1 * PDF2),4))
        plot(100:300, PDF1, type = "l", lwd = 3, col = "goldenrod", xlab = "", ylab = "", main = "", ylim = c(0, 0.04))
        mtext(side = 2, line = 2, "proportion of activity")
        mtext(side = 1, line = 2, "day of year")
        mtext(side = 3, line = 0.5, paste("PO = ", PO), cex = 1.1)
        points(100:300, PDF2, type = "l", lwd = 3, lty = "longdash", col = "black")
      }
    }
  }
}
dev.off()
