
library(gridGraphics)

require(stats) # for rnorm

hc <- hclust(dist(USArrests), "ave")

dend1 <- function() {
    plot(hc)
}

dend2 <- function() {
    plot(hc, hang = -1)
}

hc <- hclust(dist(USArrests)^2, "cen")
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))

dend3 <- function() {
    opar <- par(mfrow = c(1, 2))
    plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
    plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
    par(opar)
}

hcity.D  <- hclust(UScitiesD, "ward.D") # "wrong"
hcity.D2 <- hclust(UScitiesD, "ward.D2")

dend4 <- function() {
    opar <- par(mfrow = c(1, 2))
    plot(hcity.D,  hang=-1)
    plot(hcity.D2, hang=-1)
    par(opar)
}

plotdiff(expression(dend1()), "dend-1")
plotdiff(expression(dend2()), "dend-2")
plotdiff(expression(dend3()), "dend-3")
plotdiff(expression(dend4()), "dend-4")

plotdiffResult()
