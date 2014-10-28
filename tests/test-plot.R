
library(gridGraphics)

plotdiff(expression(plot(1)), "plot-1")
plotdiff(expression(plot(1:10)), "plot-10")
plotdiff(expression(plot(1:10, main="title")), "plot-title")
plotdiff(expression(plot(1:10, sub="byline")), "plot-sub")
plotdiff(expression(pie(1:10)), "pie")
# Test margin labelling when multiple plots on page (so cexbase=.66)
plotdiff(expression({ par(mfrow=c(2, 2)); plot(1:10) }), "plot-mfrow")

plotdiffResult()
