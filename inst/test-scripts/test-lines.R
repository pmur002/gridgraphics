
library(gridGraphics)

lines1 <- function() {
    plot(cars, main = "Stopping Distance versus Speed")
    lines(stats::lowess(cars))
}

## Straight to PNG to avoid PDF->PNG funny business
plotdiff(expression(lines1()), "lines-1", dev="png")

plotdiffResult()
