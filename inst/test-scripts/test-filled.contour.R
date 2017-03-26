library(gridGraphics)

f1 = function(){
    x = 10*1:nrow(volcano)
    y = 10*1:ncol(volcano)
    a = expand.grid(1:20, 1:20)
    b = matrix(a[,1] + a[,2], 20)
    filled.contour(x = 1:20, y = 1:20, z = b)
}



f2 = function(){
    x = y = seq(-4*pi, 4*pi, len = 30)
    r = sqrt(outer(x^2, y^2, "+"))
    filled.contour(cos(r^2)*exp(-r/(2*pi)), axes = FALSE)
}

f3 = function()
{
    x <- 10*1:nrow(volcano)
    y <- 10*1:ncol(volcano)
    filled.contour(x, y, volcano, color = terrain.colors,
                   plot.title = title(main = "The Topography of Maunga Whau",
                                      xlab = "Meters North", ylab = "Meters West"),
                   plot.axes = { axis(1, seq(100, 800, by = 100))
                     axis(2, seq(100, 600, by = 100)) },
                   key.title = title(main = "Height\n(meters)"),
                   key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1
    mtext(paste("filled.contour(.) from", R.version.string),
          side = 1, line = 4, adj = 1, cex = .66)
   # system.time(grid.echo())
}




plotdiff(expression(f1()), "f1")
plotdiff(expression(f2()), "f2")
plotdiff(expression(f3()), "f3")

plotdiffResult()