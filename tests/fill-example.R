## filled countour
ff = function(){
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
a <- expand.grid(1:20, 1:20)
b <- matrix(a[,1] + a[,2], 20)
filled.contour(x = 1:20, y = 1:20, z = b)

}

source('C:/Users/yeamin/Desktop/master/MasterProject/gridGraphics/gridgraphics/tests/loading.R')
system.time(ff())
system.time(grid.echo())




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
system.time(grid.echo())



## Persian Rug Art:
x <- y <- seq(-4*pi, 4*pi, len = 100)
r <- sqrt(outer(x^2, y^2, "+"))
filled.contour(cos(r^2)*exp(-r/(2*pi)), axes = FALSE)
## rather, the key *should* be labeled:
filled.contour(cos(r^2)*exp(-r/(2*pi)), frame.plot = FALSE,
               plot.axes = {})

tmp <- tempfile()
Rprof(tmp)
grid.echo()
Rprof(NULL)
summaryRprof(tmp)
