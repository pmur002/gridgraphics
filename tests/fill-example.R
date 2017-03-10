## filled countour
ff = function(){
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
a <- expand.grid(1:20, 1:20)
b <- matrix(a[,1] + a[,2], 20)
filled.contour(x = 1:20, y = 1:20, z = b,
               plot.axes = { axis(1); axis(2); points(10, 10) })

}

source('H:/New folder/gridgraphics/tests/loading.R')
ff()
grid.echo()
