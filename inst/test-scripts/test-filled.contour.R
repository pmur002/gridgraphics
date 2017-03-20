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

plotdiff(expression(f1()), "f11")
plotdiff(expression(f2()), "f21")


plotdiffResult()