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
}


f4 = function()
{
  x = y = seq(-4*pi, 4*pi, len = 60)
  r = sqrt(outer(x^2, y^2, "+"))
  filled.contour(cos(r^2)*exp(-r/(2*pi)), axes = FALSE, color = heat.colors)
}

f5 = function()
{
  x = y = seq(-1, 1, len = 30)
  r = log(outer(x^2, y^2, "+"))
  filled.contour(cos(r^2)*exp(-r/(2*pi)), axes = FALSE, color = rainbow)
}



f6 = function()
{
  d = c(0.4, 0.4, 0.7,-1.0,-0.1, 0.6,-0.4, 0.6,-0.4, 1.3, 0.7,-0.4, 1.1, 1.3, 0.6, 0.1,-0.0,-0.8,-0.8,-1.0 ,
           0.4,-0.4, 0.4,-1.2,-0.7, 0.4,-0.9, 0.5,-0.9, 1.2, 0.5,-1.0, 1.3, 1.1, 0.5,-0.0,-0.1,-1.2,-1.0,-0.9 ,
           0.7, 0.4, 0.1,-1.2,-0.2, 0.5,-0.6, 0.6,-0.2, 0.9, 0.6,-0.5, 1.1, 0.8, 0.6, 0.1,-0.4,-0.9,-0.7,-0.8 ,
           -1.0,-1.2,-1.2,-4.4,-1.9,-0.8,-2.2,-1.0,-2.2, 0.0,-0.3,-2.0,-0.2, 0.2,-0.8,-1.6,-1.9,-2.4,-2.3,-2.6 ,
           -0.1,-0.7,-0.2,-1.9,-2.0,-0.5,-1.9,-0.3,-1.7, 0.4,-0.2,-1.9, 0.3, 0.4,-0.3,-0.8,-0.9,-2.1,-1.8,-2.0 ,
           0.6, 0.4, 0.5,-0.8,-0.5,-0.1,-0.8, 0.6,-0.5, 1.0, 0.5,-0.7, 0.8, 1.0, 0.5, 0.1,-0.3,-0.9,-0.7,-1.1 ,
           -0.4,-0.9,-0.6,-2.2,-1.9,-0.8,-2.7,-0.6,-2.0, 0.3,-0.3,-2.3,-0.0,-0.0,-0.6,-1.1,-1.3,-2.4,-2.0,-2.2 ,
           0.6, 0.5, 0.6,-1.0,-0.3, 0.6,-0.6, 0.1,-0.8, 1.3, 0.8,-0.8, 1.1, 1.3, 0.4, 0.1, 0.1,-0.8,-1.0,-1.0 ,
           -0.4,-0.9,-0.2,-2.2,-1.7,-0.5,-2.0,-0.8,-2.9, 0.3,-0.4,-2.2,-0.0,-0.0,-0.7,-0.7,-1.3,-2.4,-2.1,-2.6 ,
           1.3, 1.2, 0.9, 0.0, 0.4, 1.0, 0.3, 1.3, 0.3, 1.1, 1.0, 0.2, 0.7, 1.9, 0.9,-0.2, 0.3, 0.1,-0.4,-0.2 ,
           0.7, 0.5, 0.6,-0.3,-0.2, 0.5,-0.3, 0.8,-0.4, 1.0, 0.3,-0.3, 1.0, 1.1, 0.6, 0.1, 0.3,-0.7,-0.5,-0.6 ,
           -0.4,-1.0,-0.5,-2.0,-1.9,-0.7,-2.3,-0.8,-2.2, 0.2,-0.3,-2.7, 0.0,-0.0,-0.6,-1.0,-1.1,-2.3,-2.1,-2.4 ,
           1.1, 1.3, 1.1,-0.2, 0.3, 0.8,-0.0, 1.1,-0.0, 0.7, 1.0, 0.0, 1.6, 0.8, 1.0, 0.8, 0.7,-0.2,-0.2,-0.2 ,
           1.3, 1.1, 0.8, 0.2, 0.4, 1.0,-0.0, 1.3,-0.0, 1.9, 1.1,-0.0, 0.8, 1.2, 1.1, 0.0, 0.2,-0.1,-0.4, 0.0 ,
           0.6, 0.5, 0.6,-0.8,-0.3, 0.5,-0.6, 0.4,-0.7, 0.9, 0.6,-0.6, 1.0, 1.1,-0.2, 0.1,-0.0,-0.9,-0.6,-1.2 ,
           0.1,-0.0, 0.1,-1.6,-0.8, 0.1,-1.1, 0.1,-0.7,-0.2, 0.1,-1.0, 0.8, 0.0, 0.1,-0.6,-0.4,-1.2,-1.3,-1.4 ,
           -0.0,-0.1,-0.4,-1.9,-0.9,-0.3,-1.3, 0.1,-1.3, 0.3, 0.3,-1.1, 0.7, 0.2,-0.0,-0.4,-1.3,-1.4,-1.6,-1.9 ,
           -0.8,-1.2,-0.9,-2.4,-2.1,-0.9,-2.4,-0.8,-2.4, 0.1,-0.7,-2.3,-0.2,-0.1,-0.9,-1.2,-1.4,-3.0,-2.3,-2.5 ,
           -0.8,-1.0,-0.7,-2.3,-1.8,-0.7,-2.0,-1.0,-2.1,-0.4,-0.5,-2.1,-0.2,-0.4,-0.6,-1.3,-1.6,-2.3,-2.3,-2.4 ,
           -1.0,-0.9,-0.8,-2.6,-2.0,-1.1,-2.2,-1.0,-2.6,-0.2,-0.6,-2.4,-0.2, 0.0,-1.2,-1.4,-1.9,-2.5,-2.4,-3.3 )
  d = matrix(d, nr = 20)
  filled.contour(d, axes = FALSE, color = heat.colors)
}

f7 = function()
{
  sy = function(x)  .08 * x * 1./ sqrt(1. + .0001 * x)
  sz = function(x)  .06 * x * 1./ sqrt(1. + .0015 * x)
  x = seq(100,10000,200)
  y = seq(-500,500,50)
  ubar =   5.      # mean wind speed
  height = 30.      # stack height (in m!)
  qout =   1.0e6  # discharge ug/s
  gpm = function(a,b)
  {
    # the input variable "a" is X, the downwind distance
    # the input variable "b" is y, the crosswind distance
    # Model Input Parameters
    ubar =   5.      # mean wind speed
    height = 30.      # stack height (in m!)
    qout =   1.0e6  # discharge ug/s
    # Here is the actual GPM for ground level concentrations
    conc = qout / (pi * sy(a) * sz(a) * ubar) * 
      exp(-1*(b*b/(2.*sy(a)*sy(a))+height*height/(2.*sz(a)*sz(a))))
    # tell the function to return "conc" as its output
    return(conc)
  }
  conc = outer(x,y,gpm)
  filled.contour(x,y,conc, nlevels=20, col=rainbow(100),
                 xlab="Downwind distance (m)", ylab="Crosswind distance (m)",
                 main="Contours of Pollutant Concentration (ug/m^3)")
}

f8 = function()
{
  x <- y <- seq(-8*pi, 8*pi, len = 40)
  r <- sqrt(outer(x^2, y^2, "+"))
  filled.contour(cos(r^2)*exp(-r/(2*pi)), 
                 axes=FALSE,
                 color.palette=heat.colors,
                 asp=1)
}

## length(color) > nlevels
f9 = function()
{
  x <- y <- seq(-8*pi, 8*pi, len = 40)
  r <- sqrt(outer(x^2, y^2, "+"))
  filled.contour(cos(r^2)*exp(-r/(2*pi)), 
                 axes=FALSE,
                 col = heat.colors(100), nlevels = 35,
                 asp=1)
}

## length(color) < nlevels
f10 = function()
{
  x <- y <- seq(-8*pi, 8*pi, len = 40)
  r <- sqrt(outer(x^2, y^2, "+"))
  filled.contour(cos(r^2)*exp(-r/(2*pi)), 
                 axes=FALSE,
                 col = heat.colors(6), nlevels = 35,
                 asp=1)
}

plotdiff(expression(f1()), "filled.contour-01")
plotdiff(expression(f2()), "filled.contour-02")
plotdiff(expression(f3()), "filled.contour-03")
plotdiff(expression(f4()), "filled.contour-04")
plotdiff(expression(f5()), "filled.contour-05")
plotdiff(expression(f6()), "filled.contour-06")
plotdiff(expression(f7()), "filled.contour-07")
plotdiff(expression(f8()), "filled.contour-08")
plotdiff(expression(f9()), "filled.contour-09")
plotdiff(expression(f10()), "filled.contour-10")
plotdiffResult()
