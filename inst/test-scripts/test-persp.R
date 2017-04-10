## testing function
## sin surface
library(gridGraphics)
testPersp = function(theta=120, phi = 20, expand = 0.5, col = 'White',
                        box = TRUE, border = 'orange', 
                        ticktype = 'simple', nticks = 5, ...) {
    x = seq(-10,10,length = 60)
    y = seq(-10,10,length = 60)
    f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
    z <- outer(x, y, f)
    z[is.na(z)] <- 1
    par(mar = c(2,2,2,2))
    trans <- persp(x, y, z, theta = theta, 
                   phi = phi, expand = expand, 
                   col = col, box = box, border = border, 
                   ticktype = ticktype, nticks = nticks, ...)
                   
}



## testing function
testPersp1 = function(theta=120, phi = 20, expand = 0.5, col = 'orange ',
                     box = TRUE, border = 'NA', 
                     ticktype = 'simple', nticks = 5, ...) {
  x = seq(-pi,pi,length = 45)
  y = seq(-pi,pi,length = 45)
  f <- function(x, y) { 1 + 3 * cos((x^2 + y^2) * 2) * exp(-(x^2 + y^2))}
  z <- outer(x, y, f)
  nrz <- nrow(z)
  ncz <- ncol(z)
  # color
  jet.colors <- colorRampPalette( c("white",'yellow', "orange") )
  nbcol <- 100
  color <- jet.colors(nbcol)
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  facetcol <- cut(zfacet, nbcol)
  
  par(mar = c(2,2,2,2))
  expand = 0.5
  trans <- persp(x, y, z, theta = theta, 
                 phi = phi, expand = expand, 
                 col = color[facetcol], box = box, border = border, 
                 ticktype = ticktype, nticks = nticks, ...)
  
}


testPersp2 = function(theta=120, phi = 20, expand = 0.5, col = 'orange ',
                      box = TRUE, border = 'NA', 
                      ticktype = 'simple', nticks = 5, ...) {
  x = seq(-1,1,length = 45)
  y = seq(-1,1,length = 45)
  f <- function(x, y) { (0.4^2-(0.6-(x^2+y^2)^0.5)^2)^0.5}
  z <- outer(x, y, f)
  nrz <- nrow(z)
  ncz <- ncol(z)
  # color
  jet.colors <- colorRampPalette( c("brown",'blue', "purple") )
  nbcol <- 100
  color <- jet.colors(nbcol)
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  facetcol <- cut(zfacet, nbcol)
  
  par(mar = c(2,2,2,2))
  expand = 0.5
  trans <- persp(x, y, z, theta = theta, 
                 phi = phi, expand = expand, 
                 col = color[facetcol], box = box, border = border, 
                 ticktype = ticktype, nticks = nticks, ...)
  
}

testPersp3 = function(){
    z <- 2 * volcano
    x <- 10 * (1:nrow(z))
    y <- 10 * (1:ncol(z))
    z0 <- min(z) - 20
    z <- rbind(z0, cbind(z0, z, z0), z0)
    x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
    y <- c(min(y) - 1e-10, y, max(y) + 1e-10)
    fill <- matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
    fill[ , i2 <- c(1,ncol(fill))] <- "gray"
    fill[i1 <- c(1,nrow(fill)) , ] <- "gray"
    fcol <- fill
    zi <- volcano[ -1,-1] + volcano[ -1,-61] +
    volcano[-87,-1] + volcano[-87,-61]  ## / 4
    fcol[-i1,-i2] <-
    terrain.colors(20)[cut(zi,
                           stats::quantile(zi, seq(0,1, length.out = 21)),
                           include.lowest = TRUE)]
    persp(x, y, 2*z, theta = 110, phi = 40, col = fcol, scale = FALSE,
    ltheta = -120, shade = 0.4, border = NA, box = FALSE)
}

plotdiff(expression(testPersp(box = FALSE)), 'sin')
plotdiff(expression(testPersp1(box = FALSE)), 'sin2')
plotdiff(expression(testPersp2(box = FALSE)), 'Torus')
plotdiff(expression(testPersp3()), 'volcano', antialias = FALSE)

#### other bugs on axis
plotdiff(expression(testPersp2(ticktype = 'detail')), 'volcano')

## test on theta
	plotdiff(expression(testPersp(30)), 'persp-1')

## test on phi
	plotdiff(expression(testPersp(phi = 15)), 'persp-2')

## test on expand
	plotdiff(expression(testPersp(epand = 0.01)), 'persp-3')

## test on lim
	plotdiff(expression(testPersp(xlim = c(-5,5))), 'persp-4')

## test on label
plotdiff(expression(
  testPersp(xlab = 'xx', ylab = 'yy', zlab = 'zz')), 'persp-5')

## test on r
plotdiff(expression(testPersp(r = 5)), 'persp-6')

##test on d
plotdiff(expression(testPersp(d = 0.5)), 'persp-7')


## test on scale
plotdiff(expression(testPersp(scale = FALSE)), 'persp-8')

## test on expand
plotdiff(expression(testPersp(expand = 0.5)), 'persp-9')


## test on multi-col
plotdiff(expression(testPersp(col = 1:5)), 'persp-10')


## test on border
plotdiff(expression(testPersp(border = 'brown')), 'persp-11')
# only one color for border
plotdiff(expression(testPersp(border = 5:6)), 'persp-12')

## test on axes
plotdiff(expression(testPersp(axes = TRUE)), 'persp-13')
plotdiff(expression(testPersp(axes = FALSE)), 'persp-14')

## if box = False then not drawing any axes even axes = TRUE
plotdiff(expression(testPersp(box = FALSE, axes = TRUE)), 'persp-15')
plotdiff(expression(testPersp(box = FALSE, axes = FALSE)), 'persp-16')

## test on ticktype
plotdiff(expression(
            testPersp(ticktype = 'detail', axes = TRUE, box = TRUE)), 'persp-17',
            antialias = FALSE)

## test on lty
plotdiff(expression(testPersp(lty = 'dotted')), 'persp-18')
plotdiff(expression(testPersp(lty = '1331')), 'persp-19')
    
    
## test on lwd
plotdiff(expression(testPersp(lwd = 2)), 'persp-20')
plotdiff(expression(testPersp(lwd = 3)), 'persp-21')

## other bugs:
    ## 1. pty = 's'
    
## test on shade
    ## single colors
plotdiff(expression(testPersp(col = 'orange', border = 'NA', 
                              shade =0.2, box = FALSE, 
                              scale = TRUE)), 'persp-22')
plotdiff(expression(testPersp(col = 1:10, border = 'NA', 
                              shade =0.2, box = FALSE, 
                              scale = TRUE)), 'persp-23')

#name.graphics = paste('persp-', 1:48, '-graphics.png', sep = '')
#name.grid = paste('persp-', 1:48, '-grid.png', sep= '')
#name.out = paste('out-', 1:48, '.png', sep = '')
#cmd = paste('compare', name.graphics, name.grid, name.out, sep = ' ')
#for(i in 1:48)
#{
#  system(cmd[i])
#}

