## testing function
## sin surface
library(gridGraphics)
testPersp = function(theta=120, phi = 20, expand = 0.5, col = 'White',
                        box = TRUE, border = 'orange', 
                        ticktype = 'simple', nticks = 5, ...) {
    x = seq(-10,10,length = 30)
    y = seq(-10,10,length = 30)
    f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
    z <- outer(x, y, f)
    z[is.na(z)] <- 1
    par(mar = c(2,2,2,2))
    persp(x, y, z, theta = theta, 
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
  persp(x, y, z, theta = theta, 
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
  jet.colors <- colorRampPalette( c("yellow",'gold', "orange") )
  nbcol <- 100
  color <- jet.colors(nbcol)
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  facetcol <- cut(zfacet, nbcol)
  
  par(mar = c(2,2,2,2))
  expand = 0.5
  persp(x, y, z, theta = theta, 
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


testPersp4 = function(theta=120, phi = 20, expand = 0.5, col = 'orange ',
                      box = TRUE, border = 'NA', 
                      ticktype = 'simple', nticks = 5, ...) {
  x = seq(-15,15,length = 45)
  y = seq(-15,15,length = 45)
  f <- function(x, y) { (25 - (10 - sqrt(x^2 + y^2))^2)}
  z <- outer(x, y, f)
  nrz <- nrow(z)
  ncz <- ncol(z)
  # color
  jet.colors <- colorRampPalette( c("yellow",'gold', "orange") )
  nbcol <- 100
  color <- jet.colors(nbcol)
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  facetcol <- cut(zfacet, nbcol)
  
  par(mar = c(2,2,2,2))
  persp(x, y, z, theta = theta, 
        phi = phi, expand = expand, 
        #col = color[facetcol], 
        box = box, border = border, col = col,
        ticktype = ticktype, nticks = nticks, ...)
  
}
testPersp4(border = 'gray', expand = 0.5, col = 'NA')
## bugs
## calling shade when some z are NA
# testPersp2(border = 'NA', shade = 0.2)
# grid.echo()
## alpha of rgb less than 0
# testPersp(col = rgb(0.5, 0.2, 0.3, 0.8), shade = 0.5, border = NA)
# grid.echo()



#plotdiff(expression(testPersp3()), 'volcano', antialias = FALSE)


## test on theta
	plotdiff(expression(testPersp(30)), 'persp-01')

## test on phi
	plotdiff(expression(testPersp(phi = 5)), 'persp-02')

## test on expand
	plotdiff(expression(testPersp(expand = 0.01)), 'persp-03')

## test on lim
	plotdiff(expression(testPersp(xlim = c(-5,5))), 'persp-04')

## test on label
plotdiff(expression(
  testPersp(xlab = 'just a label of x', 
            ylab = 'just a label of y',
            zlab = 'just a label of z')), 'persp-05')

## test on r
plotdiff(expression(testPersp(r = 10)), 'persp-06')

##test on d
plotdiff(expression(testPersp(d = 0.2)), 'persp-07')


## test on scale
plotdiff(expression(testPersp(scale = FALSE)), 'persp-08')

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

## test on lty
plotdiff(expression(testPersp(lty = 'dotted')), 'persp-18')
    
## test on lwd
plotdiff(expression(testPersp(lwd = 3)), 'persp-21')

    
plotdiff(expression(testPersp(col = 'orange', border = 'NA', 
                              shade =0.5, box = TRUE, 
                              scale = TRUE)), 'persp-22')
plotdiff(expression(testPersp(col = 1:10, border = 'NA', 
                              shade =0.5, box = TRUE, 
                              scale = TRUE)), 'persp-23')



plotdiff(expression(testPersp1(box = FALSE)), 'persp-sin2')
plotdiff(expression(testPersp2(box = FALSE)), 'persp-Torus')
            
## new test
unlikelyTest = function(i)
{
  x = 1:3
  y = 1:3
  z = outer(x, y, "+")
  z[1,1] = NA
  cols = list(
    col1 = c('NA', 'red', 'blue', 'brown'),
    col2 = c('red', 'NA', 'blue', 'brown')
  )
  persp(z, col = cols[[i]], shade = 0.5)
}
## missing value on Z with:
##first color is missing when shading
plotdiff(expression(unlikelyTest(1)), 'persp-unlike-1')
##include missing color when shadding
plotdiff(expression(unlikelyTest(2)), 'persp-unlike-2')


otherTest1 = function()
{
  par(bg = "white")
  x <- seq(-1.95, 1.95, length = 30)
  y <- seq(-1.95, 1.95, length = 35)
  z <- outer(x, y, function(a, b) a*b^2)
  nrz <- nrow(z)
  ncz <- ncol(z)
  # Create a function interpolating colors in the range of specified colors
  jet.colors <- colorRampPalette( c("blue", "green") )
  # Generate the desired number of colors from this palette
  nbcol <- 100
  color <- jet.colors(nbcol)
  # Compute the z-value at the facet centres
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  # Recode facet z-values into color indices
  facetcol <- cut(zfacet, nbcol)
  persp(x, y, z, col = color[facetcol], phi = 30, theta = -30)
}
plotdiff(expression(otherTest1()), 'persp-other-1')


otherTest2 = function()
{
  z <- 2 * volcano
  x <- 10 * (1:nrow(z)) 
  y <- 10 * (1:ncol(z))
  z0 = min(z) - 20
  
  z = rbind(z0, cbind(z0, z, z0), z0)
  x = c(min(x) - 1e-10, x, max(x) + 1e-10)
  y = c(min(y) - 1e-10, y, max(y) + 1e-10)
  fill = matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
  fill[ , i2 <- c(1,ncol(fill))] <- "gray"
  fill[i1 <- c(1,nrow(fill)) , ] <- "gray"
  zi = volcano[ -1,-1] + volcano[ -1,-61] +
                volcano[-87,-1] + volcano[-87,-61]
  fcol <- fill
  fcol[-i1,-i2] =
       terrain.colors(20)[cut(zi,
                               stats::quantile(zi, seq(0,1, length.out = 21)),
                               include.lowest = TRUE)]
  persp(x, y, 2*z, theta = 110, phi = 40, col = fcol, scale = FALSE,
             ltheta = -120, shade = 0.4, border = NA, box = FALSE)
}
plotdiff(expression(otherTest2()), 'persp-other-2')
