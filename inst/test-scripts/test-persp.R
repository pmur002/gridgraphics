## testing function
## sin surface
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


testPersp()
grid.echo()
plotdiff(expression(testPersp()), 'sin')

testPersp1()
grid.echo()
plotdiff(expression(testPersp1()), 'sin2')

testPersp2()
grid.echo()
plotdiff(expression(testPersp2()), 'Torus')
system('compare Torus-graphics.pdf Torus-grid.pdf diff.pdf')

## test on theta
	 
	plotdiff(expression(testPersp(30)), 'persp-2')
	plotdiff(expression(testPersp(175)), 'persp-3')
	plotdiff(expression(testPersp(285)), 'persp-4')

## test on phi
	plotdiff(expression(testPersp(phi = 15)), 'persp-5')
	plotdiff(expression(testPersp(phi = 22.5)), 'persp-6')
	plotdiff(expression(testPersp(phi = 80)), 'persp-7')
	plotdiff(expression(testPersp(phi = 310)), 'persp-8')
## test on expand
	plotdiff(expression(testPersp(epand = 0.01)), 'persp-9')
	plotdiff(expression(testPersp(epand = 0.2)), 'persp-10')
	plotdiff(expression(testPersp(epand = 2)), 'persp-11')
	plotdiff(expression(testPersp(epand = 10)), 'persp-12')
## test on lim
	plotdiff(expression(testPersp(xlim = c(-5,5))), 'persp-13')
  plotdiff(expression(testPersp(ylim = c(-5,5))), 'persp-14')
  plotdiff(expression(testPersp(zlim = c(-5,5))), 'persp-15')

## test on label
plotdiff(expression(
  testPersp(xlab = 'xx', ylab = 'yy', zlab = 'zz')), 'persp-16')

## test on r
plotdiff(expression(testPersp(r = 5)), 'persp-17')
plotdiff(expression(testPersp(r = 10)), 'persp-18')
plotdiff(expression(testPersp(r = 20)), 'persp-19')


##test on d
plotdiff(expression(testPersp(d = 0.5)), 'persp-20')
plotdiff(expression(testPersp(d = 0)), 'persp-21')
plotdiff(expression(testPersp(d = 5)), 'persp-22')


## test on scale
plotdiff(expression(testPersp(scale = FALSE)), 'persp-23')
plotdiff(expression(testPersp(scale = TRUE)), 'persp-24')

## test on expand
plotdiff(expression(testPersp(expand = 0.5)), 'persp-25')
plotdiff(expression(testPersp(expand = 1)), 'persp-26')
plotdiff(expression(testPersp(expand = 2)), 'persp-27')

## test on col
plotdiff(expression(testPersp(col = 1)), 'persp-28')
plotdiff(expression(testPersp(col = 'blue')), 'persp-29')
plotdiff(expression(testPersp(col = 1:5)), 'persp-30')
plotdiff(expression(testPersp(col = 1:10)), 'persp-31')
plotdiff(expression(testPersp(col = 1:100)), 'persp-32')

## test on border
plotdiff(expression(testPersp(border = 1)), 'persp-33')
plotdiff(expression(testPersp(border = 'brown')), 'persp-34')
# only one color for border
plotdiff(expression(testPersp(border = 5:6)), 'persp-35')

## test on axes
plotdiff(expression(testPersp(axes = TRUE)), 'persp-36')
plotdiff(expression(testPersp(axes = FALSE)), 'persp-37')

## if box = False then not drawing any axes even axes = TRUE
plotdiff(expression(testPersp(box = FALSE, axes = TRUE)), 'persp-38')
plotdiff(expression(testPersp(box = FALSE, axes = FALSE)), 'persp-39')

## test on ticktype
plotdiff(expression(
  testPersp(ticktype = 'detail', axes = TRUE, box = TRUE)), 'persp-40')

## test on lty
plotdiff(expression(testPersp(lty = 'dotted')), 'persp-41')
plotdiff(expression(testPersp(lty = '1331')), 'persp-42')
    
    
## test on lwd
plotdiff(expression(testPersp(lwd = 2)), 'persp-43')
plotdiff(expression(testPersp(lwd = 3)), 'persp-44')

## other bugs:
    ## 1. pty = 's'
    
## test on shade
    ## single colors
plotdiff(expression(testPersp(col = 'orange', border = 'NA', 
                              shade =0.2, box = FALSE, 
                              scale = TRUE)), 'persp-45')
plotdiff(expression(testPersp(col = 1:10, border = 'NA', 
                              shade =0.2, box = FALSE, 
                              scale = TRUE)), 'persp-46')
plotdiff(expression(testPersp(col = 1:10, border = 'NA', 
                              shade =0.2, box = FALSE, 
                              scale = FALSE)), 'persp-47')

## col.lab/cex.lab/col.axis
plotdiff(expression(testPersp(col.lab = 'red', cex.lab = 1.5, 
                              col.axis = 'green', ticktype = 'detail')), 
         'persp-48')

name.graphics = paste('persp-', 1:48, '-graphics.png', sep = '')
name.grid = paste('persp-', 1:48, '-grid.png', sep= '')
name.out = paste('out-', 1:48, '.png', sep = '')
cmd = paste('compare', name.graphics, name.grid, name.out, sep = ' ')
for(i in 1:48)
{
  system(cmd[i])
}

