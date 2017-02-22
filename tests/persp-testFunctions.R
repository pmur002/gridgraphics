## testing function
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

testPersp1()


