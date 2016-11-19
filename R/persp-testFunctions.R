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
                   
    trans
}

echoTest = function(trans = trans) {
    plot = recordPlot()
    plotInfo = perInit(plot, trans = trans, newpage = FALSE)
    C_persp(plot = plotInfo)
    grid.text('grid', 0.1, 0.9)
}