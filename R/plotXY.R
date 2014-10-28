
points <- function(x, y, pch, lty, col, bg, cex, lwd,
                   cin) {
    grid.points(x, y, default.units="native",
                #  GSTR_0  dpptr(dd)->scale * dd->dev->cra[1] * 0.5 * dd->dev->ipr[1] * gpptr(dd)->cex
                size=unit(cin[2]*0.5*cex, "in"), pch=pch,
                gp=gpar(lty=lty, col=col, fill=bg, lwd=lwd),
                name=grobname("points"))
}

lines <- function(x, y, lty, col, lwd) {
    grid.lines(x, y, default.units="native",
               gp=gpar(lty=lty, col=col, lwd=lwd),
               name=grobname("lines"))
}

# C_plotXY(xy, type, pch, lty, col, bg, cex, lwd, ...)
C_plotXY <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:9)])
    dev.set(playDev())
    # TODO:  handle 'type'
    depth <- gotovp(par$xpd)
    xx <- x[[2]]$x
    yy <- x[[2]]$y
    type <- x[[3]]
    pch <- x[[4]]
    lty <- x[[5]]
    col <- x[[6]]
    bg <- x[[7]]
    # NOTE: cex multiplied by "base" cex
    cex <- x[[8]]*par$cex
    lwd <- x[[9]]
    switch(type,
           p=points(xx, yy, pch, lty, col, bg, cex, lwd, par$cin),
           l=lines(xx, yy, lty, col, lwd))
    upViewport(depth)
}

