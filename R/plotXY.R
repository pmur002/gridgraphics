
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

step <- function(x, y, lty, col, lwd) {
    n <- length(x)
    grid.lines(rep(x, each=2)[-1],
               rep(y, each=2, length.out=2*n - 1),
               default.units="native",
               gp=gpar(lty=lty, col=col, lwd=lwd),
               name=grobname("step"))
}

bar <- function(x, y, lty, col, lwd, ylog, usr) {
    if (ylog) {
        root <- usr[3]
    } else {
        root <- 0
    }
    grid.segments(x, root, x, y, default.units="native",
                  gp=gpar(lty=lty, col=col, lwd=lwd),
                  name=grobname("spike"))
}

brokenlines <- function(x, y, lty, col, lwd, par) {
    d <- 0.5*par$cin[2]*par$cex
    xx <- convertX(unit(x, "native"), "in", valueOnly=TRUE)
    yy <- convertY(unit(y, "native"), "in", valueOnly=TRUE)
    dx <- diff(xx)
    dy <- diff(yy)
    hypot <- sqrt(dx^2 + dy^2)
    f <- d/hypot
    n <- length(x)
    sx <- xx[-n] + f*dx
    sy <- yy[-n] + f*dy
    ex <- xx[-1] - f*dx
    ey <- yy[-1] - f*dy
    grid.segments(sx, sy, ex, ey, 
                  default.units="in",
                  gp=gpar(lty=lty, col=col, lwd=lwd),
                  name=grobname("brokenline"))
}

# C_plotXY(xy, type, pch, lty, col, bg, cex, lwd, ...)
C_plotXY <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:9)])
    dev.set(playDev())
    # TODO:  handle all 'type's
    depth <- gotovp(par$xpd)
    xx <- tx(x[[2]]$x, par)
    yy <- ty(x[[2]]$y, par)
    type <- x[[3]]
    pch <- FixupPch(x[[4]], par$pch)
    lty <- FixupLty(x[[5]], par$lty)
    col <- FixupCol(x[[6]], 0, par$bg)
    bg <- FixupCol(x[[7]], NA, par$bg)
    # NOTE: cex multiplied by "base" cex
    cex <- FixupCex(x[[8]]*par$cex, 1)
    lwd <- FixupLwd(x[[9]], par$lwd)
    switch(type,
           p=points(xx, yy, pch, lty, col, bg, cex, lwd, par$cin),
           l=lines(xx, yy, lty, col, lwd),
           s=step(xx, yy, lty, col, lwd),
           h=bar(xx, yy, lty, col, lwd, par$ylog, par$usr),
           b={ brokenlines(xx, yy, lty, col, lwd, par);
               points(xx, yy, pch, lty, col, bg, cex, lwd, par$cin) })
    upViewport(depth)
}

