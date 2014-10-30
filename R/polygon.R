
# C_polygon(x, y, col, border, lty, ...)

C_polygon <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:6)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    xx <- tx(x[[2]], par)
    yy <- ty(x[[3]], par)
    col <- FixupCol(x[[4]], NA)
    border <- FixupCol(x[[5]], par$fg)
    lty <- FixupCol(x[[6]], par$lty)
    grid.polygon(xx, yy, default.units="native",
                 gp=gpar(col=border, fill=col, lty=lty),
                 name=grobname("polygon"))
    upViewport(depth)
}

