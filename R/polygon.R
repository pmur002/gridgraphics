
# C_polygon(x, y, col, border, lty, ...)

C_polygon <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:6)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    xx <- x[[2]]
    yy <- x[[3]]
    col <- x[[4]]
    border <- x[[5]]
    lty <- x[[6]]
    grid.polygon(xx, yy, default.units="native",
                 gp=gpar(col=border, fill=col, lty=lty),
                 name=grobname("polygon"))
    upViewport(depth)
}

