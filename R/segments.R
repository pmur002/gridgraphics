
# C_segments(x0, y0, x1, y1, col=col, lty=lty, lwd=lwd, ...)

C_segments <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:8)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    x0 <- x[[2]]
    y0 <- x[[3]]
    x1 <- x[[4]]
    y1 <- x[[5]]
    col <- x[[6]]
    lty <- x[[7]]
    lwd <- x[[8]]
    grid.segments(x0, y0, x1, y1, default.units="native",
                 gp=gpar(col=col, lty=lty, lwd=lwd, lineend=par$lend),
                 name=grobname("segments"))
    upViewport(depth)    
}
