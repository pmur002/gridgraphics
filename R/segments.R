
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
    col <- FixupCol(x[[6]], NA)
    lty <- FixupLty(x[[7]], par$lty)
    lwd <- FixupLwd(x[[8]], par$lwd)
    grid.segments(x0, y0, x1, y1, default.units="native",
                 gp=gpar(col=col, lty=lty, lwd=lwd, lineend=par$lend),
                 name=grobname("segments"))
    upViewport(depth)    
}
