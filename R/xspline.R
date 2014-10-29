
# C_xspline(x, y, s, open, repEnds, draw, col, border, ...)

C_xspline <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:9)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    xx <- x[[2]]
    yy <- x[[3]]
    s <- x[[4]]
    open <- x[[5]]
    repEnds <- x[[6]]
    draw <- x[[7]]
    col <- x[[8]]
    border <- x[[9]]
    if (draw) {
        grid.xspline(xx, yy, default.units="native",
                     shape=s, open=open, repEnds=repEnds,
                     gp=gpar(col=border, fill=col),
                     name=grobname("xspline"))
        result <- NULL
    }
    upViewport(depth)
}
