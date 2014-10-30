
# arrows(x0, y0, x1, y1, length, angle, code, col, lty, lwd, ...) 

C_arrows <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:11)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    x0 <- x[[2]]
    y0 <- x[[3]]
    x1 <- x[[4]]
    y1 <- x[[5]]
    length <- x[[6]]
    angle <- x[[7]]
    code <- x[[8]]
    col <- FixupCol(x[[9]], NA)
    lty <- FixupLty(x[[10]], par$lty)
    lwd <- FixupLwd(x[[11]], par$lwd)
    grid.segments(x0, y0, x1, y1, default.units="native",
                  gp=gpar(col=col, lty=lty, lwd=lwd, lineend=par$lend),
                  arrow=arrow(angle=angle, length=unit(length, "in"),
                      ends=switch(code, "first", "last", "both")),
                  name=grobname("arrows"))
    upViewport(depth)
}
