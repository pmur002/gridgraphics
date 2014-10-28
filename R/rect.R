
# C_rect(xleft, ybottom, xright, ytop, col, border, lty, lwd, ...)

C_rect <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:9)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    xleft <- x[[2]]
    ybottom <- x[[3]]
    xright <- x[[4]]
    ytop <- x[[5]]
    col <- x[[6]]
    border <- x[[7]]
    lty <- x[[8]]
    lwd <- x[[9]]
    # TODO:  convert from "native" BEFORE determining width?
    #        (so that it works in 'log' coordinates?)
    grid.rect(xleft, ybottom, xright - xleft, ytop - ybottom,
              default.units="native", just=c("left", "bottom"),
              gp=gpar(col=border, fill=col, lty=lty, lwd=lwd),
              name=grobname("rect"))
    upViewport(depth)
}
