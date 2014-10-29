
# C_path(x, y, lengths, rule, col, border, lty, ...)

C_path <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:8)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    xx <- x[[2]]
    yy <- x[[3]]
    lengths <- x[[4]]
    rule <- x[[5]]
    col <- x[[6]]
    border <- x[[7]]
    lty <- x[[8]]
    grid.path(xx, yy, default="native",
              id.lengths=lengths, rule=rule,
              gp=gpar(col=border, fill=col, lty=lty),
              name=grobname("path"))
    upViewport(depth)    
}
