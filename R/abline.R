
# C_abline(a, b, h, v, untf, col, lty, lwd, ...)
C_abline <- function(x) {
    # TODO: handle 'a'
    # TODO: handle 'b'
    # TODO: handle 'untf'
    dev.set(recordDev())
    par <- currentPar(x[-(1:9)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    h <- x[[4]]
    v <- x[[5]]
    col <- x[[7]]
    lty <- x[[8]]
    lwd <- x[[9]]
    if (!is.null(h)) {
        grid.segments(0, unit(h, "native"), 1, unit(h, "native"),
                      gp=gpar(col=col, lty=lty, lwd=lwd),
                      name=grobname("abline-h"))
    }
    if (!is.null(v)) {
        grid.segments(unit(v, "native"), 0, unit(v, "native"), 1, 
                      gp=gpar(col=col, lty=lty, lwd=lwd),
                      name=grobname("abline-v"))
    }
    upViewport(depth)
}
