
# C_abline(a, b, h, v, untf, col, lty, lwd, ...)
C_abline <- function(x) {
    # TODO: handle 'a'
    # TODO: handle 'b'
    # TODO: handle 'untf'
    dev.set(recordDev())
    par <- currentPar(x[-(1:9)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    a <- x[[2]]
    b <- x[[3]]
    h <- x[[4]]
    v <- x[[5]]
    untf <- x[[6]]
    col <- FixupCol(x[[7]], NA)
    lty <- FixupLty(x[[8]], par$lty)
    lwd <- FixupLwd(x[[9]], par$lwd)
    if (!is.null(a)) {
        if (is.null(b)) {
            a <- a[1]
            b <- a[2]
        }
        # TODO: handle "log" axes
        # TODO: this may have to be smarter to handle drawing outside
        #       the plot region
        grid.abline(a, b, gp=gpar(col=col, lty=lty, lwd=lwd))
    }
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
