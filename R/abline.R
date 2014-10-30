
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
    h <- ty(x[[4]], par)
    v <- tx(x[[5]], par)
    untf <- x[[6]]
    col <- FixupCol(x[[7]], NA)
    lty <- FixupLty(x[[8]], par$lty)
    lwd <- FixupLwd(x[[9]], par$lwd)
    if (!is.null(a)) {
        if (is.null(b)) {
            a <- a[1]
            b <- a[2]
        }
        xx <- par$usr[1:2]
        if (untf && (par$xlog || par$ylog)) {
            # TODO: draw a curve instead of straight line
        } else {
            if (par$xlog) {
                xx <- log10(xx)
            }
            yy <- a + b*xx
            if (par$ylog) {
                yy <- 10^yy
            }
        }
        # TODO: this will have to be smarter to handle drawing outside
        #       the plot region
        grid.lines(xx, yy, default.units="native",
                   gp=gpar(col=col, lty=lty, lwd=lwd),
                   name=grobname("abline-ab"))
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
