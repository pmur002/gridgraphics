
# C_box(which, lty, ...)
C_box <- function(x) {
    dev.set(recordDev())
    # NOTE: although 'lty' is passed in, it is not explicitly handled
    par <- currentPar(x[-(1:2)])
    dev.set(playDev())
    # TODO: Handle 'which' and 'bty'
    depth <- gotovp(NA, "plot")
    # NOTE: copy GBox which draws *polygon* (not rect) AND
    #       explicitly sets fill to NA
    xy <- switch(par$bty,
                 "o"=,
                 "O"=list(x=c(0, 1, 1, 0), y=c(0, 0, 1, 1)),
                 "l"=,
                 "L"=list(x=c(0, 0, 1), y=c(1, 0, 0)),
                 "7"=list(x=c(0, 1, 1), y=c(1, 1, 0)),
                 "c"=,
                 "C"=,
                 "["=list(x=c(1, 0, 0, 1), y=c(1, 1, 0, 0)),
                 "]"=list(x=c(0, 1, 1, 0), y=c(1, 1, 0, 0)),
                 "u"=,
                 "U"=list(x=c(0, 0, 1, 1), y=c(1, 0, 0, 1)))
    if (par$bty %in% c("n", "N")) {
        # do nothing
    } else if (par$bty %in% c("o", "O")) {
        grid.polygon(xy$x, xy$y,
                     gp=gpar(col=par$col, lty=par$lty, lwd=par$lwd, fill=NA),
                     name=grobname("box"))
    } else {
        grid.lines(xy$x, xy$y,
                   gp=gpar(col=par$col, lty=par$lty, lwd=par$lwd),
                   name=grobname("box"))
    }
    upViewport(depth)
}

