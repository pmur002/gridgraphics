
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
    grid.polygon(c(0, 1, 1, 0), c(0, 0, 1, 1),
                 gp=gpar(col=par$col, lty=par$lty, lwd=par$lwd, fill=NA),
                 name=grobname("box"))
    upViewport(depth)
}

