
# C_box(which = which, lty = lty, ...)
C_box <- function(x) {
    dev.set(playDev())
    # TODO: Handle 'which' and 'bty'
    depth <- gotovp(NA, "plot")
    # NOTE: copy GBox which draws *polygon* (not rect) AND
    #       explicitly sets fill to NA
    grid.polygon(c(0, 1, 1, 0), c(0, 0, 1, 1),
                 gp=gpar(fill=NA), name=grobname("box"))
    upViewport(depth)
}

