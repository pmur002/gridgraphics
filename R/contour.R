
# contour(x, y, z, levels, labels, labcex, drawlabels, method,
#         vfont, col, lty, lwd)

C_contour <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:13)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    xx <- x[[2]]
    yy <- x[[3]]
    zz <- x[[4]]
    levels <- x[[5]]
    drawlabels <- x[[8]]
    col <- x[[11]]
    lty <- x[[12]]
    lwd <- x[[13]]
    if (drawlabels) {
        warning("gridGraphics cannot emulate labels on contour lines")
    }
    clines <- contourLines(xx, yy, zz, levels=levels)
    if (length(clines)) {
        for (i in 1:length(clines)) {
            c <- clines[[i]]
            grid.lines(c$x, c$y, default.units="native",
                       gp=gpar(col=col, lty=lty, lwd=lwd),
                       name=paste(grobname("contour"), i, sep="-"))
        }
    }
    upViewport(depth)    
}
