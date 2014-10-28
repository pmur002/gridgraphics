
# image(x, y, z, col, breaks)

C_image <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:6)])
    dev.set(playDev())
    # NOTE: Deliberately override par$xpd
    depth <- gotovp(FALSE)
    xx <- x[[2]]
    yy <- x[[3]]
    zz <- x[[4]]
    col <- x[[5]]
    # NOTE: 'breaks' not used
    # NOTE: 'z' is index into 'col'
    nx <- length(xx)
    ny <- length(yy)
    xxx <- rep(xx[-nx], (ny - 1))
    www <- rep(diff(xx), (ny - 1))
    yyy <- rep(yy[-ny], each=(nx - 1))
    hhh <- rep(diff(yy), each=(nx - 1))
    zzz <- zz + 1
    zzz[zz < 1 & zz > length(col)] <- NA
    grid.rect(xxx, yyy, www, hhh,
              default.units="native",
              just=c("left", "bottom"),
              gp=gpar(col=NA, fill=col[zzz]))
    upViewport(depth)    
}
