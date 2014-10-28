
setUpUsr <- function(usr) {
    incrementWindowIndex()
    windowvp <- viewport(xscale=usr[1:2], yscale=usr[3:4],
                         name=vpname("window"))
    windowvpclip <- viewport(xscale=usr[1:2], yscale=usr[3:4], clip=TRUE,
                             name=vpname("window", clip=TRUE)) 
    downViewport(vpPath(vpname("root"), vpname("inner"), vpname("figure"),
                        vpname("plot")), strict=TRUE)
    pushViewport(windowvp)
    upViewport(2)
    downViewport(vpname("plot", clip=TRUE), strict=TRUE)
    pushViewport(windowvpclip)
    upViewport(3)
    downViewport(vpPath(vpname("figure", clip=TRUE), vpname("plot")),
                 strict=TRUE)
    pushViewport(windowvp)
    upViewport(5)
}

# C_plot_window(xlim, ylim, log, asp, ...)
C_plot_window <- function(x) {
    dev.set(recordDev())
    do.call("plot.window", x[-1])
    usr <- par("usr")
    dev.set(playDev())
    # TODO: Need to handle 'log' axes !!!
    setUpUsr(usr)
}

