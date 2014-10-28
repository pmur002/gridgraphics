
# Navigate to the correct viewport based on 'xpd' setting
# End up in either "plot" or "window" viewport

gotovp <- function(xpd, end="window") {
    root <- vpname("root")
    inner <- vpname("inner")
    if (is.na(xpd)) {
        figure <- vpname("figure")
        plot <- vpname("plot")
        window <- vpname("window")
    } else if (xpd) {
        figure <- vpname("figure", clip=TRUE)
        plot <- vpname("plot")
        window <- vpname("window")
    } else {
        figure <- vpname("figure")
        plot <- vpname("plot", clip=TRUE)
        window <- vpname("window", clip=TRUE)
    }
    path <- switch(end,
                   window=vpPath(root, inner, figure, plot, window),
                   plot=vpPath(root, inner, figure, plot),
                   inner=vpPath(root, inner))
    downViewport(path, strict=TRUE)
}


