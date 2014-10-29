
# C_clip(x1, x2, y1, y2)

# Just record this clipping setting and enforce it whenever subsequently
# descend into window viewport
C_clip <- function(x) {
    x1 <- x[[2]]
    x2 <- x[[3]]
    y1 <- x[[4]]
    y2 <- x[[5]]
    setClip(x1, y1, x2 - x1, y2 - y1)
}

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
    depth <- downViewport(path, strict=TRUE)
    if (end == "window" && !is.null(clipRegion <- getClip())) {
        grid.clip(clipRegion[1], clipRegion[2], clipRegion[3], clipRegion[4],
                  default.units="native", just=c("left", "bottom"),
                  name=grobname("clip"))
    }
    depth
}


