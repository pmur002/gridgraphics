
## A reasonably faithful translation of C code to allow
## for easy comparison of results when debugging
## (only some attempts to vectorise simple C loops)

## C_dend(n, merge, height, order, hang, labels, ...)
## C_dendwindow(n, merge, height, hang, labels, ...)

drawdend <- function(node, merge, height, order, labels, offset, hang) {
    y <- height[node]
    ## left part 
    k <- merge[node, 1]
    if (k > 0) {
        xy <- drawdend(k, merge, height, order, labels, offset, hang)
        xl <- xy$x
        yl <- xy$y
    } else {
	xl <- order[-k]
	yl <- if (hang >= 0) y - hang else 0
	if (!is.na(labels[-k]))
            grid.text(labels[-k],
                      xl,
                      yl - offset,
                      default.units="native",
                      hjust=1, vjust=.3,
                      rot=90)
    }
    ## right part
    k <- merge[node, 2]
    if (k > 0) {
        xy <- drawdend(k, merge, height, order, labels, offset, hang)
        xr <- xy$x
        yr <- xy$y
    } else {
	xr <- order[-k]
	yr <- if (hang >= 0) y - hang else 0
	if (!is.na(labels[-k]))
            grid.text(labels[-k],
                      xr,
                      yr - offset,
                      default.units="native",
                      hjust=1, vjust=.3,
                      rot=90)
    }
    grid.polyline(c(xl, xl, xr, xr),
                  c(yl, y, y, yr),
                  default.units="native")
    x <- 0.5 * (xl + xr)
    list(x=x, y=y)
}


C_dend <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:7)])
    dev.set(playDev())
    depth <- gotovp(NA)
    ## number of observations ?
    n <- x[[2]]
    ## Matrix n by 2
    ## dnd_lptr is start of first row
    ## dnd_rptr is start of second row
    merge <- x[[3]]
    ## length = n
    height <- x[[4]]
    ## length = n + 1
    order <- x[[5]]
    hang <- x[[6]]*(height[n] - height[1])
    ## length = n + 1
    labels <- x[[7]]
    offset <- convertHeight(stringWidth("m"), "native", valueOnly=TRUE)
    drawdend(n, merge, height, order, labels, offset, hang)
    upViewport(depth)
}    

C_dendwindow <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:6)])
    dev.set(playDev())
    depth <- gotovp(par$xpd)
    n <- x[[2]]
    ## Matrix n by 2
    merge <- x[[3]]
    ## length = n
    height <- x[[4]]
    hang <- x[[5]]
    ## length = n + 1
    labels <- x[[6]]
    dnd_offset <- stringWidth("m")
    ymin <- min(height)
    ymax <- max(height)
    pin <- par$pin[2]
    ll <- convertWidth(stringWidth(labels) + dnd_offset, "in", valueOnly=TRUE)
    if (hang >= 0) {
	ymin <- ymax - (1 + hang) * (ymax - ymin)
	yrange = ymax - ymin
        y <- numeric(n + 1)
        for (i in 1:n) {
	    if (merge[i, 1] < 0)
		y[-merge[i, 1] - 1] <- height[i];
	    if (merge[i, 2] < 0)
		y[-merge[i, 2] - 1] <- height[i];
	}
        temp <- ((ymax - y)/yrange)*pin + ll
        imax <- which.max(temp)
        yval <- temp[imax]
    } else {
	yrange <- ymax
        temp <- pin + ll
        imax <- which.max(temp)
        yval <- temp[imax]
    }
    ymin = ymax - (pin/(pin - ll[imax])) * yrange;
    lim <- c(GScale(1, n + 1, par$xaxs),
             GScale(ymin, ymax, par$yaxs))
    upViewport(depth)
    ## Set up new plot window
    dev.set(recordDev())
    par(usr=lim)
    dev.set(playDev())
    incrementWindowAlpha()
    setWindowPlotAlpha(plotAlpha())
    setUpUsr(lim)
}

