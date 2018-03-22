
dlDispatch <- function(x) {
    switch(x[[2]][[1]]$name,
           C_abline = C_abline(x[[2]]),
           C_plot_new = C_plot_new(x[[2]]),
           palette = C_palette(x[[2]]),
           palette2 = C_palette2(x[[2]]),
           C_plot_window = C_plot_window(x[[2]]),
           C_plotXY = C_plotXY(x[[2]]),
           C_axis = C_axis(x[[2]]),
           C_box = C_box(x[[2]]),
           C_title = C_title(x[[2]]),
           C_polygon = C_polygon(x[[2]]),
           C_text = C_text(x[[2]]),
           C_segments = C_segments(x[[2]]),
           C_rect = C_rect(x[[2]]),
           C_mtext = C_mtext(x[[2]]),
           C_arrows = C_arrows(x[[2]]),
           C_par = C_par(x[[2]]),
           C_image = C_image(x[[2]]),
           C_clip = C_clip(x[[2]]),
           C_xspline = C_xspline(x[[2]]),
           C_path = C_path(x[[2]]),
           C_raster = C_raster(x[[2]]),
           C_identify = C_identify(x[[2]]),
           C_symbols = C_symbols(x[[2]]),
           # These only partially supported
           C_contour = C_contour(x[[2]]),
           # These unsupported
           C_persp = C_persp(x[[2]]),
           C_filledcontour = C_filledcontour(x[[2]]),
           # These are ignored
           C_strWidth = NULL,
           C_strHeight = NULL,
           # Only affects next plot.new() that starts a new page
           # (so not relevant to current page)
           # (BUT may be on display list, e.g., as wrap-up of plot function)
           C_layout = NULL, 
           warning("unsupported operation on the graphics display list"))
}

offscreen <- function(width, height) {
    pdf(NULL, width=width, height=height)
    dev.control("enable")
}

grid.echo <- function(x=NULL, newpage=TRUE, prefix=NULL, device=offscreen) {
    UseMethod("grid.echo")
}

grid.echo.default <- function(x=NULL, newpage=TRUE, prefix=NULL,
                              device=offscreen) {
    if (!is.null(x)) {
        stop("Invalid graphics display list")
    }
    if (is.null(dev.list())) {
        stop("No graphics device")
    }
    grid.echo(recordPlot(), newpage, prefix, device)
}

grid.echo.recordedplot <- function(x=NULL, newpage=TRUE, prefix=NULL,
                                   device=offscreen) {
    assign("newpage", newpage, .gridGraphicsEnv)
    if (!is.null(prefix)) {
        op <- prefix()
        setPrefix(prefix)
        on.exit(setPrefix(op))
    }
    if (newpage) {
        width <- NULL
        height <- NULL
    } else {
        width <- convertWidth(unit(1, "npc"), "in", valueOnly=TRUE)
        height <- convertHeight(unit(1, "npc"), "in", valueOnly=TRUE)
    }
    init(x, width, height, device)
    if (is.null(x[[1]][[2]])) {
        warning("No graphics to replay")
    }
    ## Make sure we clean up if we error out during DL replay
    ## (or once we have finished echoing)
    on.exit(shutdown())
    lapply(x[[1]], dlDispatch)
    invisible()
}

grid.echo.function <- function(x=NULL, newpage=TRUE, prefix=NULL,
                               device=offscreen) {
    if (newpage) {
        if (dev.cur() == 1) {
            width <- 7
            height <- 7
        } else {
            din <- par("din")
            width <- din[1]
            height <- din[2]
        }
    } else {
        width <- convertWidth(unit(1, "npc"), "in", valueOnly=TRUE)
        height <- convertHeight(unit(1, "npc"), "in", valueOnly=TRUE)
    }
    cd <- dev.cur()
    device(width, height)
    echod <- dev.cur()
    ## Make sure that the device is closed if running x() errors out
    on.exit({ dev.set(echod); dev.off(); dev.set(cd) })
    x()
    dl <- recordPlot()
    ## Switch back to device we are echoing on
    dev.set(cd)
    grid.echo(dl, newpage, prefix)
    invisible()
}

echoGrob <- function(x = NULL) {
    stop("I hope to write this one day!")
}

