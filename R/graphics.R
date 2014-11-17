
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
           C_persp = warning("gridGraphics cannot emulate persp()"),
           C_filledcontour = warning("gridGraphics cannot emulate persp()"),
           # These are ignored
           C_strWidth = NULL,
           C_strHeight = NULL,
           warning("unsupported operation on the graphics display list"))
}

# TODO:  allow reproduction within a 'grid' viewport (rather than whole page) ?
grid.echo <- function(x = NULL, newpage=TRUE) {
    UseMethod("grid.echo")
}

grid.echo.default <- function(x = NULL, newpage=TRUE) {
    if (!is.null(x)) {
        stop("Invalid graphics display list")
    }
    if (is.null(dev.list())) {
        stop("No graphics device")
    }
    grid.echo(recordPlot(), newpage)
}

grid.echo.recordedplot <- function(x = NULL, newpage=TRUE) {
    assign("newpage", newpage, .gridGraphicsEnv)
    if (newpage) {
        width <- NULL
        height <- NULL
    } else {
        width <- convertWidth(unit(1, "npc"), "in", valueOnly=TRUE)
        height <- convertHeight(unit(1, "npc"), "in", valueOnly=TRUE)
    }
    init(x, width, height)
    if (is.null(x[[1]][[2]])) {
        stop("No graphics to replay")
    }
    lapply(x[[1]], dlDispatch)
    shutdown()
}

grid.echo.expression <- function(x = NULL, newpage=TRUE) {
    if (newpage) {
        width <- NULL
        height <- NULL
    } else {
        width <- convertWidth(unit(1, "npc"), "in", valueOnly=TRUE)
        height <- convertHeight(unit(1, "npc"), "in", valueOnly=TRUE)
    }
    cd <- dev.cur()
    pdf(NULL, width=width, height=height)
    dev.control("enable")
    eval(x)
    dl <- recordPlot()
    dev.off()
    dev.set(cd)
    grid.echo(dl, newpage)
}

grid.echo.call <- grid.echo.expression
    
echoGrob <- function(x = NULL) {
    stop("I hope to write this one day!")
}

