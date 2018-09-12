
# Handle log axes

tx <- function(x, par) {
    if (par$xlog && !is.null(x)) {
        log10(x)
    } else {
        x
    }
}

ty <- function(x, par) {
    if (par$ylog && !is.null(x)) {
        log10(x)
    } else {
        x
    }
}

## Make use of graphics::grconvert[X|Y]()

grconvertX <- function(x, from, to) {
    dev.set(recordDev())
    on.exit(dev.set(playDev()))
    graphics::grconvertX(x, from, to)
}

grconvertY <- function(x, from, to) {
    dev.set(recordDev())
    on.exit(dev.set(playDev()))
    graphics::grconvertY(x, from, to)
}
