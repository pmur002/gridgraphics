
# Handle log axes

tx <- function(x, par) {
    if (par$xlog) {
        log10(x)
    } else {
        x
    }
}

ty <- function(x, par) {
    if (par$ylog) {
        log10(x)
    } else {
        x
    }
}
