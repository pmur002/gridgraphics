
C_par <- function(x) {
    dev.set(recordDev())
    # Mimic call on off-screen device (so get the right answer when
    # query off-screen device in drawing functions)
    do.call("par", x[-1])
    dev.set(playDev())
    # IF we have reset par(usr), we need a new "window" viewport
    if ("usr" %in% names(x[-1][[1]]))
        setUpUsr(x[-1][[1]]$usr)
}

gparParNames <- c("font", "family", "bg", "fg", "col", "lheight",
                  "lend", "ljoin", "lmitre", "ps",
                  "cex", "lex", "lwd", "lty")

gparNameFromParName <- function(x) {
    switch(x,
           font="fontface",
           family="fontfamily",
           bg="fill",
           fg="col",
           lheight="lineheight",
           lend="lineend",
           ljoin="linejoin",
           lmitre="linemitre",
           ps="fontsize",
           x)
}

# 'x' should be a result from calling par() to set new par() values
# (i.e., a list of previous par() values)
gparFromPar <- function(x) {
    gparNames <- sapply(names(x), gparNameFromParName)
    names(x) <- gparNames
    do.call(gpar, x)
}

currentPar <- function(inlinePars) {
    if (length(inlinePars)) {
        opar <- par(inlinePars)
    }
    cpar <- par()
    if (length(inlinePars)) {
        par(opar)
    }
    cpar
}
