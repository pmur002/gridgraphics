
# C_text(xy.coords, labels, adj, pos, offset, vfont, cex, col, font, ...)

C_text <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:10)])
    dev.set(playDev())
    # TODO:  handle 'pos' etc ...
    depth <- gotovp(par$xpd)
    xx <- x[[2]]$x
    yy <- x[[2]]$y
    labels <- x[[3]]
    adj <- x[[4]]
    just <- just(adj, par)
    cex <- x[[8]]*par$cex
    col <- x[[9]]
    font <- x[[10]]
    grid.text(labels, xx, yy, default.units="native",
              hjust=just[1], vjust=just[2],
              gp=gpar(cex=cex, col=col, fontface=font),
              name=grobname("text"))
    upViewport(depth)
}

just <- function(adj, par) {
    if (is.null(adj) || length(adj) == 0) {
        adjx <- par$adj
        adjy <- NA
    } else {
        if (length(adj) == 1) {
            adjx <- adj
            adjy <- NA
        } else {
            adjx <- adj[1]
            adjy <- adj[2]
        }
    }
    c(adjx, adjy)
}
