
# C_text(xy.coords, labels, adj, pos, offset, vfont, cex, col, font, ...)

C_text <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:10)])
    dev.set(playDev())
    # TODO:  handle 'pos', 'offset', 'vfont'
    depth <- gotovp(par$xpd)
    xx <- tx(x[[2]]$x, par)
    yy <- ty(x[[2]]$y, par)
    labels <- x[[3]]
    adj <- x[[4]]
    just <- just(adj, par)
    cex <- FixupCex(x[[8]]*par$cex, 1)
    cex <- ifelse(is.na(cex), par$cex, cex)
    col <- FixupCol(x[[9]], NA, par$bg)
    col <- ifelse(is.na(col), par$col, col)
    font <- FixupFont(x[[10]], NA)
    font <- ifelse(is.na(font), par$font, font)
    grid.text(labels, xx, yy, default.units="native",
              hjust=just[1], vjust=just[2], rot=par$srt,
              gp=gpar(cex=cex, col=col, fontface=font, lineheight=par$lheight),
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
