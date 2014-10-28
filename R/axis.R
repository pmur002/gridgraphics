
# C_axis(side, at, labels, 
#        tick, line, pos, outer, font, lty, lwd, lwd.ticks, col, 
#        col.ticks, hadj, padj, ...)
C_axis <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:16)])
    defaultTicks <- axTicks(x[[2]])
    dev.set(playDev())
    # TODO:  write a more complex grid.axis() that can handle
    #        'tick', 'line', 'pos', 'outer', 'lwd.ticks', 'col.ticks',
    # TODO: Take other par() settings into account (e.g., 'las', 'adj', ...)
    depth <- gotovp(NA)
    # TODO: use axTicks() and/or axisTicks() to get axis tick locations
    #       when at=NULL, plus formatC() to format those locations for
    #       the tick labels
    side <- x[[2]]
    if (is.null(x[[3]])) {
        ticks <- defaultTicks
    } else {
        ticks <- x[[3]]
    }
    labels <- x[[4]]
    # NOTE: the use of 'trim=TRUE' in format() to mimic use of,
    #       e.g., EncodeReal0(), within labelformat() in plot.c
    if (is.null(labels)) {
        drawLabels <- TRUE
        labels <- format(ticks, trim=TRUE)
    } else if (is.logical(labels)) {
        if (labels) {
            drawLabels <- TRUE
            labels <- format(ticks, trim=TRUE)
        } else {
            drawLabels <- FALSE
        }
    } else {
        drawLabels <- TRUE
    }
    hadj <- x[[15]]
    padj <- x[[16]]
    if (side == 1) {
        grid.segments(unit(min(ticks), "native"),
                      unit(0, "npc"),
                      unit(max(ticks), "native"),
                      unit(0, "npc"),
                      name=grobname("bottom-axis-line"))
        # TODO: More complex calculation of tick length if par(tck) set
        tickLength <- unit(par$cin[2]*par$tcl*par$cex, "in")
        grid.segments(unit(ticks, "native"),
                      unit(0, "npc"),
                      unit(ticks, "native"),
                      unit(0, "npc") + tickLength,
                      name=grobname("bottom-axis-ticks"))
        # TODO: use 'labels' if given
        # NOTE: the following includes calculation based on par(mgp)
        #       to get the margin line to draw on
        #       PLUS adjustment made in GMtext() based on that line value
        if (drawLabels) {
            GMtext(labels, 1, line=par$mgp[2],
                   at=unit(ticks, "native"), las=par$las, 
                   xadj=computeXAdj(hadj, side, par$las),
                   yadj=computePAdj(padj, side, par$las),
                   mex=par$mex, cin=par$cin, 
                   cex=par$cex.axis*par$cex, linecex=par$cex.axis*par$cex,
                   font=par$font.axis, col=par$col.axis,
                   allowOverlap=FALSE,
                   label="bottom-axis-labels")
        }
    } else if (side == 2) {
        grid.segments(unit(0, "npc"),
                      unit(min(ticks), "native"),
                      unit(0, "npc"),
                      unit(max(ticks), "native"),
                      name=grobname("left-axis-line"))
        tickLength <- unit(par$cin[2]*par$tcl*par$cex, "in")
        grid.segments(unit(0, "npc"),
                      unit(ticks, "native"),
                      unit(0, "npc") + tickLength,
                      unit(ticks, "native"),
                      name=grobname("left-axis-ticks"))
        if (drawLabels) {
            GMtext(labels, 2, line=par$mgp[2],
                   at=unit(ticks, "native"), las=par$las,
                   xadj=computeXAdj(hadj, side, par$las),
                   yadj=computePAdj(padj, side, par$las),
                   mex=par$mex, cin=par$cin,
                   cex=par$cex.axis*par$cex, linecex=par$cex.axis*par$cex,
                   font=par$font.axis, col=par$col.axis,
                   allowOverlap=FALSE,
                   label="left-axis-labels")
        }
    } else if (side == 3) {
        grid.segments(unit(min(ticks), "native"),
                      unit(1, "npc"),
                      unit(max(ticks), "native"),
                      unit(1, "npc"),
                      name=grobname("top-axis-line"))
        tickLength <- unit(par$cin[2]*par$tcl*par$cex, "in")
        grid.segments(unit(ticks, "native"),
                      unit(1, "npc"),
                      unit(ticks, "native"),
                      unit(1, "npc") - tickLength,
                      name=grobname("top-axis-ticks"))
        if (drawLabels) {
            GMtext(labels, 3, line=par$mgp[2],
                   at=unit(ticks, "native"), las=par$las, 
                   xadj=computeXAdj(hadj, side, par$las),
                   yadj=computePAdj(padj, side, par$las),
                   mex=par$mex, cin=par$cin, 
                   cex=par$cex.axis*par$cex, linecex=par$cex.axis*par$cex,
                   font=par$font.axis, col=par$col.axis,
                   allowOverlap=FALSE,
                   label="top-axis-labels")
        }
    } else if (side == 4) {
        grid.segments(unit(1, "npc"),
                      unit(min(ticks), "native"),
                      unit(1, "npc"),
                      unit(max(ticks), "native"),
                      name=grobname("right-axis-line"))
        tickLength <- unit(par$cin[2]*par$tcl*par$cex, "in")
        grid.segments(unit(1, "npc"),
                      unit(ticks, "native"),
                      unit(1, "npc") - tickLength,
                      unit(ticks, "native"),
                      name=grobname("right-axis-ticks"))
        if (drawLabels) {
            GMtext(labels, 4, line=par$mgp[2],
                   at=unit(ticks, "native"), las=par$las,
                   xadj=computeXAdj(hadj, side, par$las),
                   yadj=computePAdj(padj, side, par$las),
                   mex=par$mex, cin=par$cin,
                   cex=par$cex.axis*par$cex, linecex=par$cex.axis*par$cex,
                   font=par$font.axis, col=par$col.axis,
                   allowOverlap=FALSE,
                   label="right-axis-labels")
        }
    } else {
        stop("Invalid 'side' argument")
    }
    upViewport(depth)
}

computeXAdj <- function(hadj, side, las) {
    if (is.finite(hadj)) {
        xadj <- hadj
    } else {
        if (side == 1 || side == 3) {
            if (las == 2 || las == 3) {
                if (side == 1) {
                    xadj <- 1
                } else {
                    xadj <- 0
                }
            } else {
                xadj <- 0.5
            }
        } else {
            if (las == 1 || las == 2) {
                if (side == 2) {
                    xadj <- 1
                } else {
                    xadj <- 0
                }
            } else {
                xadj <- 0.5
            }
        } 
    }
    xadj
}

computePAdj <- function(padj, side, las) {
    if (!is.finite(padj)) {
        padj <- switch(las + 1,
                       0,
                       switch(side, 0, 0.5, 0, 0.5),
                       0.5,
                       switch(side, 0.5, 0, 0.5, 0))
    }
    padj
}
