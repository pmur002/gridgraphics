
# C_mtext(text, side, line, outer, at, adj, padj, cex, col, font, ...) */

C_mtext <- function(x) {
    dev.set(recordDev())
    par <- currentPar(x[-(1:11)])
    dev.set(playDev())
    text <- x[[2]]
    side <- x[[3]]
    line <- x[[4]]
    outer <- x[[5]]
    adj <- ComputeAdjValue(x[[7]], side, par$las)
    at <- ComputeAtValue(x[[6]], adj, side, par$las)
    padj <- ComputePAdjValue(x[[8]], side, par$las)
    cex <- x[[9]]
    # NOTE: default is not par$cex, but 1.0 
    if (!is.finite(cex))
        cex <- 1
    col <- x[[10]]
    if (!is.finite(col))
        col <- par$col
    font <- x[[11]]
    if (!is.finite(font))
        font <- par$font
    if (outer) {
        depth <- gotovp(NA, "inner")
    } else {
        # NOTE: there is a bug in C_mtext() in plot.c where it checks
        #       "if (outer)" when 'outer' is still an SEXP, so the
        #       result is ALWAYS TRUE, so xpd is ALWAYS set to 2
        depth <- gotovp(NA, "window")
        # depth <- gotovp(if (is.na(par$xpd)) NA else TRUE, "window")
    }
    name <- paste0("mtext-side", side)
    if (outer)
        name <- paste(name, "outer", sep="-")
    GMtext(text, side, line, outer, at,
           las=par$las, xadj=adj, yadj=padj,
           mex=par$mex, cin=par$cin, cex=cex, linecex=par$cex,
           font=font, col=col,
           label=grobname(name))
    upViewport(depth)
}

# Helpers for C_mtext()
ComputeAdjValue <- function(adj, side, las) {
    if (is.finite(adj)) {
        adj
    } else {
	switch(las + 1,
               # las = 0
               0.5,
               # las = 1
               switch(side, 0.5, 1, 0.5, 0),
               # las = 2
               switch(side, 1, 1, 0, 0),
               # las = 3
               switch(side, 1, 0.5, 0, 0.5))
    }
}

ComputePAdjValue <- function(padj, side, las) {
    if (is.finite(padj)) {
        padj
    } else {
        switch(las+ 1,
               # las = 0
               0,
               # las = 1
               switch(side, 0, 0.5, 0, 0.5),
               # las = 2
               0.5,
               # las = 3
               switch(side, 0.5, 0, 0.5, 0))
    }
}

ComputeAtValue <- function(at, adj, side, las) {
    if (is.finite(at)) {
        at
    } else {
	# If the text is parallel to the axis, use "adj" for "at"
	# Otherwise, centre the text
	switch(las + 1,
	       # parallel to axis 
               at <- adj,
               # horizontal 
               switch(side, adj, 0.5, adj, 0.5),
               # perpendicular to axis
               0.5,
               # vertical 
               switch(side, 0.5, adj, 0.5, adj))
    }
}

# Code to centralise the work that GMtext() does to mess around with
# the (x, y) locations that it is sent
# (so that those fiddly adjustments are not reproduced all over the place)

# NOTE that 'mgp' is in 'mex' units
# NOTE the 'yLineBias' "fudge factor" of 0.2 (taken from PDF/PS/X11 values)
# NOTE we use par("cin") rather than 'grid' "lines"
# NOTE that 'linecex' attempts to capture the fact that 'cex' has a different
#      effect in C_title (where it only affects text size) and C_axis
#      (where it affects both text size and line size)

# TODO:  take notice of 'line'
# TODO:  take notice of 'outer'
# TODO:  take notice of 'las'
# TODO:  take notice of 'yadj'
GMtext <- function(str, side, line, outer=FALSE, at, las, xadj, yadj,
                   mex, cin, cex, linecex, font, col,
                   allowOverlap=TRUE, label) {
    if (side == 1) {
        if (las == 2 || las == 3) {
            angle <- 90
        } else {
            line <- line + 1/mex*(1 - 0.2)
            angle <- 0
        }
        x <- unit(at, "native")
        y <- unit(-line*cin[2]*linecex, "in")
    } else if (side == 2) {
        if(las == 1 || las == 2) {
	    angle <- 0
	} else {
	    line <- line + 1/mex*0.2
	    angle <- 90
	}
        x <- unit(-line*cin[2]*linecex, "in")
        y <- unit(at, "native")
    } else if (side == 3) {
        if(las == 2 || las == 3) {
	    angle <- 90
	}
	else {
	    line <- line + 1/mex*0.2
	    angle <- 0
	}
        x <- unit(at, "native")
        y <- unit(1, "npc") + unit(line*cin[2]*linecex, "in")
    } else if (side == 4) {
	if(las == 1 || las == 2) {
	    angle <- 0
	}
	else {
	    line <- line + 1/mex*(1 - 0.2)
	    angle <- 90
	}
        x <- unit(1, "npc") + unit(line*cin[2]*linecex, "in")
        y <- unit(at, "native")
    } else {
        stop("Invalid 'side'")
    }
    grid.text(str, x, y, hjust=xadj, vjust=yadj, rot=angle,
              gp=gpar(cex=cex, fontface=font, col=col),
              check.overlap=!allowOverlap,
              name=grobname(label))
}

