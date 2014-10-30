
# C_title(main, sub, xlab, ylab, line, outer, ...)
C_title <- function(x) {
    # TODO: Handle 'sub'
    # TODO: Handle 'line' argument
    # TODO: Query par() for default 'line'
    # TODO: Take other par() settings into account (e.g., 'las', 'adj', ...)
    # TODO: Handle annotation-specific pars like cex.main
    dev.set(recordDev())
    par <- currentPar(x[-(1:7)])
    dev.set(playDev())
    # TODO: there is an 'asp' = ipr[2]/ipr[2] adjustment missing in the
    #       x= calculation below (where ipr = cra/cin ?)
    #       (i.e., we are assuming square pixels, asp == 1)
    main <- x[[2]]
    sub <- x[[3]]
    xlab <- x[[4]]
    ylab <- x[[5]]
    line <- x[[6]]
    outer <- x[[7]]
    if (outer) {
        depth <- gotovp(NA, "inner")
    } else {
        depth <- gotovp(if (is.na(par$xpd)) NA else TRUE, "plot")
    }        
    if (!is.null(main)) {
	if (outer) {
	    if (is.finite(line)) {
		vpos <- line
		adjy <- 0
	    } else {
		vpos <- 0.5*par$oma[3]
		adjy = 0.5
	    }
	} else {
	    if (is.finite(line)) {
		vpos = line
		adjy = 0
	    } else {
		vpos = 0.5*par$mar[3]
		adjy = 0.5
	    }
	}
        grid.text(main,
                  x=unit(par$adj, "npc"),
                  y=unit(1, "npc") +
                    unit(vpos*par$cex*par$cin[2], "in"),
                  vjust=adjy,
                  gp=gpar(cex=par$cex.main*par$cex, fontface=par$font.main,
                          col=par$col.main, lineheight=par$lheight),
                  name=grobname("main"))
    }
    if (!is.null(sub)) {
        GMtext(sub, 1, line=par$mgp[1] + 1, at=0.5, las=0, xadj=0.5, yadj=0,
               mex=par$mex, cin=par$cin, cex=par$cex.sub*par$cex,
               linecex=par$cex, font=par$font.sub, col=par$col.sub,
               lheight=par$lheight, label="sub")
    }
    GMtext(xlab, 1, line=par$mgp[1], at=0.5, las=0, xadj=0.5, yadj=0, 
           mex=par$mex, cin=par$cin, cex=par$cex.lab*par$cex, linecex=par$cex,
           lheight=par$lheight, font=par$font.lab, col=par$col.lab,
           label="xlab")
    GMtext(ylab, 2, line=par$mgp[1], at=0.5, las=0, xadj=0.5, yadj=0,
           mex=par$mex, cin=par$cin, cex=par$cex.lab*par$cex, linecex=par$cex,
           lheight=par$lheight, font=par$font.lab, col=par$col.lab,
           label="ylab")
    upViewport(depth)
}

