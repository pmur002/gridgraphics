## initialize and create a viewport prepare for drawing
perInit = function ( plot, trans, newpage = FALSE, dbox = TRUE ) {
    info = plot
    ## [[1]] is the all the grapical information that transfer into grid
    ## [[3]] is the persp call information
    ## [[2]] is the plot details eg: x, y, z, xlim, ylim, zlim, col ...
    ## create a list that store all information from the persp
    ## then pass the information to per for drawing.
    ## x is [[2]]; y is [[3]]; z is [[4]]
    ## xr is [[5]]; yr is [[6]]; zr is [[7]]
    ## col is [[14]]; border is [[15]]; box is [[19]]
    ## axes is [[20]], nTicks is [[21]]
    ## tickType is [[22]]
    ## xlab/ylab/zlab = [[23]]/[[24]]/[[25]]
	## main is in plot[[1]][[4]][[2]][[2]]
    ## shade is 0.8, ltheta/lphi = [[16]]/[[17]]
    ## expand is [[13]], scale is [[12]]
    out = list(x = info[[2]], y = info[[3]], z = info[[4]],
                xr = info[[5]], yr = info[[6]], zr = info[[7]],
                col = info[[14]], border = info[[15]], dbox = info[[19]],
                newpage = newpage, 
                axes = info[[20]], nTicks = info[[21]], tickType = info[[22]],
                trans = trans, xlab = info[[23]], ylab = info[[24]], zlab = info[[25]],
				## parameters in 'par' that need added to per
                lwd = info$lwd, lty = info$lty, col.axis = info$col.axis,
				col.lab = info$col.lab, cex.lab = info$cex.lab, 
                shade = info[[18]], ltheta = info[[16]], lphi = info[[17]],
                expand = info[[13]], scale = info[[12]]
				#main = plot[[1]][[4]][[2]][[2]]
                )

    ## clip is on when drawing polygons shade
    #vp = plotViewport(out$mar, xscale = out$lim[1:2], yscale = out$lim[3:4],name = 'clipon',
    #                clip = 'on')
    #pushViewport(vp)
    #upViewport()
    
    ## clip is off when drawing text/label/tickmarks..
    #vp = plotViewport(out$mar, xscale = out$lim[1:2], yscale = out$lim[3:4],name = 'clipoff',
    #                clip = 'off')
    #pushViewport(vp)
    #upViewport()
    
    if(out$newpage == TRUE)
        grid.newpage()
    out
}


## actual drawing by passing the plot into the function
## calculation are done from the function of the 'method.r' file
## only simple function call and few calculation are been done on this function
C_persp = function(plot = NULL, ...)
{
    dev.set(recordDev())
    par = currentPar(NULL)
    dev.set(playDev())
    
    plot = perInit(plot, trans = trans, newpage = FALSE)
    #information extraction
    trans = plot$trans
    xr = plot$xr; yr = plot$yr; zr = plot$zr
    xlab = plot$xlab; ylab = plot$ylab; zlab = plot$zlab
    col.axis = plot$col.axis; col.lab = plot$col.lab; cex.lab = plot$cex.lab
    nTicks = plot$nTicks; tickType = plot$tickType
    expand = plot$expand ;scale = plot$scale
    ltheta = plot$ltheta; lphi = plot$lphi
    main = plot$main; axes = plot$axes
    dbox = plot$dbox; shade = plot$shade
        
    border = plot$border[1];
    if(is.null(plot$lwd)) lwd = 1 else lwd = plot$lwd
    if(is.null(plot$lty)) lty = 1 else lty = plot$lty
    if(any(!(is.numeric(xr) & is.numeric(yr) & is.numeric(zr)))) stop("invalid limits")
    if(any(!(is.finite(xr) & is.finite(yr) & is.finite(zr)))) stop("invalid limits")
    
    xs = LimitCheck(xr)[1]
    ys = LimitCheck(yr)[1]
    zs = LimitCheck(zr)[1]
    if(!scale) xs = ys = zs = max(xs, ys, zs)
    if(is.finite(ltheta) && is.finite(lphi) && is.finite(shade))
        DoLighting = TRUE else DoLighting = FALSE
    if (DoLighting) Light = SetUpLight(ltheta, lphi)
    
    lim = PerspWindow(xr, yr, zr, trans, 'r')
    vp = plotViewport(xscale = lim[1:2], yscale = lim[3:4])
    
    if (dbox == TRUE) {
        EdgeDone = rep(0, 12)
        if(axes == TRUE){
            depth = gotovp(TRUE)
            pushViewport(vp)
            PerspAxes(xr, yr, zr, ##x, y, z
                    xlab, ylab, zlab, ## xlab, xenc, ylab, yenc, zlab, zenc
                    nTicks, tickType, trans, ## nTicks, tickType, VT
                    lwd, lty, col.axis, col.lab, cex.lab) ## lwd, lty, col.axis, col.lab, cex.lab
            upViewport()
            upViewport(depth)} 
    } else {
        EdgeDone = rep(1, 12)
        xr = yr = zr = c(0,0)
    }
    
    
    ## draw the behind face first
    ## return the EdgeDone inorder to not drawing the same Edege two times.
    depth = gotovp(TRUE)
    pushViewport(vp)
    EdgeDone = PerspBox(0, xr, yr, zr, EdgeDone, trans, 1, lwd)
    upViewport()
    upViewport(depth)
    
    depth = gotovp(TRUE)
    pushViewport(vp)
    DrawFacets(plot = plot, z = plot$z, x = plot$x, y = plot$y,     ## basic
                xs = 1/xs, ys = 1/ys, zs = expand/zs,               ## Light
                col = plot$col, length(plot$col),                   ## cols
                ltheta = ltheta, lphi = lphi, Shade = shade, Light = Light)
    upViewport()
    upViewport(depth)

    depth = gotovp(TRUE)
    pushViewport(vp)
    EdgeDone = PerspBox(1, xr, yr, zr, EdgeDone, trans, 'dotted', lwd)
    upViewport()
    upViewport(depth)

}
