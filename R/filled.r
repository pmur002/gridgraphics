FindPolygonVertices = function(low,  high,
		     x1,  x2,  y1,  y2,
		     z11,  z21,  z12,  z22,
		     x,  y,  z, npt)
{
    npt = 1
    out = FindCutPoints(low, high, x1,  y1,  z11, x2,  y1,  z21, x, y, z, npt)
    x = out$x; y = out$y; z = out$z; npt = out$npt
    
    out = FindCutPoints(low, high, y1,  x2,  z21, y2,  x2,  z22, y, x, z, npt)
    x = out$x; y = out$y; z = out$z; npt = out$npt
    
    out = FindCutPoints(low, high, x2,  y2,  z22, x1,  y2,  z12, x, y, z, npt)
    x = out$x; y = out$y; z = out$z; npt = out$npt
    
    out = FindCutPoints(low, high, y2,  x1,  z12, y1,  x1,  z11, y, x, z, npt)
    out
    
}




C_filledcontour = function(plot)
{
    dev.set(recordDev())
    par = currentPar(NULL)
    dev.set(playDev())
    
    #plot.info = plot[[1]][[12]][[2]]
    x <<- plot[[2]]
    y <<- plot[[3]]
    z <<- plot[[4]]
    sc <<- plot[[5]]
    px = py = pz = numeric(8)
    ## not sure if we need the "FixupCol" or not 
    scol <<- plot[[6]]

    nx <<- length(x)
    ny <<- length(y)
    if (nx < 2 || ny < 2) stop("insufficient 'x' or 'y' values")

    ## do it this way as coerceVector can lose dims, e.g. for a list matrix
    if (nrow(z) != nx || ncol(z) != ny) stop("dimension mismatch")
 
    nc = length(sc)
    if (nc < 1) warning("no contour values")

    ncol = length(scol)
    npt = 1
    # grid.newpage()
    #vp = plotViewport(par('usr'), xscale = range(x, na.rm = TRUE), 
    #                    yscale = range(y, na.rm = TRUE),
    #                    name = 'clipoff', clip = 'off')
    #pushViewport(vp)
    #upViewport()
    
    depth = gotovp(FALSE)
    
    for(i in 1:(nx - 1)){
    for(j in 1:(ny - 1)){
        for(k in 1:(nc - 1)){
            out = FindPolygonVertices(low = sc[k], high = sc[k + 1],
                    x1 = x[i], x2 = x[i + 1],
                    y1 = y[j], y2 = y[j + 1],
                    z11 = z[i, (j) ],
                    z21 = z[(i + 1), (j) ],
                    z12 = z[i, (j + 1) ],
                    z22 = z[(i + 1), (j + 1) ],
                    x = px, y = py, z = pz, npt = npt)
            
            npt = out$npt
            #print(npt)
            if(npt > 3)
            {
                grid.polygon(out$x[1:(npt - 1)], out$y[1:(npt - 1)], default.units = 'native',
                    gp = gpar(fill = scol[(k - 1) %% ncol + 1], col = NA))
            }
        }
    }
    }
    upViewport(depth)
   
}