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
    plot.info = plot[[1]][[12]][[2]]
    x <<- plot.info[[2]]
    y <<- plot.info[[3]]
    z <<- plot.info[[4]]
    sc <<- plot.info[[5]]
    px = py = pz = numeric(8)
    ## not sure if we need the "FixupCol" or not 
    scol <<- FixupCol(plot.info[[6]])

    nx <<- length(x)
    ny <<- length(y)
    if (nx < 2 || ny < 2) stop("insufficient 'x' or 'y' values")

    ## do it this way as coerceVector can lose dims, e.g. for a list matrix
    if (nrow(z) != nx || ncol(z) != ny) stop("dimension mismatch")
 
    nc <<- length(sc)
    if (nc < 1) warning("no contour values")

    ncol = length(scol)
    npt = 0
    grid.newpage()
    vp = plotViewport(par('usr'), xscale = range(x, na.rm = TRUE), 
                        yscale = range(y, na.rm = TRUE),
                        name = 'clipoff', clip = 'off')
    pushViewport(vp)
    upViewport()
    
    count = 0
    for(i in 1:(nx - 1)){
    for(j in 1:(ny - 1)){
        for(k in 1:(nc - 1)){
            if(count == 0)  npt = 1; count = count + 1            
            out = FindPolygonVertices(low = sc[k], high = sc[k + 1],
                    x1 = x[i], x2 = x[i + 1],
                    y1 = y[j], y2 = y[j + 1],
                    z11 = z[i, (j) ],
                    z21 = z[(i + 1), (j) ],
                    z12 = z[i, (j + 1) ],
                    z22 = z[(i + 1), (j + 1) ],
                    x = px, y = py, z = pz, npt = npt)
            
            npt = out$npt
            if(npt > 2)
            {
                grid.polygon(out$x, out$y,default.units = 'native', vp = 'clipoff')
            }
        }
    }
    }
   
}