FindCutPoints = function(low, high, x1, y1, z1, x2, y2, z2,
                        x, y, z, npt){
    
    if( z1 > z2)
    {
        if( z2 > high || z1 < low) return(0)
        if(z1 < high)
        {
            x[npt] = x1
            y[npt] = y1
            z[npt] = z1
            npt = npt + 1
        } else if(z1 == Inf)
        {
            x[npt] = x2
            y[npt] = y1
            z[npt] = z2
            npt = npt + 1
        } else{
            c = (z1 - high)/(z1 - z2)
            x[npt] = x1 + c * (x2 - x1)
            y[npt] = y1
            z[npt] = z1 + c* (z2 - z1)
            npt = npt + 1
        }
    ##
    if (z2 == -Inf) {
	    x[npt] = x1
	    y[npt] = y1
	    z[npt] = z1
	    npt = npt + 1
	} else if (z2 <= low) { 
	    c = (z2 -low) / (z2 - z1)
	    x[npt] = x2 - c * (x2 - x1)
	    y[npt] = y1
	    z[npt] = z2 - c * (z2 - z1)
	    npt = npt + 1
	}
    
    } else if (z1 < z2) {
	if (z2 < low || z1 > high) return
	if (z1 > low) {
	    x[npt] = x1
	    y[npt] = y1
	    z[npt] = z1
	    npt = npt + 1
	} else if (z1 == -Inf) {
	    x[npt] = x2
	    y[npt] = y1
	    z[npt] = z2
	    npt = npt + 1
	} else { 
	    c = (z1 - low) / (z1 - z2)
	    x[npt] = x1 + c * (x2 - x1)
	    y[npt] = y1
	    z[npt] = z1 + c * (z2 - z1)
	    npt = npt + 1
	}
    
    if (z2 < high) {
	    x[npt] = x2
	    y[npt] = y2
	    z[npt] = z2
	    npt = npt + 1
	} else if (z2 == Inf) {
	    x[npt] = x1
	    y[npt] = y1
	    z[npt] = z1
	    npt = npt + 1
	} else { 
        c = (z2 - high) / (z2 - z1)
	    x[npt] = x2 - c * (x2 - x1)
	    y[npt] = y1
	    z[npt] = z2 - c * (z2 - z1)
	    npt = npt + 1
	}
    } else {
	if(low <= z1 && z1 <= high) {
	    x[npt] = x1
	    y[npt] = y1
	    z[npt] = z1
	    npt = npt + 1

	    x[npt] = x2
	    y[npt] = y2
	    z[npt] = z2
	    npt = npt + 1
	}
    }
    out = list(x = x, y = y, z = z, npt = npt)
    outs<<- out
    out
}

FindPolygonVertices = function(low,  high,
		     x1,  x2,  y1,  y2,
		     z11,  z21,  z12,  z22,
		     x,  y,  z, npt)
{
    FindCutPoints(low, high, x1,  y1,  z11, x2,  y1,  z21, x, y, z, npt)
    FindCutPoints(low, high, y1,  x2,  z21, y2,  x2,  z22, y, x, z, npt)
    FindCutPoints(low, high, x2,  y2,  z22, x1,  y2,  z12, x, y, z, npt)
    FindCutPoints(low, high, y2,  x1,  z12, y1,  x1,  z11, y, x, z, npt)
}




C_filledcontour = function(plot)
{
    plot.info = plot[[1]][[11]][[2]]
    x = plot.info[[2]]
    y = plot.info[[3]]
    z = plot.info[[4]]
    sc = plot.info[[5]]
    px = py = pz = numeric(8)
    ## not sure if we need the "FixupCol" or not 
    scol = FixupCol(plot.info[[6]])

    nx = length(x)
    ny = length(y)
    if (nx < 2 || ny < 2) stop("insufficient 'x' or 'y' values")

    ## do it this way as coerceVector can lose dims, e.g. for a list matrix
    if (nrow(z) != nx || ncol(z) != ny) stop("dimension mismatch")
 
    nc = length(sc)
    if (nc < 1) warning("no contour values")

    ncol = length(scol)
    npt = 0
    #/* Shorthand Pointers */
    
    
    count = 0
    for(i in 1:nx){
    for(j in 1:ny){
        for(k in 1:nc){
            
            if(count == 0)  npt = 0; count = count + 1
            
            out <<- FindPolygonVertices(sc[k], sc[k + 1],
                    x[i], x[i + 1],
                    y[j], y[j + 1],
                    z[i + (j) * nx],
                    z[(i + 1) + (j) * nx],
                    z[i + (j + 1) * nx],
                    z[(i + 1) + (j + 1) * nx],
                    px, py, pz, npt)
            npt = out$npt
            x = out$x
            y = out$y
            z = out$z
            c = out$c
        }
    }
    }
   
}