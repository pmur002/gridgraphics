## vectorization version  (main in used)
FindPolygonVertices = function(low,  high,
		     x1,  x2,  y1,  y2,
		     z11,  z21,  z12,  z22,
             colrep){

    v1 = FindCutPoints(low, high, x1, y1, x2, y1, z11, z21)
    v2 = FindCutPoints(low, high, y1, x2, y2, x2, z21, z22)
    v3 = FindCutPoints(low, high, x2, y2, x1, y2, z22, z12)
    v4 = FindCutPoints(low, high, y2, x1, y1, x1, z12, z11)

    vx = cbind(v1[[1]], v2[[2]], v3[[1]], v4[[2]])
    vy = cbind(v1[[2]], v2[[1]], v3[[2]], v4[[1]])
    
    ##  track the coordinate for x and y( if non-NA's)
    index = rowSums(!is.na(vx) )
    ## keep if non-NAs row >= 2 (npt >= 2)
    vx = t(vx)
    vy = t(vy)
    xcoor.na = as.vector(vx[, index > 2])
    ycoor.na = as.vector(vy[, index > 2])
    ## delete all NA's,
    xcoor = xcoor.na[!is.na(xcoor.na)]
    ycoor = ycoor.na[!is.na(ycoor.na)]

    id.length = index[index > 2]
    cols = colrep[index > 2]
    
    out = list(x = xcoor, y = ycoor, id.length = id.length, cols = cols)
    outs = out
    out
}

FindCutPoints = function(low, high, x1, y1, x2, y2, z1, z2)
{
## inner condiction begin
    ## first ocndiction
    c = (z1 - high) / (z1 - z2)
    cond1 = z1 < high
    cond2 = z1 == Inf
    cond3 = z2 > high | z1 < low
    
    x.1 = ifelse(cond1, x1, 
              ifelse(cond2, x2, x1 + c * (x2 - x1)))
    x.1 = ifelse(cond3, NA, x.1)
                
    y.1 = ifelse(cond1, y1, 
               ifelse(cond2, y1, y1))
    y.1 = ifelse(cond3, NA, y.1)
    
    cond4 = z2 == -Inf
    cond5 = z2 <= low
    cond6 = z2 > high | z1 < low

    c = (z2 -low) / (z2 - z1)
    x.2 = ifelse(cond4, x1,
             ifelse(cond5, x2 - c * (x2 - x1), NA))
    x.2 = ifelse(cond6, NA, x.2)
             
    y.2 = ifelse(cond4, y1,
              ifelse(cond5, y1, NA))
    y.2 = ifelse(cond6, NA, y.2)

    ## second condiction
    cond7 = z1 > low
    cond8 = z1 == -Inf
    cond9 = z2 < low | z1 > high
    
    c = (z1 - low) / (z1 - z2)
    x_1 = ifelse(cond7, x1, 
                ifelse(cond8, x2, x1 + c * (x2 - x1)))
    x_1 = ifelse(cond9, NA, x_1)
                
    y_1 = ifelse(cond7, y1, 
                ifelse(cond8, y1, y1))
    y_1 = ifelse(cond9, NA, y_1)
    
    cond10 = z2 < high
    cond11 = z2 == Inf
    cond12 = z2 < low | z1 > high
                
    c = (z2 - high) / (z2 - z1)
    x_2 = ifelse(cond10, NA, 
                ifelse(cond11, x1, x2 - c * (x2 - x1)))
    x_2 = ifelse(cond12, NA, x_2)
                
    y_2 = ifelse(cond10, NA, 
                ifelse(cond11, y1, y1))
    y_2 = ifelse(cond12, NA, y_2)
                
    ## third condiction
    cond13 = low <= z1 & z1 <= high
    x..1 = ifelse(cond13, x1, NA)
    y..1 = ifelse(cond13, y1, NA)
## inner condiction end
    
## outer condiction 
    cond14 = z1 > z2
    cond15 = z1 < z2

    xout.1 = ifelse(cond14, x.1,
                ifelse(cond15, x_1,
                        x..1))
    xout.2 = ifelse(cond14, x.2,
                ifelse(cond15, x_2,
                        NA))						

    yout.1 = ifelse(cond14, y.1,
                ifelse(cond15, y_1,
                        y..1))
    yout.2 = ifelse(cond14, y.2,
                ifelse(cond15, y_2,
                        NA))			
## outer condiction end

    ## return x1, x2, y1, y2
    xout = cbind(xout.1, xout.2)
    yout = cbind(yout.1, yout.2)
    list(xout, yout)
}

C_filledcontour = function(plot)
{
    dev.set(recordDev())
    par = currentPar(NULL)
    dev.set(playDev())

    x = plot[[2]]
    y = plot[[3]]
    z = plot[[4]]
    s = plot[[5]]
    cols = plot[[6]]
    
    ns = length(s)
    nx = length(x)
    ny = length(y)

    x1 = rep(x[-nx], each = ny - 1)
    x2 = rep(x[-1], each = ny - 1)
    y1 = rep(y[-ny], nx - 1)
    y2 = rep(y[-1], nx - 1)

    z11 = as.numeric(t(z[-nx, -ny]))
    z21 = as.numeric(t(z[-1, -ny ]))
    z12 = as.numeric(t(z[-nx, -1]))
    z22 = as.numeric(t(z[-1, -1]))
    
    x1 = rep(x1, each = ns - 1)
    x2 = rep(x2, each = ns - 1)
    y1 = rep(y1, each = ns - 1)
    y2 = rep(y2, each = ns - 1)
    z11 = rep(z11, each = ns - 1)
    z12 = rep(z12, each = ns - 1)
    z21 = rep(z21, each = ns - 1)
    z22 = rep(z22, each = ns - 1)
    low = rep(s[-ns], (nx - 1) * (ny - 1))
    high = rep(s[-1], (nx - 1) * (ny - 1))
    
    ## rep color until the same length of x, then subsetting 
    if(length(cols) > ns){
        cols = cols[1:(ns - 1)]
    }else
    {
        cols = rep_len(cols, ns - 1)
    }
    colrep = rep(cols[1:(ns - 1)], nx * ny)
    ## feed color as well as subseeting as x and y
    out = FindPolygonVertices(
                low = low, high = high,
                x1 = x1, x2 = x2, 
                y1 = y1, y2 = y2,
                z11 = z11, z21 = z21, 
                z12 = z12, z22 = z22, colrep = colrep)
    ## actual drawing
    depth = gotovp(TRUE)
    grid.polygon(out$x, out$y, default.units = 'native', id.lengths = out$id.length,
             gp = gpar(fill = out$cols, col = NA))
    upViewport(depth)
}


## for loop version
## identical to C_filledcontour in plot3d.c but very slow
lFindPolygonVertices = function(low,  high,
		     x1,  x2,  y1,  y2,
		     z11,  z21,  z12,  z22,
		     x,  y,  z, npt)
{
    out = list()
    npt = 0
    out1 = lFindCutPoints(low, high, x1,  y1,  z11, x2,  y1,  z21, x, y, z, npt)
    x = out1$x; y = out1$y; z = out1$z; npt = out1$npt

    out2 = lFindCutPoints(low, high, y1,  x2,  z21, y2,  x2,  z22, y, x, z, npt)
    x = out2$x; y = out2$y; z = out2$z; npt = out2$npt

    out3 = lFindCutPoints(low, high, x2,  y2,  z22, x1,  y2,  z12, x, y, z, npt)
    x = out3$x; y = out3$y; z = out3$z; npt = out3$npt
            
    out4 = lFindCutPoints(low, high, y2,  x1,  z12, y1,  x1,  z11, y, x, z, npt)

    out$x = out1$x + out2$y + out3$x + out4$y
    out$y = out1$y + out2$x + out3$y + out4$x
    out$npt = out4$npt
    out
}

lC_filledcontour = function(plot)
{
    dev.set(recordDev())
    par = currentPar(NULL)
    dev.set(playDev())

    x  =  plot[[2]]
    y = plot[[3]]
    z = plot[[4]]
    sc = plot[[5]]
    px = py = pz = numeric(8)
    scol = plot[[6]]

    nx = length(x)
    ny = length(y)
    if (nx < 2 || ny < 2) stop("insufficient 'x' or 'y' values")

    ## do it this way as coerceVector can lose dims, e.g. for a list matrix
    if (nrow(z) != nx || ncol(z) != ny) stop("dimension mismatch")

    nc = length(sc)
    if (nc < 1) warning("no contour values")

    ncol = length(scol)
    
    depth = gotovp(TRUE)
    for(i in 1:(nx - 1)){
    for(j in 1:(ny - 1)){
        for(k in 1:(nc - 1)){
            npt = 0
            out = lFindPolygonVertices(sc[k], sc[k + 1],
                    x[i], x[i + 1],
                    y[j], y[j + 1],
                    z[i, j],
                    z[i + 1, j],
                    z[i, j + 1],
                    z[i + 1, j + 1],
                    px, py, pz, npt)
            
            npt = out$npt
            
            if(npt > 2)
            { 
                grid.polygon(out$x[1:npt], out$y[1:npt], default.units = 'native',
                    gp = gpar(fill = scol[(k - 1) %% ncol + 1], col = NA), name = 'filled.contour')
            }
        }
    }
    }
    upViewport(depth)
   
}

lFindCutPoints = function( low,  high,
	       x1,  y1,  z1,
	       x2,  y2,  z2,
	       x,  y,  z,
	       npt)
{
    x = y = z = numeric(8)
    if (z1 > z2 ) {
        if (z2 > high || z1 < low){
            return(out = list(x = x, y = y, z = z, npt = npt))
        }

        if (z1 < high) {
            x[npt + 1] = x1
            y[npt + 1] = y1
            z[npt + 1] = z1
            npt = npt + 1
        } else if (z1 == Inf) {
            x[npt + 1] = x2
            y[npt + 1] = y1
            z[npt + 1] = z2
            npt = npt + 1
        } else {
            c = (z1 - high) / (z1 - z2)
            x[npt + 1] = x1 + c * (x2 - x1)
            y[npt + 1] = y1
            z[npt + 1] = z1 + c * (z2 - z1)
            npt = npt + 1
        }
        
        if (z2 == -Inf) {
            x[npt + 1] = x1
            y[npt + 1] = y1
            z[npt + 1] = z1
            npt = npt + 1
        } else if (z2 <= low) {
            c = (z2 -low) / (z2 - z1)
            x[npt + 1] = x2 - c * (x2 - x1)
            y[npt + 1] = y1
            z[npt + 1] = z2 - c * (z2 - z1)
            npt = npt + 1
        }
        
    } else if (z1 < z2) {
        if (z2 < low || z1 > high) {
                return(out = list(x = x, y = y, z = z, npt = npt))
        }
        
        if (z1 > low) {
            x[npt + 1] = x1
            y[npt + 1] = y1
            z[npt + 1] = z1
            npt = npt + 1
        } else if (z1 == -Inf) {
            x[npt + 1] = x2
            y[npt + 1] = y1
            z[npt + 1] = z2
            npt = npt + 1
        } else { 
            c = (z1 - low) / (z1 - z2)
            x[npt + 1] = x1 + c * (x2 - x1)
            y[npt + 1] = y1
                z[npt + 1] = z1 + c * (z2 - z1)
            npt = npt + 1
        }
        
        if (z2 < high) {
        } else if (z2 == Inf) {
            x[npt + 1] = x1
            y[npt + 1] = y1
            z[npt + 1] = z1
            npt = npt + 1
        } else {
            c = (z2 - high) / (z2 - z1)
            x[npt + 1] = x2 - c * (x2 - x1)
            y[npt + 1] = y1
            z[npt + 1] = z2 - c * (z2 - z1)
            npt = npt + 1
        }
    } else {
        if(low <= z1 && z1 <= high) {
            x[npt + 1] = x1
            y[npt + 1] = y1
            z[npt + 1] = z1
            npt = npt + 1
        }
    }
    out = list(x = x, y = y, z = z, npt = npt)
    out
}
