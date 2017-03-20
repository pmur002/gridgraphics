FindPolygonVertices = function(low,  high,
		     x1,  x2,  y1,  y2,
		     z11,  z21,  z12,  z22,
		     x,  y,  z, npt)
{
    out = list()
    npt = 0
    #      FindCutPoints(low, high, x1,  y1,  z1,  x2,  y2,  z2,  x, y, z, npt)
    out1 = FindCutPoints(low, high, x1,  y1,  z11, x2,  y1,  z21, x, y, z, npt)
    x = out1$x; y = out1$y; z = out1$z; npt = out1$npt

    out2 = FindCutPoints(low, high, y1,  x2,  z21, y2,  x2,  z22, y, x, z, npt)
    x = out2$x; y = out2$y; z = out2$z; npt = out2$npt

    out3 = FindCutPoints(low, high, x2,  y2,  z22, x1,  y2,  z12, x, y, z, npt)
    x = out3$x; y = out3$y; z = out3$z; npt = out3$npt
            
    out4 = FindCutPoints(low, high, y2,  x1,  z12, y1,  x1,  z11, y, x, z, npt)

    out$x = out1$x + out2$y + out3$x + out4$y
    out$y = out1$y + out2$x + out3$y + out4$x
    out$npt = out4$npt
    out
}




C_filledcontour = function(plot)
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
            out = FindPolygonVertices(sc[k], sc[k + 1],
                    x[i], x[i + 1],
                    y[j], y[j + 1],
                    z[i + (j - 1) * nx],
                    z[i + 1 + (j - 1) * nx],
                    z[i + (j) * nx],
                    z[i + 1 + (j) * nx],
                    px, py, pz, npt)
            npt = out$npt
            
            if(npt > 2)
            {
                grid.polygon(out$x[1:npt], out$y[1:npt], default.units = 'native',
                    gp = gpar(fill = scol[(k - 1) %% ncol + 1], col = NA))
            }
        }
    }
    }
    upViewport(depth)
   
}


FindCutPoints = function( low,  high,
	       x1,  y1,  z1,
	       x2,  y2,  z2,
	       x,  y,  z,
	       npt)
{
    x = y = z = numeric(8)
    if (z1 > z2 ) {
        ## first column
        if (z2 > high || z1 < low){
            return(out = list(x = x, y = y, z = z, npt = npt))
            # print('hey')
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
        ## second column
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
                # print('hi')
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
                # print('sup')
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


## vectorization version
vFindCutPoints = function(low, high, x1, y1, x2, y2, z1, z2)
{
## inner condiction begin
    ## first ocndiction
    c = (z1 - high) / (z1 - z2)
    x.1 = ifelse(z1 < high, x1, 
              ifelse(z1 == Inf, x2, x1 + c * (x2 - x1)))
    x.1 = ifelse(z2 > high | z1 < low, NA, x.1)
                
    y.1 = ifelse(z1 < high, y1, 
               ifelse(z1 == Inf, y1, y1))
    y.1 = ifelse(z2 > high | z1 < low, NA, y.1)

    c = (z2 -low) / (z2 - z1)
    x.2 = ifelse(z2 == -Inf, x1,
             ifelse(z2 <= low, x2 - c * (x2 - x1), NA))
    x.2 = ifelse(z2 > high | z1 < low, NA, x.2)
             
    y.2 = ifelse(z2 == -Inf, y1,
              ifelse(z2 <= low, y1, NA))
    y.2 = ifelse(z2 > high | z1 < low, NA, y.2)

    ## second condiction
    c = (z1 - low) / (z1 - z2)
    x_1 = ifelse(z1 > low, x1, 
                ifelse(z1 == -Inf, x2, x1 + c * (x2 - x1)))
    x_1 = ifelse(z2 < low | z1 > high, NA, x_1)
                
    y_1 = ifelse(z1 > low, y1, 
                ifelse(z1 == -Inf, y1, y1))
    y_1 = ifelse(z2 < low | z1 > high, NA, y_1)
                
    c = (z2 - high) / (z2 - z1)
    x_2 = ifelse(z2 < high, x1, 
                ifelse(z2 == Inf, x1, x2 - c * (x2 - x1)))
    x_2 = ifelse(z2 < low | z1 > high, NA, x_2)
                
    y_2 = ifelse(z2 < high, y1, 
                ifelse(z2 == Inf, y1, y1))
    y_2 = ifelse(z2 < low | z1 > high, NA, y_2)
                
## third condiction
    x..1 = ifelse(low <= z1 & z1 <= high, x1, x1)
    y..1 = ifelse(low <= z1 & z1 <= high, y1, y1)
    ## inner condiction end
    
## outer condiction 
    xout.1 = ifelse(z1 > z2, x.1,
                ifelse(z1 < z2, x_1,
                        x..1))
    xout.2 = ifelse(z1 > z2, x.2,
                ifelse(z1 < z2, x_2,
                        x..1))						

    yout.1 = ifelse(z1 > z2, y.1,
                ifelse(z1 < z2, y_1,
                        y..1))
    yout.2 = ifelse(z1 > z2, y.2,
                ifelse(z1 < z2, y_2,
                        y..1))					
    
    ## return x1, x2, y1, y2
    xout = cbind(xout.1, xout.2)
    yout = cbind(yout.1, yout.2)
    list(xout, yout)
}

vFindPolygonVertices = function(low,  high,
		     x1,  x2,  y1,  y2,
		     z11,  z21,  z12,  z22,
             colrep){

    v1 = vFindCutPoints(low, high, x1, y1, x2, y1, z11, z21)
    v2 = vFindCutPoints(low, high, y1, x2, y2, x2, z21, z22)
    v3 = vFindCutPoints(low, high, x2, y2, x1, y2, z22, z12)
    v4 = vFindCutPoints(low, high, y2, x1, y1, x1, z12, z11)

    vx = cbind(v1[[1]], v2[[2]], v3[[1]], v4[[2]])
    vy = cbind(v1[[2]], v2[[1]], v3[[2]], v4[[1]])
    
    ##  track the coordinate for x and y( if non-NA's)
    index = rowSums(!is.na(vx))
    ## keep if non-NAs row >= 2 (npt >= 2)
    vx = t(vx)
    vy = t(vy)
    xcoor.na = as.vector(vx[, index >= 2])
    ycoor.na = as.vector(vy[, index >= 2])
    ## delete all NA's,
    xcoor = xcoor.na[!is.na(xcoor.na)]
    ycoor = ycoor.na[!is.na(ycoor.na)]

    id.length = index[index != 0]
    
    cols = colrep[index >= 2]
    out = list(x = xcoor, y = ycoor, id.length = id.length, cols = cols)
    out
    
}


vC_filledcontour = function(plot)
{

    dev.set(recordDev())
    par = currentPar(NULL)
    dev.set(playDev())

    x = plot[[2]]
    y = plot[[3]]
    z = plot[[4]]
    s = plot[[5]]
    cols = plot[[6]]

    xt = rep(rep(x, each = length(y)), each = length(s))
    yt = rep(rep(x, length(x)), each = length(s))
    zt = rep(as.numeric(z), each = length(s))
    st = rep(s, ((length(x) - 1)) * ((length(y) - 1)))

    xr = rep(x, each = length(y))
    yr = rep(y, length(x))
    zr = as.numeric(z)

    ## 1:length(x) + 1:length(y)
    tot = length(x) * length(y)
    det = seq(1, length(xr), by = length(x))
    x1 = xr[1:(tot - length(x))][-(det - 1)]
    x2 = xr[(length(x) + 1):tot][-(det - 1)]
    y1 = yr[1:(tot - length(y))][-(det - 1)]
    y2 = yr[(length(y) + 1):tot][-det]
    z11 = zr[1:(tot - length(x))][-(det - 1)]
    z12 = zr[2:(tot - length(x) + 1)][-(det - 1)]
    z21 = zr[(length(x) + 1) : (tot - 1)][-(det - 1)]
    z22 = zr[(length(x) + 2) : (tot)][-(det - 1)]

    ## plus 1:length(k)
    ns = length(s) - 1
    x1 = rep(x1, each = ns)
    x2 = rep(x2, each = ns)
    y1 = rep(y1, each = ns)
    y2 = rep(y2, each = ns)
    z11 = rep(z11, each = ns)
    z12 = rep(z12, each = ns)
    z21 = rep(z21, each = ns)
    z22 = rep(z22, each = ns)

    scut = seq(0, length(st), by = length(s))[-1]
    low = st[-scut]
    high = st[-((scut - length(s)) + 1)]
    
    ## rep color until the same length of x, then subsetting 
    k = rep((1:length(s)), (length(x) - 1) * (length(y) - 1))
    ncol = length(cols)
    colrep = cols[(k - 1) %% ncol + 1][-scut]

    ## feed color as well as subseeting as x and y
    out = vFindPolygonVertices(
                low = low, high = high,
                x1 = x1, x2 = x2, 
                y1 = y1, y2 = y2,
                z11 = z11, z21 = z21, 
                z12 = z12, z22 = z22, colrep = colrep)
                
    depth = gotovp(TRUE)
    grid.polygon(out$x, out$y, default.units = 'native', id.lengths = out$id.length,
             gp = gpar(fill = out$cols, col = NA))
    upViewport(depth)

}

