FindPolygonVertices = function(low,  high,
		     x1,  x2,  y1,  y2,
		     z11,  z21,  z12,  z22,
		     x,  y,  z, npt)
{
	out = list(x = 0, y = 0, npt = 0)
	x = y = z = numeric(8)
	npt = 0
    out1 <<- FindCutPoints(low, high, x1,  y1,  z11, x2,  y1,  z21, x, y, z, npt)
	x = out1$x; y = out1$y; z = out1$z; npt = out1$npt
	x = y = z = numeric(8)
    out2 <<- FindCutPoints(low, high, y1,  x2,  z21, y2,  x2,  z22, y, x, z, npt)
	x = out2$x; y = out2$y; z = out2$z; npt = out2$npt

	x = y = z = numeric(8)
    out3 <<- FindCutPoints(low, high, x2,  y2,  z22, x1,  y2,  z12, x, y, z, npt)
	x = out3$x; y = out3$y; z = out3$z; npt = out3$npt
			

	x = y = z = numeric(8)
    out4 <<- FindCutPoints(low, high, y2,  x1,  z12, y1,  x1,  z11, y, x, z, npt)
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
    
    #plot.info = plot[[1]][[12]][[2]]
    x <<- plot[[2]]
    y = plot[[3]]
    z = plot[[4]]
    sc = plot[[5]]
    px = py = pz = numeric(8)
    ## not sure if we need the "FixupCol" or not 
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
            #out = FindPolygonVertices(low = sc[k], high = sc[k + 1],
            #        x1 = x[i], x2 = x[i + 1],
            #        y1 = y[j], y2 = y[j + 1],
            #        z11 = z[i, (j)],
            #        z21 = z[(i + 1), (j)],
            #        z12 = z[i, (j + 1) ],
            #        z22 = z[(i + 1), (j + 1)],
            #        x = px, y = py, z = pz, npt = npt)
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
            #print(npt)
            if(npt > 2)
            {
				#outs <<- out
				#print(out)
				#stop()
                grid.polygon(out$x[1:npt], out$y[1:npt], default.units = 'native',
                    gp = gpar(fill = scol[(k - 1) %% ncol + 1], col = NA))
            }
        }
    }
    }
    upViewport(depth)
   
}