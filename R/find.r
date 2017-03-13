reduce = function(vbind, vremian)
{
	vna = is.na(vbind)
	vbind[vna] = vremian[vna]
	vremian[vna] = NA
	cbind(vbind, vremian)
}



find = function(low, high, x1, y1, x2, y2, z1, z2)
{
	## first condiction
	# cond <<- z2 > high | z1 < low
	# x.1 means npt = 1...
	# x_1 means npt = 2...
	
	c = (z1 - high) / (z1 - z2)
	x.1 = ifelse(z1 < high, x1, 
			  ifelse(z1 < high, x2, x1 + c * (x2 - x1)))
	x.1 = ifelse(z2 > high | z1 < low, 0, x.1)
			  	
	y.1 = ifelse(z1 < high, y1, 
			   ifelse(z1 < high, y1, y1))
    y.1 = ifelse(z2 > high | z1 < low, 0, y.1)
	
	c = (z2 -low) / (z2 - z1)
	x.2 = ifelse(z2 == -Inf, x1,
			 ifelse(z2 <= low, x2 - c * (x2 - x1), NA))
	x.2 = ifelse(z2 > high | z1 < low, 0, x.2)
			 
	y.2 = ifelse(z2 == -Inf, y1,
			  ifelse(z2 <= low, y1, NA))
	y.2 = ifelse(z2 > high | z1 < low, 0, x.2)
	
	
	## second condiction
	c = (z1 - low) / (z1 - z2)
	x_1 = ifelse(z1 > low, x1, 
				ifelse(z1 == -Inf, x2, x1 + c * (x2 - x1)))
	x_1 = ifelse(z2 < low | z1 > high, 0, x_1)
				
	y_1 = ifelse(z1 > low, y1, 
				ifelse(z1 == -Inf, y1, y1))
	y_1 = ifelse(z2 < low | z1 > high, 0, y_1)
				
	c = (z2 - high) / (z2 - z1)
	x_2 = ifelse(z2 < high, x1, 
				ifelse(z2 == Inf, x1, x2 - c * (x2 - x1)))
	x_2 = ifelse(z2 < low | z1 > high, 0, x_2)
				
	y_2 = ifelse(z2 < high, y1, 
				ifelse(z2 == Inf, y1, y1))
	y_2 = ifelse(z2 < low | z1 > high, 0, y_2)
				
	x..1 = ifelse(low <= z1 & z1 <= high, x1, x1)
	y..1 = ifelse(low <= z1 & z1 <= high, y1, y1)
	
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

	xout = cbind(xout.1, xout.2)
	yout = cbind(yout.1, yout.2)
	list(xout, yout)
}


reduceProc = function(v)
{
	v1 <<- cbind(v[,1], 
				reduce(v[,2], v[,3]), 
				reduce(v[,4], v[,5]), 
				reduce(v[,6], v[,7]), 
				v[,8])

	v2 <<- cbind(v1[,1], v1[,2], 
				reduce(v1[,3], v1[,4]),
				reduce(v1[,5], v1[,6]), 
				reduce(v1[,7], v1[,8]))

	v3 <<- cbind(v2[,1], v2[,2], v2[,3], 
				reduce(v2[,4], v2[,5]), 
				reduce(v2[,6], v2[,7]), v2[,8])

	v4 <<- cbind(v3[,1:4],
				reduce(v3[,5], v3[,6]),
				reduce(v3[,7], v3[,8]))
				
	v5 <<- cbind(v4[,1:5], reduce(v4[,6], v4[,7]), v4[,8])
	v6 <<- cbind(v5[,1:6], reduce(v5[,7], v5[,8]))
	v7 <<- cbind(v6[,1:5], reduce(v6[,6], v6[,7]), v6[,8])
				

	colnames(v7) = paste('v',1:8, sep = '')

	v7
}
