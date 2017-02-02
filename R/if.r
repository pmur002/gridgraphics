FindCutPoints = function(low, high, x1, y1, z1, x2, y2, z2,
                        x, y, z, npt){
    print(z2)
    
    if( z1 > z2)
    {
        if( z2 > high || z1 < low) {
        #return(out = list(x = x, y = y, z = z, npt = npt))
        }
        if(z1 < high)
        {   
            print("if1")
            x[npt] = x1
            y[npt] = y1
            z[npt] = z1
            npt = npt + 1
        } else if(z1 == Inf)
        {
            print("if2")
            x[npt] = x2
            y[npt] = y1
            z[npt] = z2
            npt = npt + 1
        } else{
            print("if3")
            c = (z1 - high)/(z1 - z2)
            x[npt] = x1 + c * (x2 - x1)
            y[npt] = y1
            z[npt] = z1 + c* (z2 - z1)
            npt = npt + 1
        }
    ##
    if (z2 == -Inf) {
    print("if4")
	    x[npt] = x1
	    y[npt] = y1
	    z[npt] = z1
	    npt = npt + 1
	} else if (z2 <= low) { 
    print("if5")
	    c = (z2 -low) / (z2 - z1)
	    x[npt] = x2 - c * (x2 - x1)
	    y[npt] = y1
	    z[npt] = z2 - c * (z2 - z1)
	    npt = npt + 1
	}
    
    } else if (z1 < z2) {
    print("if6")
	if (z2 < low || z1 > high){
    print("if7")
        #return(out = list(x = x, y = y, z = z, npt = npt))
        }
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
