FindCutPoints = function( low,  high,
	       x1,  y1,  z1,
	       x2,  y2,  z2,
	       x,  y,  z,
	       npt)
{

	if (z1 > z2 ) {
		if (z2 > high || z1 < low) return(out = list(x = x, y = y, z = z, npt = npt))
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
			print(12)
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
	if (z2 < low || z1 > high) return(out = list(x = x, y = y, z = z, npt = npt))
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
	#ifdef OMIT
		   if(FALSE){
			x[npt + 1] = x2
			y[npt + 1] = y2
			z[npt + 1] = z2
			npt = npt + 1
			}
	#endif
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
			print(123)
			x[npt + 1] = x1
			y[npt + 1] = y1
			z[npt + 1] = z1
			npt = npt + 1
	#ifdef OMIT
		   if(FALSE){
			x[npt + 1] = x2
			y[npt + 1] = y2
			z[npt + 1] = z2
			npt = npt + 1
	#endif
			}
		}
    }
	out = list(x = x, y = y, z = z, npt = npt)
	out
}
