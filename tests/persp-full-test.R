

## test on theta
	trans = testPersp(15)
	echoTest(trans)

	trans = testPersp(60)
	echoTest(trans)

	trans = testPersp(120)
	echoTest(trans)

	trans = testPersp(160)
	echoTest(trans)

	trans = testPersp(220)
	echoTest(trans)

	trans = testPersp(280)
	echoTest(trans)

	trans = testPersp(350)
	echoTest(trans)


## test on phi
	trans = testPersp(phi = 15)
	echoTest(trans)

	trans = testPersp(phi = 22.5)
	echoTest(trans)

	trans = testPersp(phi = 50)
	echoTest(trans)

	trans = testPersp(phi =80)
	echoTest(trans)

	trans = testPersp(phi = 120)
	echoTest(trans)

	trans = testPersp(phi =310)
	echoTest(trans)

	trans = testPersp(phi = 720)
	echoTest(trans)


## test on expand
	trans = testPersp(expand = 0.01)
	echoTest(trans)

	trans = testPersp(expand = 0.2)
	echoTest(trans)

	trans = testPersp(expand = 0.35)
	echoTest(trans)

	trans = testPersp(expand = 2)
	echoTest(trans)

	trans = testPersp(expand = 5)
	echoTest(trans)

	trans = testPersp(expand = 10)
	echoTest(trans)

	trans = testPersp(expand = 100)
	echoTest(trans)

## test on lim
## Bug on here...........................................................................
	trans = testPersp(xlim = c(-5,5))
	echoTest(trans)

	trans = testPersp(ylim = c(-5,5))
	echoTest(trans)

	trans = testPersp(zlim = c(-5,5))
	echoTest(trans)

## test on label
	trans = testPersp(xlab = 'xx', ylab = 'yy', zlab = 'zz')
	echoTest(trans)

## test on r
	trans = testPersp(r = 5)
	echoTest(trans)

	trans = testPersp(r = 10)
	echoTest(trans)

	trans = testPersp(r = 20)
	echoTest(trans)

##test on d
	trans = testPersp(d = 0.5)
	echoTest(trans)

	trans = testPersp(d = 0)
	echoTest(trans)

	trans = testPersp(d = 1)
	echoTest(trans)

## test on scale
	trans = testPersp(scale = FALSE)
	echoTest(trans)

	trans = testPersp(scale = TRUE)
	echoTest(trans)

## test on expand
	trans = testPersp(expand = 0.5)
	echoTest(trans)

	trans = testPersp(expand = 1)
	echoTest(trans)

	trans = testPersp(expand = 0.25)
	echoTest(trans)

	trans = testPersp(d = 2)
	echoTest(trans)

## test on col
	trans = testPersp(col = 1)
	echoTest(trans)

	trans = testPersp(col = 'blue')
	echoTest(trans)

	trans = testPersp(col = 1:5)
	echoTest(trans)

	trans = testPersp(col = 1:10)
	echoTest(trans)

	trans = testPersp(col = 1:100)
	echoTest(trans)

## test on border
	trans = testPersp(border = 1)
	echoTest(trans)

	trans = testPersp(border = 'brown')
	echoTest(trans)

	trans = testPersp(border = 5:6)
	echoTest(trans)

## test on axes
	trans = testPersp(axes = TRUE)
	echoTest(trans)

	trans = testPersp(axes = FALSE)
	echoTest(trans)

## if box = False then not drawing any axes even axes = TRUE
	trans = testPersp(box = FALSE, axes = TRUE)
	echoTest(trans)

	trans = testPersp(box = FALSE, axes = FALSE)
	echoTest(trans)

## test on ticktype
	trans = testPersp(ticktype = 'detail', axes = TRUE, box = FALSE)
	echoTest(trans)
    
    
## test on lty
	trans = testPersp(lty = 'dotted')
	echoTest(trans)
    
    trans = testPersp(lty = '1331')
	echoTest(trans)
    
    
## test on lwd
	trans = testPersp(lwd = 2)
	echoTest(trans)
    
    trans = testPersp(lwd = 3)
	echoTest(trans)
    
## other bugs:
    ## 1. pty = 's'
    
## test on shade
    ## single colors
    trans = testPersp(col = 'orange', border = 'NA', shade =0.2, box = FALSE, scale = TRUE)
    echoTest(trans)
    ## multiplue colors:
    trans = testPersp(col = 1:10, border = 'NA', shade =0.2, box = FALSE, scale = TRUE)
    echoTest(trans)
    ## changing the scale 
    trans = testPersp(col = 1:10, border = 'NA', shade =0.2, box = FALSE, scale = FALSE)
    echoTest(trans)
    
## lty/lwd
    trans = testPersp(lty = 5, lwd = 2)
    echoTest(trans)

## col.lab/cex.lab/col.axis
    trans = testPersp(col.lab = 'red', cex.lab = 1.5, col.axis = 'green', ticktype = 'detail')
    echoTest(trans)
