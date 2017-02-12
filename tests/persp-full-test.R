

## test on theta
	trans = testPersp(15)
	grid.echo()

	trans = testPersp(60)
	grid.echo()

	trans = testPersp(120)
	grid.echo()

	trans = testPersp(160)
	grid.echo()

	trans = testPersp(220)
	grid.echo()

	trans = testPersp(280)
	grid.echo()

	trans = testPersp(350)
	grid.echo()


## test on phi
	trans = testPersp(phi = 15)
	grid.echo()

	trans = testPersp(phi = 22.5)
	grid.echo()

	trans = testPersp(phi = 50)
	grid.echo()

	trans = testPersp(phi =80)
	grid.echo()

	trans = testPersp(phi = 120)
	grid.echo()

	trans = testPersp(phi =310)
	grid.echo()

	trans = testPersp(phi = 720)
	grid.echo()


## test on expand
	trans = testPersp(expand = 0.01)
	grid.echo()

	trans = testPersp(expand = 0.2)
	grid.echo()

	trans = testPersp(expand = 0.35)
	grid.echo()

	trans = testPersp(expand = 2)
	grid.echo()

	trans = testPersp(expand = 5)
	grid.echo()

	trans = testPersp(expand = 10)
	grid.echo()

	trans = testPersp(expand = 100)
	grid.echo()

## test on lim
## Bug on here...........................................................................
	trans = testPersp(xlim = c(-5,5))
	grid.echo()

	trans = testPersp(ylim = c(-5,5))
	grid.echo()

	trans = testPersp(zlim = c(-5,5))
	grid.echo()

## test on label
	trans = testPersp(xlab = 'xx', ylab = 'yy', zlab = 'zz')
	grid.echo()

## test on r
	trans = testPersp(r = 5)
	grid.echo()

	trans = testPersp(r = 10)
	grid.echo()

	trans = testPersp(r = 20)
	grid.echo()

##test on d
	trans = testPersp(d = 0.5)
	grid.echo()

	trans = testPersp(d = 0)
	grid.echo()

	trans = testPersp(d = 1)
	grid.echo()

## test on scale
	trans = testPersp(scale = FALSE)
	grid.echo()

	trans = testPersp(scale = TRUE)
	grid.echo()

## test on expand
	trans = testPersp(expand = 0.5)
	grid.echo()

	trans = testPersp(expand = 1)
	grid.echo()

	trans = testPersp(expand = 0.25)
	grid.echo()

	trans = testPersp(d = 2)
	grid.echo()

## test on col
	trans = testPersp(col = 1)
	grid.echo()

	trans = testPersp(col = 'blue')
	grid.echo()

	trans = testPersp(col = 1:5)
	grid.echo()

	trans = testPersp(col = 1:10)
	grid.echo()

	trans = testPersp(col = 1:100)
	grid.echo()

## test on border
	trans = testPersp(border = 1)
	grid.echo()

	trans = testPersp(border = 'brown')
	grid.echo()

	trans = testPersp(border = 5:6)
	grid.echo()

## test on axes
	trans = testPersp(axes = TRUE)
	grid.echo()

	trans = testPersp(axes = FALSE)
	grid.echo()

## if box = False then not drawing any axes even axes = TRUE
	trans = testPersp(box = FALSE, axes = TRUE)
	grid.echo()

	trans = testPersp(box = FALSE, axes = FALSE)
	grid.echo()

## test on ticktype
	trans = testPersp(ticktype = 'detail', axes = TRUE, box = FALSE)
	grid.echo()
    
    
## test on lty
	trans = testPersp(lty = 'dotted')
	grid.echo()
    
    trans = testPersp(lty = '1331')
	grid.echo()
    
    
## test on lwd
	trans = testPersp(lwd = 2)
	grid.echo()
    
    trans = testPersp(lwd = 3)
	grid.echo()
    
## other bugs:
    ## 1. pty = 's'
    
## test on shade
    ## single colors
    trans = testPersp(col = 'orange', border = 'NA', shade =0.2, box = FALSE, scale = TRUE)
    grid.echo()
    ## multiplue colors:
    trans = testPersp(col = 1:10, border = 'NA', shade =0.2, box = FALSE, scale = TRUE)
    grid.echo()
    ## changing the scale 
    trans = testPersp(col = 1:10, border = 'NA', shade =0.2, box = FALSE, scale = FALSE)
    grid.echo()
    
## lty/lwd
    trans = testPersp(lty = 5, lwd = 2)
    grid.echo()

## col.lab/cex.lab/col.axis
    trans = testPersp(col.lab = 'red', cex.lab = 1.5, col.axis = 'green', ticktype = 'detail')
    grid.echo()
