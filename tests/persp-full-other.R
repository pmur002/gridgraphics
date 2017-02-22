
setwd('C:/Users/yeamin/Desktop/mproject/diff/Torus/')

## test on theta

plotdiff(expression(testPersp1(30)), 'persp-2')
plotdiff(expression(testPersp1(175)), 'persp-3')
plotdiff(expression(testPersp1(285)), 'persp-4')

## test on phi
plotdiff(expression(testPersp1(phi = 15)), 'persp-5')
plotdiff(expression(testPersp1(phi = 22.5)), 'persp-6')
plotdiff(expression(testPersp1(phi = 80)), 'persp-7')
plotdiff(expression(testPersp1(phi = 310)), 'persp-8')
## test on expand
plotdiff(expression(testPersp1(epand = 0.01)), 'persp-9')
plotdiff(expression(testPersp1(epand = 0.2)), 'persp-10')
plotdiff(expression(testPersp1(epand = 2)), 'persp-11')
plotdiff(expression(testPersp1(epand = 10)), 'persp-12')
## test on lim
plotdiff(expression(testPersp1(xlim = c(-5,5))), 'persp-13')
plotdiff(expression(testPersp1(ylim = c(-5,5))), 'persp-14')
plotdiff(expression(testPersp1(zlim = c(-5,5))), 'persp-15')

## test on label
plotdiff(expression(
  testPersp1(xlab = 'xx', ylab = 'yy', zlab = 'zz')), 'persp-16')

## test on r
plotdiff(expression(testPersp1(r = 5)), 'persp-17')
plotdiff(expression(testPersp1(r = 10)), 'persp-18')
plotdiff(expression(testPersp1(r = 20)), 'persp-19')


##test on d
plotdiff(expression(testPersp1(d = 0.5)), 'persp-20')
plotdiff(expression(testPersp1(d = 0)), 'persp-21')
plotdiff(expression(testPersp1(d = 5)), 'persp-22')


## test on scale
plotdiff(expression(testPersp1(scale = FALSE)), 'persp-23')
plotdiff(expression(testPersp1(scale = TRUE)), 'persp-24')

## test on expand
plotdiff(expression(testPersp1(expand = 0.5)), 'persp-25')
plotdiff(expression(testPersp1(expand = 1)), 'persp-26')
plotdiff(expression(testPersp1(expand = 2)), 'persp-27')

## test on col
plotdiff(expression(testPersp1(col = 1)), 'persp-28')
plotdiff(expression(testPersp1(col = 'blue')), 'persp-29')
plotdiff(expression(testPersp1(col = 1:5)), 'persp-30')
plotdiff(expression(testPersp1(col = 1:10)), 'persp-31')
plotdiff(expression(testPersp1(col = 1:100)), 'persp-32')

## test on border
plotdiff(expression(testPersp1(border = 1)), 'persp-33')
plotdiff(expression(testPersp1(border = 'brown')), 'persp-34')
# only one color for border
plotdiff(expression(testPersp1(border = 5:6)), 'persp-35')

## test on axes
plotdiff(expression(testPersp1(axes = TRUE)), 'persp-36')
plotdiff(expression(testPersp1(axes = FALSE)), 'persp-37')

## if box = False then not drawing any axes even axes = TRUE
plotdiff(expression(testPersp1(box = FALSE, axes = TRUE)), 'persp-38')
plotdiff(expression(testPersp1(box = FALSE, axes = FALSE)), 'persp-39')

## test on ticktype
plotdiff(expression(
  testPersp1(ticktype = 'detail', axes = TRUE, box = TRUE)), 'persp-40')

## test on lty
plotdiff(expression(testPersp1(lty = 'dotted')), 'persp-41')
plotdiff(expression(testPersp1(lty = '1331')), 'persp-42')


## test on lwd
plotdiff(expression(testPersp1(lwd = 2)), 'persp-43')
plotdiff(expression(testPersp1(lwd = 3)), 'persp-44')

## other bugs:
## 1. pty = 's'

## test on shade
## single colors
plotdiff(expression(testPersp1(col = 'orange', border = 'NA', 
                              shade =0.2, box = FALSE, 
                              scale = TRUE)), 'persp-45')
plotdiff(expression(testPersp1(col = 1:10, border = 'NA', 
                              shade =0.2, box = FALSE, 
                              scale = TRUE)), 'persp-46')
plotdiff(expression(testPersp1(col = 1:10, border = 'NA', 
                              shade =0.2, box = FALSE, 
                              scale = FALSE)), 'persp-47')

## col.lab/cex.lab/col.axis
plotdiff(expression(testPersp1(col.lab = 'red', cex.lab = 1.5, 
                              col.axis = 'green', ticktype = 'detail')), 
         'persp-48')

name.graphics = paste('persp-', 1:48, '-graphics.png', sep = '')
name.grid = paste('persp-', 1:48, '-grid.png', sep= '')
name.out = paste('out-', 1:48, '.png', sep = '')
cmd = paste('compare', name.graphics, name.grid, name.out, sep = ' ')
for(i in 1:48)
{
  system(cmd[i])
}

