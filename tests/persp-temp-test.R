#setwd('H:/Documents/mproject/MasterProject-master')
setwd('C:/Users/yeamin/Desktop/mproject/MasterProject/standalone/') ##at uni
setwd('H:/Documents/MasterProject-master/MasterProject') ## at home
##################################################################################


## quick example
source('loading.R')
trans = testPersp(col = 1:10, border = 'NA', shade = 0.9, box = FALSE, scale = FALSE)
echoTest(trans)

## lty/lwd
trans = testPersp(lty = 5, lwd = 2)
echoTest(trans)

## col.lab/cex.lab/col.axis
trans = testPersp(col.lab = 'red', cex.lab = 1.5, col.axis = 'green', ticktype = 'detail')
echoTest(trans)


trans = testPersp(col = 'orange', border = 'NA', shade =0.2, box = FALSE, scale = FALSE)
echoTest(trans)



## main example
x = seq(-10,10,length = 60)
y = seq(-10,10,length = 60)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

source('loading.R')
par(mar = c(2,2,2,2))
trans = persp(x, y, z, theta = 20, xlim = c(-5,5),
              phi = 20, expand = 0.32, scale = 0.99,  
              col = 'White', box = TRUE, border = 'orange',col.axis = 'red', ticktype = 'detail',
              col.lab = 'red')
plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = TRUE)
per(plot = plotInfo)





