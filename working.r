source('C:/Users/yeamin/Desktop/mproject/loading.r')

x <- seq(-20, 20, length = 30)
y <- seq(-20, 20, length = 35)
z <- outer(x, y, function(a, b) a*b^2)
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
trans <- persp(x, y, z, col = color[facetcol], phi = 30, theta = -30, axes = FALSE)
par('usr')
PerspWindow(range(x),range(y),range(z), trans, 'r')

grid.echo()


##demo compare
setwd('C:/Users/yeamin/Desktop/mproject/diff')
x <- seq(-20, 20, length = 30)
y <- seq(-20, 20, length = 35)
z <- outer(x, y, function(a, b) a*b^2)
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
trans <- persp(x, y, z, col = color[facetcol], phi = 30, theta = -30, axes = FALSE)

plotdiff(expression(persp(x, y, z, col = color[facetcol], phi = 30, theta = -30, axes = FALSE)), 'persp')
system('compare persp-graphics.pdf persp-grid.pdf out.pdf out1.pdf')


## filled countour
source('C:/Users/yeamin/Desktop/mproject/loading.r')
plot = recordPlot()
C_filledcontour(plot)

FindCutPoints(low, high, y1,  x2,  z21, y2,  x2,  z22, y, x, z, d$npt)
