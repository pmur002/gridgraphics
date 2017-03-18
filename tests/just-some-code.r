#cols.out = 0
xx = recordPlot()
x = xx[[1]][[12]][[2]][[2]]
y = xx[[1]][[12]][[2]][[3]]
z = xx[[1]][[12]][[2]][[4]]
s = xx[[1]][[12]][[2]][[5]]
cols = xx[[1]][[12]][[2]][[6]]

xt = rep(rep(x, each = length(y)), each = length(s))
yt = rep(rep(x, length(x)), each = length(s))
zt = rep(as.numeric(z), each = length(s))
st = rep(s, ((length(x) - 1)) * ((length(y) - 1)))

xr = rep(x, each = length(y))
yr = rep(y, length(x))
zr = as.numeric(z)

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


x1 = rep(x1, each = length(s) - 1)
x2 = rep(x2, each = length(s) - 1)
y1 = rep(y1, each = length(s) - 1)
y2 = rep(y2, each = length(s) - 1)
z11 = rep(z11, each = length(s) - 1)
z12 = rep(z12, each = length(s) - 1)
z21 = rep(z21, each = length(s) - 1)
z22 = rep(z22, each = length(s) - 1)

scut = seq(0, length(st), by = length(s))[-1]
low = st[-scut]
high = st[-((scut - length(s)) + 1)]

k = (rep((1:length(s)), (length(x) - 1) * (length(y) - 1)) - 1)
ncol = length(cols)
colrep = cols[(k %% ncol) + 1][-scut]



## vectors-join
source('C:/Users/yeamin/Desktop/master/MasterProject/gridGraphics/gridgraphics/tests/loading.R')
v1 = vFindCutPoints(low = low, high = high, x1 = x1, y1 = y1, x2 = x2, y2 = y1, z1 = z11, z2 = z21)
v2 = vFindCutPoints(low = low, high = high, x1 = y1, y1 = x2, x2 = y2, y2 = x2, z1 = z21, z2 = z22)
v3 = vFindCutPoints(low = low, high = high, x1 = x2, y1 = y2, x2 = x1, y2 = y2, z1 = z22, z2 = z12)
v4 = vFindCutPoints(low = low, high = high, x1 = y2, y1 = x1, x2 = y1, y2 = x1, z1 = z12, z2 = z11)

vx = cbind(v1[[1]], v2[[2]], v3[[1]], v4[[2]])
vy = cbind(v1[[2]], v2[[1]], v3[[2]], v4[[1]])


index = rowSums(!is.na(vx))
vx = t(vx)
vy = t(vy)
xcoor.na = as.vector(vx[, index >= 1])
ycoor.na = as.vector(vy[, index >= 1])
xcoor = xcoor.na[!is.na(xcoor.na)]
ycoor = ycoor.na[!is.na(ycoor.na)]

id.length = index[index != 0]


## not done yet
grid.newpage()
pushViewport(vp = viewport(0.5, 0.5, 1, 1, xscale = range(x), yscale = range(y)))

grid.polygon(xcoor, ycoor, default.units = 'native', id.length = id.length,
             gp = gpar(fill = cols.out, col = NA))
