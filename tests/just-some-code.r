
xx = recordPlot()
x = xx[[1]][[11]][[2]][[2]][1:6]
y = xx[[1]][[11]][[2]][[3]][1:6]
z = xx[[1]][[11]][[2]][[4]][1:6, 1:6]
s = xx[[1]][[11]][[2]][[5]][1:6]


xt = rep(rep(x, each = length(y)), each = length(s))
yt = rep(rep(x, length(x)), each = length(s))
zt = rep(as.numeric(z), each = length(s))
st = rep(s, (length(x) - 1) * (length(y) - 1))

xr = rep(x, each = length(y))
yr = rep(y, length(x))
zr = as.numeric(z)

tot = length(x) * length(y)
det = seq(1, length(xr), by = length(x))
x1 = xr[1:(tot - length(x))][-det]
x2 = xr[(length(y) + 1):tot][-det]
y1 = yr[1:(tot - length(x))][-(det - 1)]
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

scut = seq(0, length(st), by = 6)[-1]
low = st[-scut]
high = st[-((scut - 6) + 1)]


## vectors-join
source('C:/Users/yeamin/Desktop/master/loading.r')
v.1 = find(low = low, high = high, x1 = x2, y1 = y1, x2 = x2, y2 = y1, z1 = z11, z2 = z21)[[1]]
v.2 = find(low = low, high = high, x1 = y1, y1 = x2, x2 = y2, y2 = x2, z1 = z21, z2 = z22)[[1]]
v.3 = find(low = low, high = high, x1 = x2, y1 = y2, x2 = x1, y2 = y2, z1 = z22, z2 = z12)[[1]]
v.4 = find(low = low, high = high, x1 = y2, y1 = x1, x2 = y1, y2 = x1, z1 = z12, z2 = z11)[[1]]

v_1 = find(low = low, high = high, x1 = x1, y1 = y1, x2 = x2, y2 = y1, z1 = z11, z2 = z21)[[2]]
v_2 = find(low = low, high = high, x1 = y1, y1 = x2, x2 = y2, y2 = x2, z1 = z21, z2 = z22)[[2]]
v_3 = find(low = low, high = high, x1 = x2, y1 = y2, x2 = x1, y2 = y2, z1 = z22, z2 = z12)[[2]]
v_4 = find(low = low, high = high, x1 = y2, y1 = x1, x2 = y1, y2 = x1, z1 = z12, z2 = z11)[[2]]

vx = cbind(v.1, v_2, v.3, v_4)
vy = cbind(v_1, v.2, v_3, v.4)
xPoints = reduceProc(vx)
yPoints = reduceProc(vy)

## not done yet
grid.newpage()
pushViewport(vp = viewport(0.5, 0.5, 1, 1, xscale = range(x), yscale = range(y)))

grid.polygon(out$x[1:npt], out$y[1:npt], default.units = 'native',
             gp = gpar(fill = cols, col = NA))
