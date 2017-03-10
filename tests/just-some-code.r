
<<<<<<< HEAD
xx = recordPlot()
x = xx[[1]][[11]][[2]][[2]][1:6]
y = xx[[1]][[11]][[2]][[3]][1:6]
z = xx[[1]][[11]][[2]][[4]][1:6, 1:6]
s = xx[[1]][[11]][[2]][[5]][1:6]


xt = rep(rep(x, each = length(y)), each = length(s))
yt = rep(rep(x, length(x)), each = length(s))
zt = rep(as.numeric(z), each = length(s))
st = rep(s, length(x) * length(y))

xr = rep(x, each = length(y))
yr = rep(y, length(x))
zr = as.numeric(z)

tot = length(x) * length(y)
det = seq(1, length(xr), by = length(x))
x1 = xr[1:(tot - length(x))][-det]
x2 = xr[(length(y) + 1):tot][-det]
y1 = yr[1:(tot - length(x))][-det]
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

low = st[1:((length(s) - 1) *(length(x) - 1) * (length(y) - 1))]
high = st[(length(s)+1):(length(s) *(length(x) - 1) * (length(y) - 1))]

z1 = z11; z2 = z21; x1 = x1; x2 = x2;

find = function(low = 3, high = 4, x1, x2, z1, z2)
{
  v1 = v2 = v3 = rep(NA, length(z1))
  
  c = (z1 - high) / (z1 - z2)
  v1 = ifelse(z2 > high || z1 < low, v1, numeric(length(z1)))
  v2 = ifelse(z1 < high, x1, 
              ifelse(z1 == Inf, x2, x1 + c * (x2 - x1)))
  
  c = (z2 -low) / (z2 - z1)
  v3= ifelse(z2 == -Inf, x1,
             ifelse(z2 <= low, x2 - c * (x2 - x1), NA))
  
  cbind(v2, v3)
}

## vectors-join
reduce = function(vbind, vremian)
{
  vna = is.na(vbind)
  vbind[vna] = vremian[vna]
  vremian[vna] = NA
  cbind(vbind, vremian)
}

## inside if z1 > z2
vout1 = find(x1 = x1, x2 = x2, z1 = z11, z2 = z21)
vout2 = find(x1 = y1, x2 = x2, z1 = z21, z2 = z21)
vout3 = find(x1 = x2, x2 = y2, z1 = z22, z2 = z12)
vout4 = find(x1 = y2, x2 = x1, z1 = z12, z2 = z11)

vp0 = cbind(vout1, vout2, vout3, vout4)

vp1 = cbind(vp0[,1], 
            reduce(vp0[,2], vp0[,3]), 
            reduce(vp0[,4], vp0[,5]), 
            reduce(vp0[,6], vp0[,7]), 
            vp0[,8])

vp2 = cbind(vp1[,1], vp1[,2], 
            reduce(vp1[,3], vp1[,4]),
            reduce(vp1[,5], vp1[,6]), 
            reduce(vp1[,7], vp1[,8]))

vp3 = cbind(vp2[,1], vp2[,2], vp2[,3], 
            reduce(vp2[,4], vp2[,5]), 
            reduce(vp2[,6], vp2[,7]), vp2[,8])

vp4 = cbind(vp3[,1:4],
            reduce(vp3[,5], vp3[,6]),
            vp3[,7], vp3[,8])


# in side z2 < low || z1 > high
npt = 3
c = (z1 - low) / (z1 - z2)
o1[, npt] = ifelse(z1 > low, x1,
                  ifelse(z1 == -Inf, x2, x1 + c * (x2 - x1)))

c = (z2 - high) / (z2 - z1)
o2[, npt] = ifelse(z2 < high, o[, npt], 
                  ifelse(z2 == Inf, x1, x2 - c * (x2 - x1) ))

## inside else
npt = npt + 1
o3[, npt] = ifelse(low <= z1 && z1 <= high, x1, o[, npt])

##overall 
out = ifelse(z1 > z2, o, 
             ifelse(z1 < z2, o1,
                    ifelse()))
=======
i = 0
ifelse(5 >= 1:3, {1:2; 1:2; i = i + 1}, 3:4)
a = list(a, list())



if (z1 < high) {
  x[npt + 1] = x1
  y[npt + 1] = y1
  z[npt + 1] = z1
  npt = npt + 1
}

npt = 0
x = y = z = 0
z1 = c(1,5,2,3,5)
high = 3

x = x1[z1 < high]
y = y1[z1 < high]
z = z1[z1 < high]

xx = recordPlot()
x = xx[[1]][[11]][[2]][[2]]
y = xx[[1]][[11]][[2]][[3]]
z = xx[[1]][[11]][[2]][[4]]
s = xx[[1]][[11]][[2]][[5]]


ind = ((length(x) * length(y)) * (length(s) - 1))
xt = rep(rep(x, each = length(y)), each = length(s))
yt = rep(rep(x, length(x)), each = length(s))
zt = rep(as.numeric(z), each = length(s))
st = rep(s, length(x) * length(y))


xtop = xt[1:ind]
xbot = xt[(400 + 1): 8000]
ytop = yt[1:ind]
ybot = yt[(400 + 1): 8000]
ztop = zt[1:ind]
zbot = zt[(400 + 1): 8000]

low = 3; high = 4;
FindCutPoints(low, high, xtop, ytop, ztop, xbot, ybot, zbot)
>>>>>>> origin/master
