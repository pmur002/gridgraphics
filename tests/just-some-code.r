
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


x = xx[[1]][[11]][[2]][[2]]
y = xx[[1]][[11]][[2]][[3]]
z = xx[[1]][[11]][[2]][[4]]
s = xx[[1]][[11]][[2]][[5]]

xt = rep(rep(x, each = length(y)), each = length(s))
yt = rep(rep(x, length(x)), each = length(s))
zt = rep(as.numeric(z), each = length(s))
st = rep(s, length(x) * length(y))

mlength(y) * length(s)
