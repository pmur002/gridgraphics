## global variables.
TickVector = matrix(ncol = 3, byrow = TRUE, data = c(
    0, -1, -1,
    -1, 0, -1,
    0, 1, -1,
    1, 0, -1,
    -1, -1, 0,
    1, -1, 0,
    -1, 1, 0,
    1, 1, 0 ))
Vertex = matrix(ncol = 3, byrow = TRUE, data = c(
	1, 1, 1,  #xlim[1], ylim[1], zlim[1]
	1, 1, 2,  #xlim[1], ylim[1], zlim[2]
	1, 2, 1,
	1, 2, 2,
	2, 1, 1,
	2, 1, 2,
	2, 2, 1,
	2, 2, 2 ))


Face  = matrix (ncol = 4, byrow = TRUE, data = c(
    1, 2, 6, 5,
    3, 7, 8, 4,
    1, 3, 4, 2,
    5, 6, 8, 7,
    1, 5, 7, 3,
    2, 4, 8, 6 ))


Edge  = matrix (ncol = 4, byrow = TRUE, data = c(
    0, 1, 2, 3,
    4, 5, 6, 7,
    8, 7, 9, 0,
    2,10, 5,11,
    3,11, 4, 8,
    9, 6,10, 1)) + 1

    
    
AxisStart = c(1, 1, 3, 5, 1, 5, 3, 7)
