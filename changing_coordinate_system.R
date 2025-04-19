equation_of_parallel_line <- function(x0, y0, a, b, c){
  
  # we want to obatin the equation of a straight line parallel to ax + by + c = 0 passing through (x0, y0)
  
  lambda <- -(a * x0 + b * y0)
  
  return(c(a, b, lambda))
}

point_of_intersection <- function(a1, b1, c1, a2, b2, c2){
  
  # we want the point of intersetion of two straight lines a1 * x + b1 * y + c1 = 0 and a2 * x + b2 * y + c2 = 0
  
  # here we are sure that they intersect
  
  x <- (b1 * c2 - b2 * c1) / (a1 * b2 - a2 * b1)
  
  y <- (c1 * a2 -  c2 * a1) / (a1 * b2 - a2 * b1)
  
  return(c(x, y))
}

new_coordiantes <- function(x, y, a1, b1, c1, a2, b2, c2){
  
  # (x, y) are the coordinates of a point in standard coordinate system
  
  # our new basis has two staright lines a1 * x + b1 * y + c1 = 0 and a2 * x + b2 * y + c2 = 0
  
  # we want new coordinates of (x, y) w.r.t. our new coordinate system
  
  
  # first we obtain the new x-coordinate
  
  # the following is for equation of a straight line parallel to our new y-axis passing through (x, y)
  
  l1 <- equation_of_parallel_line(x, y, a2, b2, c2)
  
  # now we want point of intersection of new x-axis and l1
  
  poi1 <- point_of_intersection(a1, b1, c1, l1[1], l1[2], l1[3])
  
  # new x-coordinate is simply the abscissa of this point of intersection
  
  new_x <- poi1[1]
  
  
  # now we obtain the new y-coordinate
  
  # the following is for equation of a straight line parallel to our new x-axis passing through (x, y)
  
  l2 <- equation_of_parallel_line(x, y, a1, b1, c1)
  
  # now we want point of intersection of new y-axis and l2
  
  poi2 <- point_of_intersection(a2, b2, c2, l2[1], l2[2], l2[3])
  
  # new y-coordinate is simply the ordinate of this point of intersection
  
  new_y <- poi2[2]
  
  return(c(new_x, new_y))
}

A <- matrix(c(4, 2, 1, 3), nrow = 2, ncol = 2, byrow = TRUE); A

# eigenvalues of A are 2 and 5 with corresponding eigenvectors (1, -1) and (2, 1) respectively
# we consider our new x-axis in the direction provided by (1, -1), so it is the straight line x + y = 0
# and our new y-axis in the direction provided by (2, 1), so it is the straight line x - 2y = 0

# we take a vector v1
v1 <- matrix(c(-1, 3), nrow = 2); v1

A %*% v1 # this is the result of A applied on v1

# in the new coordinate system v1 will be
v1_new <- matrix(new_coordiantes(-1, 3, 1, 1, 0, 1, -2, 0), nrow = 2); v1_new

# diagonal form of A
D <- diag(c(2, 5)); D

D %*% v1_new # result of D applied on new representation of v1 
# this must be same as new representation of (A %*% v1)

new_coordiantes(2, 8, 1, 1, 0, 1, -2, 0)


