#creating data
z <- rnorm(30)
z
x <- 2 * z + rnorm(30) / 5
x
y <- 3 * z + rnorm(30) / 5
y
cor(x, y)

#technique 1 :: using pcor function of package ppcor
library(ppcor)
pcor(cbind(x, y, z))
pcor(cbind(x, y, z))$estimate
#first row second column element is partial correlation between x and y given the rest of the variables, here only z

#technique 2 :: using regression
x_on_z <- lm(x~z)
y_on_z <- lm(y~z)
cor(x_on_z$residuals, y_on_z$residuals)

#technique 3 :: using matrices
dat <- cbind(z, x, y)
v <- cov(dat)
v
A <- v[1,1]
A
B <- v[1, 2:3]
B
C <- v[2:3, 2:3]
C
D <- C - B%*%t(B) / A # as A is scalar, only dividing it works
D
D[1, 2] / sqrt(D[1, 1] * D[2, 2])
