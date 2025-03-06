f <- function(x) 2*x^3 - 2*x - 5

f_dash <- function(x) 6*x^2 - 2

x <- c(1.5)

iterations <- 10

for (i in 1:iterations) {
  x[i+1] <- x[i] - f(x[i]) / f_dash(x[i])
}

root <- x[length(x)]; root

plot(x, col = "blue", pch = 19)
