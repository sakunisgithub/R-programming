# bisection method
f <- function(x) x^3 - 2*x^2 + x - 1

a <- 1; b <- 2

iterations <- 100

for(i in 1:iterations){
  
  midpoint <- (a + b) / 2
  
  if(f(midpoint) == 0){
    break
  } else if(f(a) * f(midpoint) < 0){
    a <- a; b <- midpoint
  } else if(f(midpoint) * f(b) < 0){
    a <- midpoint; b <- b
  }
  
}

root <- (a + b) / 2; root


# Newton-Raphson Method

f_dash <- function(x) 3*x^2 - 4*x + 1

x <- c(2)

for (i in 1:100) {
  x[i+1] <- x[i] - f(x[i]) / f_dash(x[i])
}

x[length(x)]
