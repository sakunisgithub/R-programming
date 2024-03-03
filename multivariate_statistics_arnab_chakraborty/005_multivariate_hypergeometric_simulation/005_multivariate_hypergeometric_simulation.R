N <- c(10, 4, 9, 5)
n <- 10

rmhg <- function(N, n){
  p <- length(N)
  vals <- 1:p
  vals
  population <- rep(vals, N)
  population
  x <- sample(population, n)
  x
  return(tabulate(x, p))
}
rmhg(N, n)
rmhg(c(234, 50, 300, 45, 150), 100)
