lambda <- 4

s <- rpois(100, lambda = lambda)

numbers <- s

mean(numbers)

lambda_hat <- c()

for (i in 1:1000) {
  lambda_hat[i] <- mean(rpois(100, lambda))
}

hist(sqrt(100) * (lambda_hat - lambda), probability = TRUE)
curve(dnorm(x, 0, sqrt(lambda)), lwd = 2, add = TRUE)
