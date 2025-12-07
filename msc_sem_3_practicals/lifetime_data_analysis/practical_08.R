rm(list=ls())
lambda.true <- 0.5
n <- 100
r <- 75      # for type-II censoring
sim <- 1000

estimates <- data.frame(
  complete = numeric(sim),
  type2 = numeric(sim),
  random = numeric(sim)
)

for (i in 1:sim) {
  # complete
  x <- rexp(n, rate = lambda.true)
  lambda.hat.complete <- 1 / mean(x)
  
  # type-II
  x.sorted <- sort(x)
  lambda.hat.type2 <- r / ((n - r) * x.sorted[r] + sum(x.sorted[1:r]))
  
  # random
  ctime <- rexp(n, rate = lambda.true * 0.8)
  obs.time <- pmin(x, ctime)
  delta <- as.numeric(x <= ctime)
  lambda.hat.random <- sum(delta) / sum(obs.time)
  
  estimates[i, ] <- c(lambda.hat.complete, lambda.hat.type2, lambda.hat.random)
}


apply(estimates, 2, mean)
