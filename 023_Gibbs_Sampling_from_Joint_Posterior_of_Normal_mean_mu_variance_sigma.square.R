# true parameter values
mu <- 2; sigma <- 2

# sample
n <- 100

set.seed(14)
x <- rnorm(n, mean = mu, sd = sigma)

hist(x)

# prior for mu
mu0 <- 1.5; tau <- 1

# prior for sigma.square
a <- 2; b <- 2

# posterior variance of mu
mu.posterior.var <- function(sigma) 1 / ( (n / sigma^2) + (1 / tau^2) )

# posterior mean of mu
mu.posterior.mean <- function(sigma) mu.posterior.var(sigma) * ( (sum(x) / sigma^2) + (mu0 / tau^2) )

# posterior shape of sigma.square
sigma.sq.posterior.a <- a + (n / 2)

# posterior rate of sigma.square
sigma.sq.posterior.b <- function(mu) b + sum((x - mu)^2) / 2 

# posterior sample size 
size <- 1000

# initial values
estimates <- matrix(c(1, 1), nrow = size, ncol = 2, byrow = TRUE)

estimates[1, ]

# sample generation
for (i in 2:size) {
  
  mu.post.var <- mu.posterior.var(sqrt(estimates[i-1, 2]))
  
  estimates[i, 1] <- rnorm(1, 
                           mean = mu.posterior.mean(sqrt(estimates[i-1, 2])),
                           sd = sqrt(post.var))
  
  estimates[i, 2] <- 1 / rgamma(1, 
                            shape = sigma.sq.posterior.a,
                            rate = sigma.sq.posterior.b(estimates[i, 1]))
}

head(estimates)

apply(estimates, 2, mean)
