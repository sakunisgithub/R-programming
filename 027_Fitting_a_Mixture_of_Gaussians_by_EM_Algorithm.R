# data simulation

set.seed(14)

n <- 1000

u <- runif(n)

x <- ifelse(u < 0.6, rnorm(n, mean = 0, sd = 1), rnorm(n, mean = 3, sd = 1))

library(tidyverse)

data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15,
                 color = "black", fill = "red")

# theta = (phi1, phi2, mu1, mu2, sigma1.sq, sigma2.sq)

# Initialization
theta <- c(0.4, 0.6, -1, 1, 2, 2)

epsilon <- 0.0001

# EM Algorithm

repeat{
  # current setting
  phi1 <- theta[1]; phi2 <- theta[2]
  mu1 <- theta[3]; sigma1.sq <- theta[5]
  mu2 <- theta[4]; sigma2.sq <- theta[6]
  
  # E-Step
  joint.prob <- matrix(NA, nrow = n, ncol = 2)
  joint.prob[,1] <- dnorm(x, mu1, sqrt(sigma1.sq)) * phi1
  joint.prob[,2] <- dnorm(x, mu2, sqrt(sigma2.sq)) * phi2
  
  marginal.prob <- rowSums(joint.prob)
  
  posterior.prob <- joint.prob / marginal.prob # using recycling property of R
  
  # M-Step
  
  theta.new <- c()
  
  # update phi
  theta.new[c(1, 2)] <- colMeans(posterior.prob)
  
  # update mu
  theta.new[c(3, 4)] <- colSums(posterior.prob * x) / colSums(posterior.prob)
  
  # update sigma.sq
  temp <- (x - matrix(rep(c(mu1, mu2), n), nrow = n, ncol = 2, byrow = TRUE))^2
  
  theta.new[c(5, 6)] <- colSums(posterior.prob * temp) / colSums(posterior.prob)
  
  if(max(abs(theta - theta.new)) < epsilon){
    theta <- theta.new
    break
  }else{
    theta <- theta.new
  }
}

# Final estimates
theta
