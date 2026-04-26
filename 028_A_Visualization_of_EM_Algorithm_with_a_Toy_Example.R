set.seed(14)

library(tidyverse)

n <- 50

u <- runif(n)

x <- ifelse(u < 0.6, rnorm(n, mean = 0, sd = 1), rnorm(n, mean = 3, sd = 1))

length(x)

l <- function(phi) sapply(phi, function(p) sum(log(p * dnorm(x) + (1 - p) * dnorm(x, 3))))

l(0.1)

ELBO <- function(theta, x, Q){
  
  phi <- theta
  
  sapply(phi, function(p){
    joint.prob <- matrix(NA, nrow = n, ncol = 2)
    joint.prob[,1] <- dnorm(x, 0, 1) * p
    joint.prob[,2] <- dnorm(x, 3, 1) * (1 - p)
  
    return(sum(Q * log(joint.prob / Q)))
  }
  )
}

p <- data.frame(phi = c(0, 1)) %>%
  ggplot(aes(x = phi)) +
  stat_function(fun = l, linewidth = 1.25)

p

# theta = (phi)
theta <- c(0.1)

epsilon <- 0.0001

iterations <- c()

posterior.probs <- list()

repeat{
  # current setting
  phi <- theta
  
  # E-Step
  joint.prob <- matrix(NA, nrow = n, ncol = 2)
  joint.prob[,1] <- dnorm(x, 0, 1) * phi
  joint.prob[,2] <- dnorm(x, 3, 1) * (1 - phi)
  
  marginal.prob <- rowSums(joint.prob)
  
  posterior.prob <- joint.prob / marginal.prob # using recycling property of R
  
  posterior.probs[[length(posterior.probs) + 1]] <- posterior.prob
  
  # M-Step
  
  theta.new <- c()
  
  # update phi
  theta.new <- mean(posterior.prob[,1])
  
  if(max(abs(theta - theta.new)) < epsilon){
    theta <- theta.new
    iterations <- append(iterations, theta)
    break
  }else{
    theta <- theta.new
    iterations <- append(iterations, theta)
  }
}

theta

length(iterations)

length(posterior.probs)

# Initial value
p <- p + geom_point(data = data.frame(x = 0.1, y = l(0.1)), 
                    aes(x = x, y = y), col = "red")
p

# ELBO at initial value
p <- p + stat_function(fun = ELBO, 
                       args = list(x = x, Q = posterior.probs[[1]]), 
                       linewidth = 1.25, col = "red")
p

# First maximization
p <- p + geom_point(data = data.frame(x = iterations[1], 
                                      y = ELBO(iterations[1], x, posterior.probs[[1]])), 
                    aes(x = x, y = y))
p

p <- p + geom_point(data = data.frame(x = iterations[1], 
                                      y = l(iterations[1])), 
                    aes(x = x, y = y))
p

# ELBO at first maximization
p <- p + stat_function(fun = ELBO, 
                       args = list(x = x, Q = posterior.probs[[2]]), 
                       linewidth = 1.25, col = "blue")
p

# Second Maximization
p <- p + geom_point(data = data.frame(x = iterations[2], 
                                      y = ELBO(iterations[2], x, posterior.probs[[2]])), 
                    aes(x = x, y = y))
p

p <- p + geom_point(data = data.frame(x = iterations[2], 
                                      y = l(iterations[2])), 
                    aes(x = x, y = y))
p

# ELBO at second maximization
p <- p + stat_function(fun = ELBO, 
                       args = list(x = x, Q = posterior.probs[[3]]), 
                       linewidth = 1.25, col = "cyan")
p

# Third Maximization
p <- p + geom_point(data = data.frame(x = iterations[3], 
                                      y = ELBO(iterations[3], x, posterior.probs[[3]])), 
                    aes(x = x, y = y))
p

p <- p + geom_point(data = data.frame(x = iterations[3], 
                                      y = l(iterations[3])), 
                    aes(x = x, y = y))
p

# again we construct an ELBO at third maximization and proceed.......