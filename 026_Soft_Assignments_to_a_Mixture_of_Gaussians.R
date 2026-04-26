# Suppose data come from a mixture of two Gaussians
# N(0, 1) with probability 0.6
# N(3, 1) with probability 0.4
# 
# We have an observation x = 1.
# 
# To which Gaussian does x belong to ?

# Joint Probabilities
dnorm(1, mean = 0, sd = 1) * 0.6
dnorm(1, mean = 3, sd = 1) * 0.4

# Marginal Probability
m <- dnorm(1, mean = 0, sd = 1) * 0.6 + dnorm(1, mean = 3, sd = 1) * 0.4; m

# Posterior Probabilities of z
Q1 <- (dnorm(1, mean = 0, sd = 1) * 0.6) / m; Q1
Q2 <- (dnorm(1, mean = 3, sd = 1) * 0.4) / m; Q2
