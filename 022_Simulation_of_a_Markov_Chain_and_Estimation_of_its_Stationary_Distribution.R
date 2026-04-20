states <- c("Home", "Products", "Cart", "Checkout")

P <- matrix(c(0.5, 0.5, 0.0, 0.0,
              0.2, 0.5, 0.3, 0.0,
              0.0, 0.3, 0.3, 0.4,
              0.2, 0.0, 0.0, 0.8), 
            nrow = 4, ncol = 4, byrow = TRUE)


## obtaining pi by simulation

set.seed(14)

n <- 10^5

trajectory <- c(1) # Initial State : Home

# Simulating the Markov Chain

for (i in 2:n) {
  trajectory[i] <- sample(1:4, 1, prob = P[trajectory[i-1], ])
}

pi.hat <- as.numeric(table(trajectory) / n)
names(pi.hat) <- states
pi.hat




## obtaining pi theoretically

eig.P <- eigen(t(P))
eig.P

eigenvector.of.interest <- eig.P$vectors[, 1]
# eigenvector of interest is the one with eigenvalue 1

pi.true <- Re(eigenvector.of.interest / sum(eigenvector.of.interest))
# scaling to make sure they represent probabilities

names(pi.true) <- states
pi.true
