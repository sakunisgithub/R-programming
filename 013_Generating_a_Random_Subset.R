# Generating a random subset of 1, 2, ..., 10

n <- 10

P <- 1:n

# Case 1 when r <= n/2

r <- 4

k <- n # length(P) in general

while(k > n - r){
  u <- runif(1)
  
  I <- floor(u * k) + 1
  
  P[c(I, k)] <- P[c(k, I)]
  
  k <- k - 1
}

P[(n-r+1):n]




# Case 2 when r > n/2

r <- 7

r <- n - r

k <- n # length(P) in general

while(k > n - r){
  u <- runif(1)
  
  I <- floor(u * k) + 1
  
  P[c(I, k)] <- P[c(k, I)]
  
  k <- k - 1
}

P[-c((n-r+1):n)]
