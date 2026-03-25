# Generating a random permutation of 1, 2, ..., 10

n <- 10

P <- 1:n

k <- n # length(P) in general

while(k > 1){
  u <- runif(1)
  
  I <- floor(u * k) + 1
  
  P[c(I, k)] <- P[c(k, I)]
  
  k <- k - 1
}

P