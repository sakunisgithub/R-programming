n <- 1000

x <- c()

lambda <- 2

for(i in 1:n){
  u <- runif(1)
  
  j <- 0; p <- exp(-lambda)
  
  CDF_F <- p
  
  while(u > CDF_F){
    p <- (lambda * p) / (j + 1)
    
    CDF_F <- CDF_F + p
    
    j <- j + 1
  }
  
  x[i] <- j
}

head(x)

mean(x)

var(x)

barplot(table(x))
