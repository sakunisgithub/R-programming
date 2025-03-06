myunif <- function(initial, n){
  
  x <- c(initial)
  
  for (i in 2:n) {
    x[i] <- (171 * x[i-1]) %% 30269
  }
  
  u <- x / 30269
  
  return(u)
}

myunif(5, 10)
