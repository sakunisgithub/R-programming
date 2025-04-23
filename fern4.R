n = 100000

x <- c(0)
y <- c(0)

i = 2

while (i <= n) {
  num <- runif(1)
  
  if(num <= 0.02){
    x[i] <- 0
    y[i] <- 0.25 * y[i-1]
  }else if(num <= 0.86){
    x[i] <- 0.95 * x[i-1] + 0.005 * y[i-1] - 0.002
    y[i] <- -0.005 * x[i-1] + 0.93 * y[i-1] + 0.5
  }else if(num <= 0.93){
    x[i] <- 0.035 * x[i-1] - 0.2 * y[i-1] - 0.09
    y[i] <- 0.16 * x[i-1] + 0.04 * y[i-1] + 0.02
  }else{
    x[i] <-  -0.04 * x[i-1]  + 0.2 * y[i-1] + 0.083
    y[i] <- 0.16 * x[i-1] + 0.04 * y[i-1] + 0.12
  }
  
  i = i + 1
}

par(bg = "black")
plot(x, y,
     col = "green", 
     pch = ".",
     axes = FALSE)
