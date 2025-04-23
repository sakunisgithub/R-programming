n = 100000

x <- c(0)
y <- c(0)

i = 2

while (i <= n) {
  num <- runif(1)
  
  if(num <= 0.01){
    x[i] <- 0
    y[i] <- 0.16 * y[i-1]
  }else if(num <= 0.86){
    x[i] <- 0.85 * x[i-1] - 0.04 * y[i-1]
    # x[i] <- 0.85 * x[i-1] + 0.04 * y[i-1]
    y[i] <- -0.04 * x[i-1] + 0.85 * y[i-1] + 1.6
  }else if(num <= 0.93){
    x[i] <- 0.2 * x[i-1] - 0.26 * y[i-1]
    y[i] <- 0.23 * x[i-1] + 0.22 * y[i-1] + 1.6
  }else{
    x[i] <-  -0.15 * x[i-1]  + 0.28 * y[i-1]
    y[i] <- 0.26 * x[i-1] + 0.24 * y[i-1] + 0.44
  }
  
  i = i + 1
}

par(bg = "black")
plot(x, y,
     col = "green", 
     pch = ".",
     axes = FALSE)
