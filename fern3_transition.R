iterations <- 100000

x <- numeric(iterations)
y <- numeric(iterations)

x[1] <- 0
y[1] <- 0

tau <- seq(-0.1, 0.1, by = 0.01)

for(k in tau){
  for (i in 2:iterations) {
    r <- runif(1)
    
    if (r <= 0.01) {
      x[i] <- 0
      y[i] <- 0.16 * y[i-1]
    } else if (r <= 0.86) {
      x[i] <- 0.85 * x[i-1] + k * y[i-1]
      y[i] <- -0.04 * x[i-1] + 0.85 * y[i-1] + 1.6
    } else if (r <= 0.93) {
      x[i] <- 0.2 * x[i-1] - 0.26 * y[i-1]
      y[i] <- 0.23 * x[i-1] + 0.22 * y[i-1] + 1.6
    } else {
      x[i] <- -0.15 * x[i-1] + 0.28 * y[i-1]
      y[i] <- 0.26 * x[i-1] + 0.24 * y[i-1] + 0.44
    }
  }
  
  
  par(bg = "black")
  plot(x, y, pch= ".", col = "green", axes = FALSE)
}
