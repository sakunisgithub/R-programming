n = 100000

i = 1

numbers <- c()

while(i <= n){
  numbers[i] = floor(runif(n = 1, min = 1, max = 5))
  
  i = i + 1
}

x <- c(0)
y <- c(0)

i = 2

while (i <= n+1){
  if(numbers[i-1] == 1)
  {
    x[i] <- ( (0.8 * x[i-1]) + 0.1 )
    y[i] <- ( (0.8 * y[i-1]) + 0.04 )
  }
  else if(numbers[i-1] == 2)
  {
    x[i] <- ( (0.5 * x[i-1]) + 0.25 )
    y[i] <- ( (0.5 * y[i-1]) + 0.4 )
  }
  else if(numbers[i-1] == 3)
  {
    x[i] <- ( (0.355 * (x[i-1] - y[i-1])) + 0.266 )
    y[i] <- ( (0.355 * (x[i-1] + y[i-1])) + 0.078 )
  }
  else if(numbers[i-1] == 4)
  {
    x[i] <- ( (0.355 * (x[i-1] + y[i-1])) + 0.378 )
    y[i] <- ( (0.355 * (y[i-1] - x[i-1])) + 0.434 )
  }
  
  i = i + 1
}


par(bg = "black")
plot(NA, NA,
     xlim = c(min(x), max(x)),
     ylim = c(min(y), max(y)))

pause <- function(seconds){
  start <- Sys.time()
  while((Sys.time() - start) < seconds){}
}

stops <- c(c(5, 10), 
           seq(100, 900, by = 100), 
           seq(1000, 9000, by = 1000), 
           seq(10000, 100000, by = 10000))

prev <- 0

for (i in stops) {
  title(xlab = paste("n = ", prev), col.lab = "black")
  points(x = x[1:i], y = y[1:i], col = "green", pch = ".")
  title(xlab = paste("n = ", i), col.lab = "white")
  prev <- i
  pause(0.1)
}
