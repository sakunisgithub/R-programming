set.seed(14)

lambda <- 2

time.units.T <- 5

I <- 0 # number of events that occur by time T

t <- 0

S <- c()

while(t <= time.units.T){
  t <- t + rexp(1, rate = lambda)
  
  if(t > time.units.T){
    break
  }
  else{
    I <- I + 1
    S[I] <- t
  }
}

I

S # event times

plot(1:(I+1) ~ c(S, time.units.T),
     type = "s",
     col = "blue",
     xaxt = "n",
     yaxt = "n",
     xlim = c(0, T + 0.05),
     xlab = "Occurrence Time",
     ylab = "Count",
     main = bquote("Realization of Poisson Process with " ~ lambda == .(lambda)))

axis(1, at = S, labels = round(S, 2), las = 2)
axis(2, at = 1:I, labels = 1:I)
points(S, 1:I, cex = 1, col = "blue", pch = 19)
abline(v = time.units.T, col = "red")
