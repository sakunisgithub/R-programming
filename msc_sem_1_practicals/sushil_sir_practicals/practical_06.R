toss_results <- rbinom(1000, 1, 0.5)

coins_head <- which(toss_results == 1)

n <- c(length(coins_head))

while(length(coins_head) > 1){
  
  toss_results <- rbinom(length(coins_head), 1, 0.5)
  
  coins_head <- coins_head[which(toss_results == 1)]
  
  n <- append(n, length(coins_head))
  
}

coins_head

n; length(n)

plot(NA, NA,
     xlim = c(0, length(n)), ylim = c(0, 1000),
     xlab = "Run", ylab = "Number of coins",
     main = "Animated plot of number of coins vs run")

for (i in 1:length(n)) {
  points(x = i, y = n[i], col = "red", pch = 20)
  Sys.sleep(1)
}
