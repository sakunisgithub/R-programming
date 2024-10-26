toss_results <- rbinom(1000, 1, 0.5)

head_cum_freq <- cumsum(toss_results)

head_cum_relative_freq <- head_cum_freq / 1:1000

epsilon <- 0.001

i <- 1; n <- 0

while(i < 1000){
  if(abs(head_cum_relative_freq[i+1] - head_cum_relative_freq[i]) < epsilon){
    if(i >= 30){
      n <- i
      break
    }
  }
  i <- i + 1
}

n

df <- data.frame(runs = 1:n, rel_freq = head_cum_relative_freq[1:n])

plot(NA, NA,
     xlim = c(0, n), ylim = c(0, 1),
     xlab = "Runs", ylab = "Relative Frequency",
     main = "Animated plot for simulated coin toss")

for (i in 1:n) {
  points(x = i, y = df[i,2], col =  "red", pch = 20)
  abline(h = 0.5, col = "blue")
  Sys.sleep(1)
}
