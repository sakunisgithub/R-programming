p <- 1:365

matched <- c()

for (i in 1:10000) {
  birthdays <- sample(p, size = 60, replace = TRUE)
  matched[i] <- ifelse(length(which(table(birthdays) > 1)) > 0, 1, 0)
}

cum_freq <- cumsum(matched)

relative_frequency <- cum_freq / 1:10000

estimated_prob <- relative_frequency[length(relative_frequency)]

estimated_prob

sizes <- seq(10, 100, 10); sizes

probs <- c()

for (k in 1:length(sizes)) {
  
  matched <- c()
  
  for (i in 1:10000) {
    birthdays <- sample(p, size = sizes[k], replace = TRUE)
    matched[i] <- ifelse(length(which(table(birthdays) > 1)) > 0, 1, 0)
  }
  
  cum_freq <- cumsum(matched)
  
  relative_frequency <- cum_freq / 1:10000
  
  probs[k] <- relative_frequency[length(relative_frequency)]
}

probs

plot(NA, NA,
     xlim = c(0, 100), ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Relative Frequency of matching",
     main = "Animated plot of relative frequency of matching vs sample size")

for (i in 1:length(probs)) {
  points(x = sizes[i], y = probs[i], col = "red", pch = 20)
  Sys.sleep(1)
}
