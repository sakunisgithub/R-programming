theta <- 2

epsilon <- 1

x_order_n <- c()

prob <- function(size, epsilon){
  
  sample_list <- list()
  
  for(i in 1:1000){
    sample_list[[i]] <- runif(size, 0, theta)
  }
  
  x_order_n <- sapply(sample_list, max)
  
  m <- length(which(abs(x_order_n - theta) < epsilon))
  
  return(m/1000)
}

probs <- c()

for (i in 1:30) {
  probs[i] <- prob(size = i, epsilon = 1)
}

probs

library(tidyverse)

df1 <- data.frame(sample_size = 1:30, probability = probs)

df1 %>%
  ggplot(aes(x = sample_size, y = probability)) +
  geom_point(size = 2, col = "red") +
  labs(x = "Sample Size", y = "Probability")

# part 2
for(s in 1:10){
  
  x_order_n <- c()
  
  for (i in 1:1000) {
    x_order_n[i] <- max(runif(s, 0, theta))
  }
  
  values <- s*(x_order_n - theta)
  
  hist(values, probability = TRUE, breaks = 20)
  curve(dexp(-x, rate = 1/theta), add = TRUE, lwd = 2)
  
  Sys.sleep(1)
}
