random_walk <- function(n, p){

  x <- seq(from = -n, to = n, by = 2)
  
  probs <- c()
  
  for (i in 1:length(x)) {
    a <- choose(n, i-1)
    b <- p^(i-1) * (1-p)^(n-(i-1))
    
    probs[i] <- a * b
  }
  
  plot(probs ~ x, 
       type = "o", 
       lwd = 2, 
       col = "red",
       ylab = "probabilities",
       ylim = c(0, 1), 
       main = paste("PMF of S_n at n = ", n))
}

pause <- function(t){
  start <- Sys.time()
  while((Sys.time() - start) < t){}
}

for (i in 1:50) {
  random_walk(i, 0.5)
  pause(0.1)
}

trajectory <- function(n){
  
  t <- 1:n
  
  realizations <- sample(c(1, -1), 
                         size = length(t), 
                         replace = TRUE, 
                         prob = c(0.5, 0.5))
  
  plot(c(0, cumsum(realizations)) ~ c(0, t), type = "o", col = "red", lwd = "2")
  
  df <- data.frame(n = c(0, t),
                   realizations = c(0, cumsum(realizations)))
  
  library(tidyverse)
  
  df %>%
    ggplot(aes(x = n, y = realizations)) +
    geom_line(linewidth = 1, col = "blue") +
    geom_point(col = "red", size = 2) +
    geom_hline(yintercept = 0, col = "red", linewidth = 1) +
    labs(x = "n", y = "Position", title = "Trajectory of Random Walk")
    
}

trajectory(50)
