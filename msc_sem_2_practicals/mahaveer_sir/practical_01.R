df <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/refs/heads/master/student_screen_watch_time.csv')
dim(df)

times <- df$exact_minutes[!is.na(df$exact_minutes)]
times
length(times)

prob <- function(size, epsilon, population){
  
  sample_list <- list()
  
  for(i in 1:1000){
    sample_list[[i]] <- sample(population, size, replace = FALSE)
  }
  
  sample_means <- sapply(sample_list, mean)
  
  m <- length(which(abs(sample_means - mean(population)) < epsilon))
  
  return(m/1000)
}

probs <- c()

for (i in 1:33) {
  probs[i] <- prob(size = i, epsilon = 10, population = times)
}

probs

df2 <- data.frame(size = 1:33, probabilites = probs)

library(tidyverse)

df2 %>%
  ggplot(aes(x = size, y = probabilites)) +
  geom_point(size = 2, col = "red") +
  geom_hline(yintercept = 1, col = "blue", linewidth = 1) +
  labs(x = "Sample Size", y = "Probability",
       title = "Convergence in Probability of Sample Mean to Population Mean")
