df <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/refs/heads/master/student_screen_watch_time.csv')
dim(df)

times <- df$exact_minutes[!is.na(df$exact_minutes)]
times
length(times)

pop_mean <- mean(times)

means <- c()

for (i in 1:30) {
  a_sample <- sample(times, i, replace = FALSE)
  
  means[i] <- mean(a_sample)
}

df1 <- data.frame(size = 1:30, sample_means = means)

library(tidyverse)

df1 %>%
  ggplot(aes(x = size, y = sample_means)) +
  geom_point(size = 2.5, col = "red") +
  geom_line(col = "blue", linewidth = 1) +
  geom_hline(yintercept = pop_mean, linewidth = 1, col = "magenta") +
  labs(x = "Sample Size", y = "Sample Mean",
       title = "Convergence of Sample Mean to Population Mean")

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

df2 %>%
  ggplot(aes(x = size, y = probabilites)) +
  geom_point(size = 2, col = "red") +
  geom_hline(yintercept = 1, col = "blue", linewidth = 1) +
  labs(x = "Sample Size", y = "Probability",
       title = "Convergence in Probability of Sample Mean to Population Mean")
