set.seed(22)

n <- 10000

f <- function(x){
  y <- numeric(length = length(x))
  
  index1 <- x >= 2 & x <= 3
  index2 <- x > 3 & x <= 6
  
  y[index1] <- (x[index1] - 2) / 2
  y[index2] <- (2 - x[index2]/3) / 2
  
  return(y)
}

library(tidyverse)

p <- data.frame() %>%
      ggplot() +
      stat_function(fun = f, xlim = c(0, 7), linewidth = 1.25) +
      theme_classic()

p

g <- function(x) dunif(x, min = 2, max = 6)

p <- p +
      stat_function(fun = g, xlim = c(0, 7), linewidth = 1.25, col = "red")

p

c <- 2

g1 <- function(x) g(x) * c

p <- p +
      stat_function(fun = g1, xlim = c(0, 7), linewidth = 1.25, col = "blue")

p

X <- runif(n, min = 2, max = 6)
Y <- runif(n, min = 0, max = g1(X))

p <- p +
      geom_point(data = data.frame(X, Y),
                 aes(x = X, y = Y))

p

good <- Y <= f(X)
accepted <- X[good]

p <- p +
      geom_point(data = data.frame(x = X[good], y = Y[good]),
                 aes(x = x, y = y),
                 col = "green")

p

data.frame(accepted) %>%
  ggplot(aes(x = accepted)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20,
                 color = "black",
                 fill = "#0FD8F0") +
  stat_function(fun = f, xlim = c(2, 6), linewidth = 1.25)

1 / c
