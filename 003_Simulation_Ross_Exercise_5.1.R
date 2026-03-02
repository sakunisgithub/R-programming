set.seed(22)

n <- 10000

library(tidyverse)

f <- function(x) exp(x) / (exp(1) - 1)

p <- data.frame(x = c(0, 1)) %>%
      ggplot() +
      stat_function(aes(x = x), fun = f, linewidth = 1.25) +
      theme_classic()

p

g <- function(x) 1

p <- p +
      stat_function(fun = g, linewidth = 1.25, col = "blue")

p

c <- exp(1) / (exp(1) - 1)

g1 <- function(x) c * g(x)

p <- p +
      stat_function(fun = g1, linewidth = 1.25, col = "red")

p

X <- runif(n)

Y <- runif(n, min = 0, max = g1(X))

p <- p +
      geom_point(data = data.frame(X, Y), aes(x = X, y = Y))

p

good <- Y <= f(X) # or Y / g1(X) <= f(X) / g1(X)

accepted <- X[good]

p <- p +
      geom_point(data = data.frame(x = X[good], y = Y[good]), aes(x = x, y = y), col = "green")

p

data.frame(accepted) %>%
  ggplot(aes(x = accepted)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10, color = "black", fill = "#0FD8F0") +
  stat_function(fun = f, linewidth = 1.25, col = "black") +
  labs(title = "Histogram of Accepted Points")

1 / c
