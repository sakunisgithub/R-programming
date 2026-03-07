set.seed(22)

n <- 10^5

z <- sample(1:3, size = n, replace = TRUE, prob = rep(1/3, 3))

x <- numeric(n)

x[z == 1] <- runif(sum(z == 1))
x[z == 2] <- runif(sum(z == 2))^(1/3)
x[z == 3] <- runif(sum(z == 3))^(1/5)

f <- function(x) (1/3) * dunif(x) + (1/3) * (3 * x^2) + (1/3) * (5 * x^4)

library(tidyverse)

data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10,
                 fill = "#7627F5", color = "black", linewidth = 1) +
  stat_function(fun = f, linewidth = 1.25)
