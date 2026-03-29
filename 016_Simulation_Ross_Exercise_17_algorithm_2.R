set.seed(14)

n <- 10^5

z <- sample(1:3, size = n, replace = TRUE, prob = c(1/4, 2/4, 1/4))

x <- numeric(n)

x[z == 1] <- rbeta(sum(z == 1), shape1 = 1, shape2 = 1)
x[z == 2] <- rbeta(sum(z == 2), shape1 = 4, shape2 = 1)
x[z == 3] <- rbeta(sum(z == 3), shape1 = 5, shape2 = 1)

f <- function(x) 1/4 + 2 * x^3 + (5/4) * x^4

library(tidyverse)

data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "#7627F5", color = "black", linewidth = 1) +
  stat_function(fun = f, linewidth = 1.25) +
  labs(title = "Histogram of Generated Sample")
