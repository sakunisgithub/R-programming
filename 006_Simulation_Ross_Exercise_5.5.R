set.seed(22)

n <- 10^5

y <- rexp(n, rate = 2)

u <- runif(n)

x <- ifelse(u <= 1/2, y, -y)

f <- function(x) exp(- 2 * abs(x))

library(tidyverse)

data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "#F5E427", color = "black", linewidth = 1) +
  stat_function(fun = f, xlim = c(-3, 3), linewidth = 1.25)
