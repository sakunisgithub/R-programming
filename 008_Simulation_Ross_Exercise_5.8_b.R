set.seed(22)

n <- 10^5

x_1 <- rexp(n, rate = 2)

x_2 <- runif(n)

u <- runif(n)

x <- ifelse(u <= 1/3, x_1, x_2)

library(tidyverse)

f <- function(x) (1/3) * dexp(x, rate = 2) + (2/3) * dunif(x)

data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "#27F5E4", color = "black", linewidth = 1) +
  stat_function(fun = f, linewidth = 1.25)
