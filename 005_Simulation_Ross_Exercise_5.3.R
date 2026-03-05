set.seed(22)

n <- 10000

CDF_F <- function(x) (x^2 + x) / 2

PDF_f <- function(x) x + 1/2

F_inverse <- function(x) (sqrt(1 + 8 * x) - 1) / 2

u <- runif(n)

x <- F_inverse(u)

library(tidyverse)

data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15,
                 fill = "#F511F1", color = "black", linewidth = 1) +
  stat_function(fun = PDF_f, linewidth = 1.25) +
  labs(title = "Histogram of Generated Sample")
