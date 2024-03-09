raw <- sample(6, 100, replace = TRUE, prob = c(1, 2, 3, 2, 2, 3))
raw
tabulate(raw, 6)