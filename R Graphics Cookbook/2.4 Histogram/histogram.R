# using Base R
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)

# using ggplot
library(tidyverse)
mtcars %>%
  ggplot(aes(x = mpg)) +
  geom_histogram(binwidth = 5, fill = "#F81009", color = "black", linewidth = 1)
