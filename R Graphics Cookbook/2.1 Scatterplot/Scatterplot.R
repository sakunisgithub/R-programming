# using Base R
plot(mtcars$wt, mtcars$mpg)
# with(mtcars, plot(wt, mpg))
# plot(mpg ~ wt, mtcars)

# using ggplot
library(tidyverse)
mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point()

## using ggplot - with vector arguments
ggplot(data = NULL, aes(x = mtcars$wt, y = mtcars$mpg)) +
  geom_point()
