library(dobson)

data("poisson")

View(poisson)

library(tidyverse)

poisson %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15)) +
  theme_classic()

X <- model.matrix(y ~ x, data = poisson)
X

W <- function(b) diag(1 / c(X %*% b))

b <- matrix(c(7, 5), nrow = 2, byrow = FALSE)

W(b)

z <- matrix(c(poisson$y), ncol = 1, byrow = FALSE)

epsilon <- 0.001

repeat{
  
  Wb <- W(b)
  
  b.new <- solve(t(X) %*% Wb %*% X, t(X) %*% Wb %*% z)
  
  if(max(abs(b - b.new)) < epsilon){
    b <- b.new
    break
  }
  
  b <- b.new
}

b
