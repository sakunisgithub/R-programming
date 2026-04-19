# To estimate E[X] where X ~ Be_1(2, 3)

# Sample size 100, number of estimates 1000

# Monte Carlo Estimates

rm(list = ls())

set.seed(14)

size = 100

h <- function(u, m, n){
  u1 <- u[1:m]
  u2 <- u[(m + 1) : (m + n)]
      
  x1 <- -sum(log(u1))
  x2 <- -sum(log(u2))
      
  x <- x1 / (x1 + x2)
}

B <- 1000

theta.hat.monte.carlo <- c()

for(i in 1:B){
  u <- matrix(runif(size * (2 + 3)), nrow = size, ncol = (2 + 3))
            
  theta.hat.monte.carlo[i] <- mean(apply(u, MARGIN = 1, FUN = h, 2, 3))
}





# Estimates by Antithetic variable method

theta.hat.antithetic <- c()

for (i in 1:B) {
  u <- matrix(runif((size / 2) * (2 + 3)), nrow = size / 2, ncol = (2 + 3))
  
  x <- apply(u, MARGIN = 1, FUN = h, 2, 3)
  x_dash <- apply(1 - u, MARGIN = 1, FUN = h, 2, 3)
  
  theta.hat.antithetic[i] <- mean(c(x, x_dash))
}


library(tidyverse)

df <- data.frame(monte.carlo = theta.hat.monte.carlo,
                 antithetic = theta.hat.antithetic)

df_melted <- df %>%
  pivot_longer(cols = c("monte.carlo", "antithetic"),
               names_to = "technique",
               values_to = "theta.hat")

df_melted %>%
  ggplot(aes(x = theta.hat, color = technique)) +
  geom_density(linewidth = 1.25)
