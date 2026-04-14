df <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/refs/heads/master/msc_semester_4/Beetle_Mortality_Data.csv')

View(df)

library(tidyverse)

df %>%
  ggplot(aes(x = dose, y = number.killed/number.of.beetles)) +
  geom_point(size = 2) +
  labs(x = "Dose", y = "Proportion killed") +
  theme_classic()

X <- model.matrix(number.killed ~ dose, data = df)
X

pi.i_hat <- function(b) exp(X %*% b) / (1 + exp(X %*% b)) # a vector containing all pi.i hats

ni <- df$number.of.beetles # a vector containing all ni's
ni

yi <- df$number.killed # a vector containing all yi's
yi

xi <- df$dose # a vector containing all xi's
xi

b <- matrix(c(0, 0), ncol = 1)
b

J <- function(b){
  matrix(c(sum(ni * pi.i_hat(b) * (1 - pi.i_hat(b))),
           sum(ni * xi * pi.i_hat(b) * (1 - pi.i_hat(b))),
           sum(ni * xi * pi.i_hat(b) * (1 - pi.i_hat(b))),
           sum(ni * xi^2 * pi.i_hat(b) * (1 - pi.i_hat(b)))), 
         nrow = 2, ncol = 2, byrow = FALSE)
}

U <- function(b){
  matrix(c(sum(yi - ni * pi.i_hat(b)),
           sum(xi * (yi - ni * pi.i_hat(b)))),
         ncol = 1, byrow = FALSE)
}

log.likelihood <- function(b, ignore.constant = TRUE){
  
  if(ignore.constant){
    sum(yi * (X %*% b) - ni * log(1 + exp(X %*% b)))
  }else{
    sum(yi * (X %*% b) - ni * log(1 + exp(X %*% b)) + log(choose(ni, yi)))
  }
}

log.likelihood.values <- c(log.likelihood(b))
log.likelihood.values

all.fitted.values <- matrix(c(ni * pi.i_hat(b)), ncol = 1, byrow = FALSE)
all.fitted.values

epsilon <- 0.001

repeat{
  b.new <- b + solve(J(b)) %*% U(b)
  
  log.likelihood.values <- append(log.likelihood.values, log.likelihood(b.new))
  
  all.fits <- ni * pi.i_hat(b.new)
  all.fitted.values <- cbind(all.fitted.values, all.fits)
  
  if(max(abs(b - b.new)) < epsilon){
    b <- b.new
    break
  }
  
  b <- b.new
}

b

log.likelihood.values

all.fitted.values
