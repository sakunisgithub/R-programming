my_data <- read.csv("D:\\data_sets\\cc12_prac_q12_data.csv")
dim(my_data)
names(my_data)
library(tidyverse)
my_data %>%
  ggplot(aes(x = x1, y = avg_y)) +
  geom_point(size = 2, col = "blue") +
  labs(x = "x_1", y = "average y", title = "Scatterplot of x_1 vs y")
my_data %>%
  ggplot(aes(x = x2, y = avg_y)) +
  geom_point(size = 2, col = "blue") +
  labs(x = "x_2", y = "average y", title = "Scatterplot of x_2 vs y")
my_data$avg_y
my_data$x1
my_data$x2
x_1_sq <- (my_data$x1)^2
x_1_sq
x_2_sq <- (my_data$x2)^2
x_2_sq
y_x_1 <- (my_data$avg_y) * (my_data$x1)
y_x_1
y_x_2 <- (my_data$avg_y) * (my_data$x2)
y_x_2
x_1_x_2 <- (my_data$x1) * (my_data$x2)
x_1_x_2
sum(my_data$avg_y)
sum(my_data$x1)
sum(my_data$x2)
sum(x_1_sq)
sum(x_2_sq)
sum(y_x_1)
sum(y_x_2)
sum(x_1_x_2)
mean(my_data$avg_y)
mean(my_data$x1)
mean(my_data$x2)
mean(x_1_sq)
mean(x_2_sq)
mean(y_x_1)
mean(y_x_2)
mean(x_1_x_2)
a <- mean(y_x_1) - (mean(my_data$avg_y) * mean(my_data$x1))
a
b <- mean(y_x_2) - (mean(my_data$avg_y) * mean(my_data$x2))
b
c <- mean(x_1_x_2) - (mean(my_data$x1) * mean(my_data$x2))
c
d <- mean(x_1_sq) - mean(my_data$x1)^2
d
e <- mean(x_2_sq) - mean(my_data$x2)^2
e
beta_1_hat <- (c * b - e * a)/(c^2 - d * e)
beta_1_hat
beta_2_hat <- (c * a - d * b)/(c^2 - d * e)
beta_2_hat
alpha_hat <- mean(my_data$avg_y) - beta_1_hat * mean(my_data$x1) - beta_2_hat * mean(my_data$x2)
alpha_hat
fit1 <- lm(avg_y ~ x1 + x2, data = my_data)
summary(fit1)
model.matrix(fit1)
fit1$rank
df1 <- data.frame(obs = 1:8, res = fit1$residuals)
df1 %>%
  ggplot(aes(x = obs, y = res)) +
  geom_point(size = 2, col = "blue") +
  geom_hline(yintercept = 0, linewidth = 1, col = "red") +
  scale_x_discrete(limits = 1:8)
  labs(x = "obsevation", y = "residual", title = "Residual Plot")