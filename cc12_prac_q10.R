marks_data <- read.csv("D:\\data_sets\\cc12_prac_q10_data.csv")
dim(marks_data)
names(marks_data)
marks_data$y
marks_data$x
x_sq <- (marks_data$x)^2
x_sq
x_y <- marks_data$y * marks_data$x
x_y
y_total <- sum(marks_data$y)
y_total
x_total <- sum(marks_data$x)
x_total
x_sq_total <- sum(x_sq)
x_sq_total
x_y_total <- sum(x_y)
x_y_total
mean(marks_data$y)
mean(marks_data$x)
s_xy <- x_y_total - 18 * 58.06 * 61.39
s_xy
s_xx <- x_sq_total - 18 * (58.06)^2
s_xx
beta_hat <- s_xy / s_xx
beta_hat
alpha_hat <- 61.39 - beta_hat * 58.06
alpha_hat
fit1 <- lm(y ~ x, data = marks_data)
summary(fit1)
mse <- sum((fit1$residuals)^2) / 16
mse
sqrt(mse)
res <- round(fit1$residuals, digits = 2)
res
fitted_values <- round(fit1$fitted.values, digits = 2)
fitted_values
res_sq <- round(res^2, digits = 3)
res_sq
sum(res_sq)
sum(res_sq) / 16
sqrt(sum(res_sq) / 16)
(sqrt(s_xx)*0.873)/sqrt(191.9215)
library(tidyverse)
marks_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, col = "blue") +
  labs(x = "homework marks", y = "midterm marks", title = "Scatterplot of homework marks and midterm marks")
model.matrix(fit1)
fit1$rank
df1 <- data.frame(students = 1:18, residuals = fit1$residuals)
df1 %>%
  ggplot(aes(x = students, y = residuals)) +
  geom_point(size = 2, col = "#0595F9") +
  geom_hline(yintercept = 0, col = "red", linewidth = 1) +
  scale_x_discrete(limits = 1:18) +
  labs(x = "student no", y = "residual", title = "Residual plot")
