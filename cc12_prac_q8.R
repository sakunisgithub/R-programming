yield_data <- read.csv("D:\\data_sets\\cc12_prac_q8_data.csv")
dim(yield_data)
names(yield_data)
yield_data$yield_of_dry_bark
yield_data$height
yield_data$girth
library(tidyverse)
yield_data %>%
  ggplot(aes(x = height, y = yield_of_dry_bark)) +
  geom_point(size = 2, col = "blue") +
  labs(x = "Height of Plant", y = "Yield of dry bark", title = "Scatterplot of Height vs Yield")
yield_data %>%
  ggplot(aes(x = girth, y = yield_of_dry_bark)) +
  geom_point(size = 2, col = "#065AF9") +
  labs(x = "Girth of Plant at height 6'' from ground", y = "Yield of dry bark", title = "Scatterplot of Girth vs Yield")
x_1_i_sq <- (yield_data$height)^2
x_1_i_sq
x_2_i_sq <- (yield_data$girth)^2
x_2_i_sq
y_i_x_1_i <- (yield_data$yield_of_dry_bark) * (yield_data$height)
y_i_x_1_i
y_i_x_2_i <- (yield_data$yield_of_dry_bark) * (yield_data$girth)
y_i_x_2_i
x_1_i_x_2_i <- (yield_data$height) * (yield_data$girth)
x_1_i_x_2_i
y_total <- sum(yield_data$yield_of_dry_bark)
y_total
x_1_total <- sum(yield_data$height)
x_1_total
x_2_total <- sum(yield_data$girth)
x_2_total
x_1_sq_total <- sum(x_1_i_sq)
x_1_sq_total
x_2_sq_total <- sum(x_2_i_sq)
x_2_sq_total
y_x_1_total <- sum(y_i_x_1_i)
y_x_1_total
y_x_2_total <- sum(y_i_x_2_i)
y_x_2_total
x_1_x_2_total <- sum(x_1_i_x_2_i)
x_1_x_2_total
y_bar <- round(y_total / 18, digits = 2)
y_bar
x_1_bar <- round(x_1_total / 18, digits = 2)
x_1_bar
x_2_bar <- round(x_2_total / 18, digits = 2)
x_2_bar
x_1_sq_total / 18
x_2_sq_total / 18
y_x_1_total / 18
y_x_2_total / 18
x_1_x_2_total / 18
s_x_1_sq <- round(x_1_sq_total / 18 - x_1_bar^2, digits = 2)
s_x_1_sq
s_x_2_sq <- round(x_2_sq_total / 18 - x_2_bar^2, digits = 2)
s_x_2_sq
cov_y_x_1 <- round(y_x_1_total / 18 - y_bar * x_1_bar, digits = 2)
cov_y_x_1
cov_y_x_2 <- round(y_x_2_total / 18 - y_bar * x_2_bar, digits = 2)
cov_y_x_2
cov_x_1_x_2 <- round(x_1_x_2_total / 18 - x_1_bar * x_2_bar, digits = 2)
cov_x_1_x_2
beta_1_hat <- (cov_x_1_x_2 * cov_y_x_2 - s_x_2_sq * cov_y_x_1)/(cov_x_1_x_2^2 - s_x_1_sq * s_x_2_sq)
beta_1_hat
beta_2_hat <- (cov_x_1_x_2 * cov_y_x_1 - s_x_1_sq * cov_y_x_2)/(cov_x_1_x_2^2 - s_x_1_sq * s_x_2_sq)
beta_2_hat
alpha_hat <- y_bar - beta_1_hat * x_1_bar - beta_2_hat * x_2_bar
alpha_hat
fit1 <- lm(yield_of_dry_bark ~ height + girth, data = yield_data)
summary(fit1)
model.matrix(fit1)
fit1$rank
df1 <- data.frame(plant_no = 1:18, residuals = fit1$residuals)
df1 %>%
  ggplot(aes(x = plant_no, y = residuals)) +
  geom_point(size = 2, color = "blue") +
  geom_hline(yintercept = 0, col = "red", linewidth = 1.2) +
  labs(x = "Plant Number", y = "Residual", title = "Residual Plot")