voltage_data <- read.csv("D:\\data_sets\\sqc_practical_q5_data.csv")
dim(voltage_data)
names(voltage_data)
sample_means <- rowMeans(voltage_data)
sample_means
sample_standard_deviations <- sqrt(rowSums(voltage_data^2)/4 - rowMeans(voltage_data)^2)
sample_standard_deviations <- round(sample_standard_deviations, digits = 2)
sample_standard_deviations
sum(sample_means)
sum(sample_standard_deviations)
x_bar_bar <- mean(sample_means)
x_bar_bar
s_bar <- mean(sample_standard_deviations)
s_bar
lcl_x <- x_bar_bar - 1.88 * s_bar
lcl_x
ucl_x <- x_bar_bar + 1.88 * s_bar
ucl_x
ucl_s <- 2.266 * s_bar
ucl_s
library(tidyverse)
df1 <- data.frame(sample_no = 1:20, sample_means = sample_means, sample_sds = sample_standard_deviations)
df1 %>%
  ggplot(aes(x = sample_no, y = sample_means)) +
  geom_point(size = 2, col = "blue") +
  geom_hline(yintercept = lcl_x, col = "red", linewidth = 1) +
  geom_hline(yintercept = x_bar_bar, col = "red", linewidth = 1) +
  geom_hline(yintercept = ucl_x, col = "red", linewidth = 1) +
  scale_x_discrete(limits = 1:20) +
  labs(x = "Sample Number", y = "Sample Mean", title = "x-bar chart")
df1 %>%
  ggplot(aes(x = sample_no, y = sample_sds)) +
  geom_point(size = 2, col = "blue") +
  geom_hline(yintercept = 0, col = "red", linewidth = 1) +
  geom_hline(yintercept = s_bar, col = "red", linewidth = 1) +
  geom_hline(yintercept = ucl_s, col = "red", linewidth = 1) +
  scale_x_discrete(limits = 1:20) +
  labs(x = "Sample Number", y = "Sample SDs", title = "s chart")
