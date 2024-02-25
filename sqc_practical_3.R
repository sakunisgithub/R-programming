fuse_data <- read.csv("D:\\data_sets\\sqc_practical_q3_data.csv")
dim(fuse_data)
names(fuse_data)
sample_totals <- rowSums(fuse_data)
sample_totals
sample_means <- rowMeans(fuse_data)
sample_means
sample_ranges <- c()
for (i in 1:12) {
  sample_ranges[i] <- max(fuse_data[i,]) - min(fuse_data[i,])
}
sample_ranges
x_bar_bar <- mean(sample_means)
x_bar_bar
r_bar <- mean(sample_ranges)
r_bar <- round(r_bar, digits = 2)
r_bar
sum(sample_means)
sum(sample_ranges)
lcl_x <- x_bar_bar - 0.577 * r_bar
lcl_x
ucl_x <- x_bar_bar + 0.577 * r_bar
ucl_x
ucl_r <- 2.114 * r_bar
ucl_r
library(tidyverse)
df1 <- data.frame(sample_no = 1:12, sample_means = sample_means)
df1 %>%
  ggplot(aes(x = sample_no, y = sample_means)) +
  geom_point(color = "blue", size = 1.5) +
  geom_hline(yintercept = lcl_x, linewidth = 1.5, col = "red") +
  geom_hline(yintercept = x_bar_bar, linewidth = 1.5, col = "red") +
  geom_hline(yintercept = ucl_x, linewidth = 1.5, col = "red")+
  scale_x_discrete(limits = 1:12) +
  labs(x = "Sample Number", y = "Mean of Samples", title = "x-bar chart")
df2 <- data.frame(sample_no = 1:12, sample_ranges = sample_ranges)
df2 %>%
  ggplot(aes(x = sample_no, y = sample_ranges)) +
  geom_point(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linewidth = 1.5, col = "red") +
  geom_hline(yintercept = r_bar, linewidth = 1.5, col = "red") +
  geom_hline(yintercept = ucl_r, linewidth = 1.5, col = "red") +
  scale_x_discrete(limits = 1:12) +
  labs(x = "Sample Number", y = "Range of Samples", title = "R-chart")