chicks_data <- read.csv("D:\\data_sets\\doe_prac_q1_data.csv", stringsAsFactors = TRUE)
View(chicks_data)
dim(chicks_data)
library(tidyverse)
chicks_data %>%
  ggplot(aes(x = feed, y = gain_in_weight)) +
  stat_boxplot("geom" = "errorbar", linewidth = 1) +
  geom_boxplot(fill = "#DD0FF1") +
  labs(x = "Feed", y = "Gain in Weight", title = "Boxplot - Gain in Weight vs Feed")
chicks_data %>%
  ggplot(aes(sample = gain_in_weight)) +
  geom_qq(size = 2, col = "blue") +
  geom_qq_line(linewidth = 1, col = "red") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")
homo_test <- bartlett.test(gain_in_weight ~ feed, data = chicks_data)
homo_test
chicks_data_aov <- aov(gain_in_weight ~ feed, data = chicks_data)
summary(chicks_data_aov)
TukeyHSD(chicks_data_aov, ordered = TRUE)

df1 <- chicks_data %>%
  group_by(feed) %>%
  summarise(row_totals = sum(gain_in_weight), row_means = mean(gain_in_weight), row_total_sq = row_totals^2, row_total_sq_by_ni = row_total_sq / 5)
View(df1)
G <- sum(chicks_data$gain_in_weight)
G
CF <- G^2 / length(chicks_data$gain_in_weight)
CF
RSS <- sum(chicks_data$gain_in_weight^2)
RSS
TSS <- RSS - CF
TSS
SS_due_to_feed <- sum(df1$row_total_sq_by_ni) - CF
SS_due_to_feed
SSE <- TSS - SS_due_to_feed
SSE
MS_due_to_feed <- SS_due_to_feed / 3
MS_due_to_feed
MSE <- SSE / 16
MSE
VR <- MS_due_to_feed / MSE
VR
VR > qf(0.05, 3, 16, lower.tail = FALSE)
critical_difference <- qt(0.025, 16, lower.tail = FALSE) * sqrt((2 * MSE) / 5)
critical_difference
# a <- outer(df1$row_means, df1$row_means, FUN = "-")
# b <- unique(c(abs(a)))
# b
# c <- which(b > critical_difference)       
# b[c]
mean_differences <- TukeyHSD(chicks_data_aov)$feed[,1]
mean_differences
imp <- which(mean_differences > critical_difference)
imp
mean_differences[imp]
