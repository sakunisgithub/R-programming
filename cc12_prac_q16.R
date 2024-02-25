swine_weigtht_gain_data <- read.csv("D:\\data_sets\\cc12_prac_q16_data.csv", stringsAsFactors = TRUE)
dim(swine_weigtht_gain_data)
names(swine_weigtht_gain_data)
View(swine_weigtht_gain_data)
summary(swine_weigtht_gain_data)
library(tidyverse)
swine_weigtht_gain_data %>%
  ggplot(aes(x = ration, y = gain_in_weight)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(linewidth = 1, fill = "#FA6E01") +
  labs(x = "Ration", y = "Gain in Weight", title = "Box-plot of Gain in weight vs Ration")
swine_weigtht_gain_data %>%
  ggplot(aes(x = lot, y = gain_in_weight)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(linewidth = 1, fill = "#E401FA") +
  labs(x = "Lot", y = "Gain in Weight", title = "Box-plot of Gain in Weight vs Lot")
swine_weigtht_gain_data %>%
  ggplot(aes(sample = gain_in_weight)) +
  geom_qq(size = 2, col = "blue") +
  geom_qq_line(linewidth = 1, col = "red") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Q-Q Plot")
swine_weight_anova <- aov(gain_in_weight ~ ration + lot, data = swine_weigtht_gain_data)
summary(swine_weight_anova)
df1 <- swine_weigtht_gain_data %>%
  group_by(ration) %>%
  summarise(total = sum(gain_in_weight), total_sq = total^2)
View(df1)
df2 <- swine_weigtht_gain_data %>%
  group_by(lot) %>%
  summarise(total = sum(gain_in_weight), total_sq = total^2)
View(df2)
sum(swine_weigtht_gain_data$gain_in_weight)
56.5^2/40
sum(df1$total_sq)
321.23/4 - 79.81
sum(df2$total_sq)
811.95/10 - 79.81
sum(swine_weigtht_gain_data$gain_in_weight^2) - 79.81
2.46-0.4975-1.385
0.4975/9
1.385/3
0.5775/27
0.0553/0.0214
