litter_birth_weight_data <- read.csv("D:\\data_sets\\cc12_prac_q11_data.csv", stringsAsFactors = TRUE)
dim(litter_birth_weight_data)
names(litter_birth_weight_data)
summary(litter_birth_weight_data)
library(tidyverse)
litter_birth_weight_data %>%
  ggplot(aes(x = litters, y = birth_weights)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(fill = "#0A8DF4", linewidth = 0.85) +
  labs(x = "Litters", y = "Birth-weights", title = "Box-plot of Birth-weights of Different Litters")
litter_birth_weight_data %>%
  ggplot(aes(sample = birth_weights)) +
  geom_qq(size = 2, col = "blue") +
  geom_qq_line(linewidth = 1, col = "red") +
  labs(x = "Theoretical quantiles", y = "Sample Quantiles", title = "Q-Q Plot")
shapiro.test(litter_birth_weight_data$birth_weights)
litter_anova <- aov(birth_weights ~ litters, data = litter_birth_weight_data)
summary(litter_anova)
df1 <- litter_birth_weight_data %>%
  group_by(litters) %>%
  summarise(n = length(birth_weights), total = sum(birth_weights), t_sq = total^2, a = round(t_sq / n, digits = 2), sum_sq = sum(birth_weights^2))
View(df1)
sum(df1$n)
sum(df1$total)
sum(df1$a)
sum(df1$sum_sq)
151.9^2/56
419.95-412.0288
438.15-412.0288
26.1212-7.9212
7.9212/7
18.2/48
1.1316/0.3792
