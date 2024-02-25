?ToothGrowth
names(ToothGrowth)
# using Base R
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)

# using ggplot
library(tidyverse)
ToothGrowth %>%
  ggplot(aes(x = supp, y = len)) +
  geom_boxplot(fill = "#76F41A", col = "black", linewidth = 1) +
  labs(x = "Supplement Type", y = "Tooth Length")

ToothGrowth %>%
  ggplot(aes(x = supp, y = len)) +
  stat_boxplot(geom = "errorbar", linewidth = 1) +
  geom_boxplot(fill = "#76F41A", col = "black", linewidth = 1) +
  labs(x = "Supplement Type", y = "Tooth Length")

ToothGrowth %>%
  ggplot(aes(x = interaction(supp, dose), y = len)) +
  geom_boxplot(fill = "#1A27F4", col = "black", linewidth = 1) +
  labs(x = "Supplement X Dose", y = "Tooth Length")
