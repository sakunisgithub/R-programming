# Question 2
View(mtcars)

library(tidyverse)

mtcars$gear

mtcars$vs <- as.factor(mtcars$vs)

mtcars %>%
  ggplot(aes(x = as.factor(gear), fill = vs)) +
  geom_bar(position = "stack", col = "black", linewidth = 1) +
  labs(x = "Gear", y = "Count", title = "Stacked Barplot of Gear and vs")

mtcars %>%
  ggplot(aes(x = as.factor(gear), fill = vs)) +
  geom_bar(position = "dodge", col = "black", linewidth = 1) +
  labs(x = "Gear", y = "Count", title = "Grouped Barplot of Gear grouped by vs")
