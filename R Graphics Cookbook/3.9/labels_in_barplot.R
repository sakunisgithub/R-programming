# values in y-axis
BOD
library(tidyverse)
BOD %>%
  ggplot(aes(x = factor(Time), y = demand)) +
  geom_col(fill = "#1CF30A", color = "black", linewidth = 1) +
  labs(x = "Time", y = "Demand", title = "Biochemical Oxygen Demand") +
  geom_text(aes(label = demand), vjust = 2.0, color = "black")
  # geom_text(aes(label = demand), vjust = -2.0, color = "black")

# counts in y-axis
mtcars %>%
  ggplot(aes(x = factor(cyl))) +
  geom_bar(fill = "#A90AF3", color = "black", linewidth = 1) +
  labs(x = "Number of Cylinders", y = "Count", title = "Barplot") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 2.0, color = "black", size = 8)
  # geom_text(aes(label = ..count..), stat = "count", vjust = 2.0, color = "black")
