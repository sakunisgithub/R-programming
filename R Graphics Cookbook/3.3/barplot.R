library(tidyverse)
diamonds %>%
  ggplot(aes(x = cut)) +
  geom_bar(fill = "#A415EC", color = "black", linewidth = 1)