#library(tidyverse)
BOD
BOD %>%
  ggplot(aes(x = Time, y = demand)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(size = 3)