library(gcookbook)
tg %>%
  ggplot(aes(x = dose, y = length, color = supp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4)