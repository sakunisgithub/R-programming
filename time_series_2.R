df1 <- data.frame(h = -4:4, acf = c(0, 0, 1/6, 2/3, 1, 2/3, 1/6, 0, 0))

library(tidyverse)

df1 %>%
  ggplot(aes(x = h, y = acf)) +
  geom_hline(yintercept = 0, col = 'red', linewidth = 1) +
  geom_segment(aes(x = h, xend = h, y = 0, yend = acf), linewidth = 1, col = 'blue') +
  geom_point(shape = 4, stroke = 2) +
  scale_x_continuous(breaks = -4:4) +
  labs(y = "ACF", title = "ACF as a Function of Lag h")

# caution ---- this writes in a git repo
ggsave(path = "D:\\Users\\Documents\\LaTeX\\M.Sc.Semester_3\\MSMS-301",
       width = 9,
       height = 9,
       device='png',
       dpi=500,
       filename = "ACF_Plot.png",
       units = "in")
