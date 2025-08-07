acf <- function(h, alpha) alpha^abs(h)

h1 <- -5:5

df1 <- data.frame(h = h1, value = acf(h1, 0.35)); dim(df1)

library(tidyverse)

df1 %>%
  ggplot(aes(x = h, y = value)) +
  geom_hline(yintercept = 0, col = 'red', linewidth = 1) +
  geom_segment(aes(x = h, xend = h, y = 0, yend = value), linewidth = 1, col = 'blue') +
  labs(y = "rho(h)", title = "AR(1) ACF with alpha = 0.35")

# caution ---- this writes in a git repo
ggsave(path = "D:\\Users\\Documents\\LaTeX\\M.Sc.Semester_3\\MSMS-301",
       width = 7,
       height = 7,
       device='png',
       dpi=500,
       filename = "alpha_0.35.png",
       units = "in")

h2 <- -30:30

df2 <- data.frame(h = h2, value = acf(h2, 0.85)); dim(df2)

df2 %>%
  ggplot(aes(x = h, y = value)) +
  geom_hline(yintercept = 0, col = 'red', linewidth = 1) +
  geom_segment(aes(x = h, xend = h, y = 0, yend = value), linewidth = 1, col = 'blue') +
  labs(y = "rho(h)", title = "AR(1) ACF with alpha = 0.85")

# caution ---- this writes in a git repo
ggsave(path = "D:\\Users\\Documents\\LaTeX\\M.Sc.Semester_3\\MSMS-301",
       width = 7,
       height = 7,
       device='png',
       dpi=500,
       filename = "alpha_0.85.png",
       units = "in")

h3 <- -5:5

df3 <- data.frame(h = h3, value = acf(h3, -0.35)); dim(df3)

df3 %>%
  ggplot(aes(x = h, y = value)) +
  geom_hline(yintercept = 0, col = 'red', linewidth = 1) +
  geom_segment(aes(x = h, xend = h, y = 0, yend = value), linewidth = 1, col = 'blue') +
  labs(y = "rho(h)", title = "AR(1) ACF with alpha = -0.35")

# caution ---- this writes in a git repo
ggsave(path = "D:\\Users\\Documents\\LaTeX\\M.Sc.Semester_3\\MSMS-301",
       width = 7,
       height = 7,
       device='png',
       dpi=500,
       filename = "alpha_negative_0.35.png",
       units = "in")
