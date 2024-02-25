a1 <- c(620, 687, 666, 769, 839, 686)
a2 <- c(501, 585, 524, 585, 655, 668)
a3 <- c(673, 701, 636, 567, 622, 660)
a4 <- c(646, 626, 572, 628, 632, 743)
a5 <- c(494, 984, 659, 643, 660, 640)
a6 <- c(634, 755, 625, 582, 685, 555)
a7 <- c(619, 710, 664, 693, 773, 534)
a8 <- c(631, 723, 614, 535, 551, 570)
a9 <- c(482, 791, 533, 612, 497, 499)
a10 <- c(706, 524, 626, 503, 662, 754)
a11 <- c(530, 432, 379, 690, 724, 536)
a12 <- c(485, 497, 608, 393, 648, 729)
a13 <- c(585, 535, 762, 588, 625, 737)
a14 <- c(462, 490, 635, 587, 554, 673)
a15 <- c(722, 608, 665, 587, 531, 705)

life_time <- matrix(data = c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), nrow = 15, ncol = 6, byrow = TRUE)

rowSums(life_time)

ranges <- c()

for (i in 1:15) {
  ranges[i] <- (max(life_time[i,]) - min(life_time[i,]))
}

ranges

row_means <- round(rowMeans(life_time), digits = 2)

row_means

sum(row_means)

mean(row_means)

sum(ranges)

mean(ranges)

df1 <- data.frame(sample_no = 1:15, row_means = row_means)

library(tidyverse)

df1 %>%
   ggplot(aes(y = row_means, x = sample_no)) +
   geom_point(col = "blue", size = 2) +
   geom_hline(yintercept = 502.8464, linewidth = 1, col = "red") +
   geom_hline(yintercept = 621.278, linewidth = 1, col = "red") +
   geom_hline(yintercept = 739.7096, linewidth = 1, col = "red") +
   scale_x_discrete(limits = 1:15) +
   labs(x = "sample number", y = " Mean life of bulbs", title = "x-bar chart")

df2 <- data.frame(sample_no = 1:15, range = ranges)

df2

df2 %>%
  ggplot(aes(x = sample_no, y = range)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, col = "red") +
  geom_hline(yintercept = 245.2, col = "red") +
  geom_hline(yintercept = 491.3808, col = "red") +
  scale_x_discrete(limits = 1:15) +
  labs(x = "sample_number", y = "Range of Samples", title = "R-chart")
