defective_assembiles_data <- read.csv("D:\\data_sets\\sqc_practical_q7_data.csv")
dim(defective_assembiles_data)
names(defective_assembiles_data)
fraction_defectives <- defective_assembiles_data$no_of_defectives / 100
fraction_defectives
sum(defective_assembiles_data$no_of_defectives)
p_bar <- sum(defective_assembiles_data$no_of_defectives) / (20*100)
p_bar
lcl_p <- p_bar - (3 /sqrt(100)) * sqrt(p_bar * (1 - p_bar))
lcl_p
ucl_p <- p_bar + (3 /sqrt(100)) * sqrt(p_bar * (1 - p_bar))
ucl_p
library(tidyverse)
df1 <- data.frame(sample_no = 1:20, frac_defec = fraction_defectives)
df1 %>%
  ggplot(aes(x = sample_no, y = frac_defec)) +
  geom_point(size = 2, col = "red") +
  geom_hline(yintercept = 0, linewidth = 1, col = "blue") +
  geom_hline(yintercept = p_bar, linewidth = 1, col = "blue") +
  geom_hline(yintercept = ucl_p, linewidth = 1, col = "blue") +
  scale_x_discrete(limits = 1:20) +
  labs(x = "Sample Number", y = "Fraction defectives", title = "p-chart")
which(fraction_defectives > ucl_p)
p_bar_new <- (sum(defective_assembiles_data$no_of_defectives) - defective_assembiles_data[12,]) / (19*100)
p_bar_new
lcl_p_new <- p_bar_new - (3 /sqrt(100)) * sqrt(p_bar_new * (1 - p_bar_new))
lcl_p_new
ucl_p_new <- p_bar_new + (3 /sqrt(100)) * sqrt(p_bar_new * (1 - p_bar_new))
ucl_p_new