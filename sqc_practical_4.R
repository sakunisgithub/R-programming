fill_heights <- read.csv("D:\\data_sets\\sqc_practical_q4_data.csv")
dim(fill_heights)
names(fill_heights)
sample_totals <- rowSums(fill_heights)
sample_totals
sample_means <- rowMeans(fill_heights)
sample_means
mean_total <- sum(sample_means)
mean_total
x_bar_bar <- mean(sample_means)
x_bar_bar
sample_standard_deviations <- sqrt( rowSums(fill_heights^2) / 10 - (rowMeans(fill_heights))^2)
sample_standard_deviations <- round(sample_standard_deviations, digits = 2)
sample_standard_deviations
sd_total <- sum(sample_standard_deviations)
sd_total
s_bar <- mean(sample_standard_deviations)
s_bar
lcl_x <- x_bar_bar - 1.028 * s_bar
lcl_x
ucl_x <- x_bar_bar + 1.028 * s_bar
ucl_x
lcl_s <- 0.284 * s_bar
lcl_s
ucl_s <- 1.716 * s_bar
ucl_s
any(sample_means < lcl_x)
any(sample_means > ucl_x)
any(sample_standard_deviations < lcl_s)
any(sample_standard_deviations > ucl_s)