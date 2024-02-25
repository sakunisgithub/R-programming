defectives <- read.csv("D:\\data_sets\\sqc_practical_q9_data.csv")
dim(defectives)
names(defectives)
defectives
sum(defectives$no_of_defectives)
fraction_defectives <- defectives$no_of_defectives / 10
fraction_defectives
sum(fraction_defectives)
p_bar <- mean(fraction_defectives)
p_bar
lcl <- 10 * p_bar - 3 * sqrt(10 * p_bar * (1-p_bar))
lcl
ucl <- 10 * p_bar + 3 * sqrt(10 * p_bar * (1-p_bar))
ucl
which(defectives$no_of_defectives > 4.98)
