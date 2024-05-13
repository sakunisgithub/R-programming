my_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/factorial_design_practical_2.csv', stringsAsFactors = TRUE)
dim(my_data)
names(my_data)
str(my_data)
View(my_data)
summary(my_data)

options(digits = 10)

my_data_anova <- aov(response ~ block + factor_N * factor_K * factor_D, data = my_data)
summary(my_data_anova)

library(tidyverse)
library(doe.addon)

df_block <- my_data %>%
  group_by(block) %>%
  summarise(block_total = sum(response), block_total_sq = block_total^2)

View(df_block)

df_treatment_combination <- my_data %>%
  group_by(treatment_combination) %>%
  summarise(treatment_combination_total = sum(response))

trt_comb_proper_order <- c("(1)", "n", "k", "nk", "d", "nd", "kd", "nkd")

df_treatment_combination <- order_trt_comb_col(dataframe = df_treatment_combination,
                   column_number = 1,
                   proper_order = trt_comb_proper_order)

View(df_treatment_combination)

yates_operations <- yates_algo_factorial_exp(trt.comb = df_treatment_combination$treatment_combination,
                                             trt.comb.total = df_treatment_combination$treatment_combination_total,
                                             n = 3,
                                             nreplicates = 4)

yates_operations$sum_squares <- round(yates_operations$sum_squares, digits = 2)

View(yates_operations)

SStreatments <- sum(yates_operations$sum_squares[2:7])
SStreatments

G <- yates_operations$run_3[which(yates_operations$treatment_combinations == "(1)")]
G

CF <- yates_operations$sum_squares[which(yates_operations$treatment_combinations == "(1)")]
CF

RSS <- sum(my_data$response^2)
RSS

TSS <- RSS - CF
TSS

sum(df_block$block_total_sq)

SSblocks <- sum(df_block$block_total_sq) / 4 - CF
SSblocks

SSE <- TSS - SSblocks - SStreatments
SSE

MSblocks <- SSblocks / 7
MSblocks

MStreatments <- SStreatments / 6
MStreatments

MSE <- SSE / 18
MSE

MSblocks / MSE

MStreatments / MSE
qf(0.05, 6, 18, lower.tail = FALSE)

round(yates_operations$sum_squares[2:7] / MSE, digits = 2)
qf(0.05, 1, 18, lower.tail = FALSE)
