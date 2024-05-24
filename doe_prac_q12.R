my_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/doe_prac_q12_data.csv', stringsAsFactors = TRUE)
dim(my_data)
names(my_data)
str(my_data)
View(my_data)
summary(my_data)

options(digits = 10)

my_data_anova <- aov(response ~ block + factor_N * factor_K * factor_P, data = my_data)
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

trt_comb_proper_order <- c("(1)", "n", "k", "nk", "p", "np", "kp", "nkp")

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

SStreatments <- sum(yates_operations$sum_squares[2:7])
SStreatments

SSE <- TSS - SSblocks - SStreatments
SSE

MSblocks <- round(SSblocks / 7, digits = 2)
MSblocks

MStreatments <- round(SStreatments / 6, digits = 2)
MStreatments

MSE <- round(SSE / 18, digits = 2)
MSE

MSblocks / MSE
qf(0.05, 7, 18, lower.tail = FALSE)

MStreatments / MSE
qf(0.05, 6, 18, lower.tail = FALSE)

round(yates_operations$sum_squares[2:7] / MSE, digits = 2)
qf(0.05, 1, 18, lower.tail = FALSE)
