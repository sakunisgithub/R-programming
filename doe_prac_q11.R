my_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/doe_prac_q11_data.csv', stringsAsFactors = TRUE)
dim(my_data)
names(my_data)
str(my_data)
View(my_data)
summary(my_data)

my_data_anova <- aov(response ~ block + factor_N * factor_P * factor_K, data = my_data)
summary(my_data_anova)

library(tidyverse)

# block totals

block_totals <- my_data %>%
  group_by(block) %>%
  summarise(block_total = sum(response), block_total_sq = block_total^2)

View(block_totals)

# treatment combination totals

treatment_combination_totals <- my_data %>%
  group_by(treatment_combination) %>%
  summarise(treatment_combination_total = sum(response))

library(doe.addon)

trt_comb_proper_order <- c("(1)", "n", "p", "np", "k", "nk", "pk", "npk")

treatment_combination_totals <- order_trt_comb_col(dataframe = treatment_combination_totals,
                                                   column_number = 1,
                                                   proper_order = trt_comb_proper_order)

View(treatment_combination_totals)

yates_operations <- yates_algo_factorial_exp(trt.comb = treatment_combination_totals$treatment_combination,
                                             trt.comb.total = treatment_combination_totals$treatment_combination_total,
                                             n = 3,
                                             nreplicates = 3)

yates_operations$sum_squares <- round(yates_operations$sum_squares, digits = 2)

View(yates_operations)

# NPK is confounded in replicate I and II and PK is confounded in replicate III.

AF_NPK_rep_1 <- adjustment_factor_for_confounded_effect(data = my_data,
                                                        confounded_effect = "npk",
                                                        confounded_in_replicate = "R1")

AF_NPK_rep_1

AF_NPK_rep_2 <- adjustment_factor_for_confounded_effect(data = my_data,
                                                        confounded_effect = "npk",
                                                        confounded_in_replicate = "R2")

AF_NPK_rep_2

NPK_total_effect <- yates_operations$run_3[which(yates_operations$treatment_combinations == "npk")] -
                    AF_NPK_rep_1 -
                    AF_NPK_rep_2

NPK_total_effect

SS_NPK <- NPK_total_effect^2 / 8
SS_NPK

AF_PK <- adjustment_factor_for_confounded_effect(data = my_data,
                                                 confounded_effect = "pk",
                                                 confounded_in_replicate = "R3")


AF_PK

PK_total_effect <- yates_operations$run_3[which(yates_operations$treatment_combinations == "pk")] - AF_PK

PK_total_effect

SS_PK <- round(PK_total_effect^2 / 16, digits = 2)
SS_PK

G <- yates_operations$run_3[which(yates_operations$treatment_combinations == "(1)")]
G

CF <- yates_operations$sum_squares[which(yates_operations$treatment_combinations == "(1)")]
CF

RSS <- sum(my_data$response^2)
RSS

TSS <- RSS - CF
TSS

sum(block_totals$block_total_sq)

SSblocks <- sum(block_totals$block_total_sq) / 4 - CF
SSblocks <- round(SSblocks, digits = 2)
SSblocks

SStreatments <- sum(yates_operations$sum_squares[2:6]) + SS_PK + SS_NPK
SStreatments

SSE <- TSS - SSblocks - SStreatments
SSE

MSblocks <- SSblocks / 5
MSblocks

MStreatments <- SStreatments / 7
MStreatments

MSE <- SSE / 11
MSE

MSblocks / MSE
qf(0.05, 5, 11, lower.tail = FALSE)

MStreatments / MSE

round(yates_operations$sum_squares[2:6] / MSE, digits = 2)
SS_PK / MSE
SS_NPK / MSE
qf(0.05, 1, 11, lower.tail = FALSE)
