my_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/factorial_design_practical_3.csv', stringsAsFactors = TRUE)
dim(my_data)
names(my_data)
colnames(my_data)[7] <- "response"
str(my_data)
View(my_data)
summary(my_data)

my_data_anova <- aov(response ~ block + factor_N * factor_P * factor_K, data = my_data)
summary(my_data_anova)

library(tidyverse)

# block totals

df_block <- my_data %>%
  group_by(block) %>%
  summarise(block_total = sum(response), block_total_sq = block_total^2)

View(df_block)

# treatment combination totals

df_treatment_combinations <- my_data %>%
  group_by(treatment_combination) %>%
  summarise(treatment_combination_total = sum(response))

library(doe.addon)

trt_comb_proper_order <- c("(1)", "n", "p", "np", "k", "nk", "pk", "npk")

df_treatment_combinations <- order_trt_comb_col(dataframe = df_treatment_combinations,
                                                column_number = 1,
                                                proper_order = trt_comb_proper_order)

View(df_treatment_combinations)

yates_operations <- yates_algo_factorial_exp(trt.comb = df_treatment_combinations$treatment_combination,
                                             trt.comb.total = df_treatment_combinations$treatment_combination_total,
                                             n = 3,
                                             nreplicates = 4)

yates_operations$sum_squares <- round(yates_operations$sum_squares, digits = 2)

View(yates_operations)

AF_NPK <- adjustment_factor_for_confounded_effect(data = my_data,
                                                  confounded_effect = "npk",
                                                  confounded_in_replicate = "R1")

NPK_effect_total <- yates_operations$run_3[which(yates_operations$treatment_combinations == "npk")] - AF_NPK
NPK_effect_total

AF_NP <- adjustment_factor_for_confounded_effect(data = my_data,
                                                  confounded_effect = "np",
                                                  confounded_in_replicate = "R2")

NP_effect_total <- yates_operations$run_3[which(yates_operations$treatment_combinations == "np")] - AF_NP
NP_effect_total

AF_NK <- adjustment_factor_for_confounded_effect(data = my_data,
                                                  confounded_effect = "nk",
                                                  confounded_in_replicate = "R3")

NK_effect_total <- yates_operations$run_3[which(yates_operations$treatment_combinations == "nk")] - AF_NK
NK_effect_total

AF_PK <- adjustment_factor_for_confounded_effect(data = my_data,
                                                  confounded_effect = "pk",
                                                  confounded_in_replicate = "R4")

PK_effect_total <- yates_operations$run_3[which(yates_operations$treatment_combinations == "pk")] - AF_PK
PK_effect_total

G <- sum(my_data$response)
G

CF <- G^2 / length(my_data$response)
CF <- round(CF, digits = 2)
CF

RSS <- sum(my_data$response^2)
RSS

TSS <- RSS - CF
TSS

sum(df_block$block_total_sq)

SSblocks <- sum(df_block$block_total_sq) / 4 - CF
SSblocks

temp_df <- data.frame(treatment_combination = yates_operations$treatment_combinations,
                      sum_squares = yates_operations$sum_squares)

temp_df <- temp_df[2:8,]

temp_df$sum_squares[which(temp_df$treatment_combination == "npk")] <- NPK_effect_total^2 / 24
temp_df$sum_squares[which(temp_df$treatment_combination == "np")] <- NP_effect_total^2 / 24
temp_df$sum_squares[which(temp_df$treatment_combination == "nk")] <- NK_effect_total^2 / 24
temp_df$sum_squares[which(temp_df$treatment_combination == "pk")] <- PK_effect_total^2 / 24

temp_df$sum_squares <- round(temp_df$sum_squares, digits = 2)

View(temp_df)

SStreatments <- sum(temp_df$sum_squares)
SStreatments

SSE <- TSS - SSblocks - SStreatments
SSE

MSblocks <- SSblocks / 7
MSblocks

MStreatments <- SStreatments / 7
MStreatments <- round(MStreatments, digits = 2)
MStreatments

MSE <- SSE / 17
MSE <- round(MSE, digits = 2)
MSE

MSblocks / MSE
qf(0.05, 7, 17, lower.tail = FALSE)

MStreatments / MSE
qf(0.05, 7, 17, lower.tail = FALSE)

round(temp_df$sum_squares / MSE, digits = 2)
qf(0.05, 1, 17, lower.tail = FALSE)
