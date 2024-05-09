my_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/doe_prac_q9_data.csv', stringsAsFactors = TRUE)
dim(my_data)
names(my_data)
str(my_data)
View(my_data)
summary(my_data)

my_data_anova <- aov(response ~ block + factor_N * factor_P * factor_K, data = my_data)
summary(my_data_anova)

library(tidyverse)

# treatment_combination_totals

df1 <- my_data %>%
  group_by(treatment_combination) %>%
  summarise(treatment_combination_total = sum(response))

trt_comb_proper_order <- c("(1)", "n", "p", "np", "k", "nk", "pk", "npk")

temp_df <- df1

for (i in 1:8) {
  df1_index <- which(df1$treatment_combination == trt_comb_proper_order[i])
  temp_df[i,] <- df1[df1_index,]
}

df1 <- temp_df

View(df1)

# block totals

df2 <- my_data %>%
  group_by(block) %>%
  summarise(block_total = sum(response), block_total_sq = block_total^2)

View(df2)

# yates algorithm

library(yates.algo.doe.factorial.exp)

yates_operations <- yates_algo_factorial_exp(trt.comb = df1$treatment_combination, trt.comb.total = df1$treatment_combination_total, n = 3, nreplicates = 3)

yates_operations$sum_squares <- round(yates_operations$sum_squares, digits = 2)

View(yates_operations)

G <- sum(my_data$response)
G

CF <- G^2 / length(my_data$response)
CF

RSS <- sum(my_data$response^2)
RSS

TSS <- RSS - CF
TSS

sum(df2$block_total_sq)

SSblocks <- sum(df2$block_total_sq) / 4 - CF
SSblocks

source("D:\\Programming Languages\\R\\adjustment_factor_for_confounded_effects.R")

AF_NP <- adjustment_factor_for_confounded_effect(data = my_data, 
                                                 confounded_effect = "NP", 
                                                 confounded_in_replicate = "R1")
AF_NP

AF_NK <- adjustment_factor_for_confounded_effect(data = my_data,
                                                 confounded_effect = "NK",
                                                 confounded_in_replicate = "R2")
AF_NK

AF_NPK <- adjustment_factor_for_confounded_effect(data = my_data,
                                                  confounded_effect = "NPK",
                                                  confounded_in_replicate = "R3")

AF_NPK

total_confounded_effect <- function(confounded_effect, adjustment_factor) {
  
  index <- which(yates_operations$treatment_combinations == confounded_effect)
  total <- (yates_operations$run_3[index] - adjustment_factor)
  
  return(total)
  
}

total_confounded_effect("np", AF_NP)
total_confounded_effect("nk", AF_NK)
total_confounded_effect("npk", AF_NPK)

SS_confounded_effect <- function(confounded_effect, adjustment_factor) {
  
  index <- which(yates_operations$treatment_combinations == confounded_effect)
  SS <- (yates_operations$run_3[index] - adjustment_factor)^2 / (8 * 2)
  
  return(SS)
  
}

SS_NP <- SS_confounded_effect("np", AF_NP)
SS_NP

SS_NK <- SS_confounded_effect("nk", AF_NK)
SS_NK

SS_NPK <- SS_confounded_effect("npk", AF_NPK)
SS_NPK

rm_sum_squares <- c(which(yates_operations$treatment_combinations == "np"),
                    which(yates_operations$treatment_combinations == "nk"),
                    which(yates_operations$treatment_combinations == "npk"))
rm_sum_squares

SStreatments <- sum(yates_operations$sum_squares[setdiff(2:8, rm_sum_squares)],
                    SS_NP,
                    SS_NK,
                    SS_NPK)
SStreatments                    

SSE <- TSS - SSblocks - SStreatments
SSE

MSblocks <- SSblocks / 5
MSblocks

MStreatments <- SStreatments / 7
MStreatments <- round(MStreatments, digits = 2)
MStreatments

MSE <- SSE / 11
MSE

MSblocks / MSE
qf(0.05, 5, 11, lower.tail = FALSE)

yates_operations$sum_squares[which(yates_operations$treatment_combinations == 'p')] / MSE

SS_NP / MSE
qf(0.05, 1, 11, lower.tail = FALSE)
