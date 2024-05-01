our_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/factorial_design_practical_1.csv", stringsAsFactors = TRUE)
View(our_data)
dim(our_data)
names(our_data)
str(our_data)

our_data$N_code <- as.factor(our_data$N_code)
our_data$K_code <- as.factor(our_data$K_code)
our_data$D_code <- as.factor(our_data$D_code)

str(our_data)

summary(our_data)

our_data_anova <- aov(response ~ block + factor_N * factor_K * factor_D, data = our_data)
summary(our_data_anova)

G <- sum(our_data$response)
G

CF <- G^2 / length(our_data$response)
CF <- round(CF, digits = 2)
CF

RSS <- sum(our_data$response^2)
RSS

TSS <- RSS - CF
TSS

library(tidyverse)

df1 <- our_data %>%
  group_by(block) %>%
  summarise(block_total = sum(response), block_total_sq = block_total^2)

View(df1)

df2 <- our_data %>%
  group_by(treatment_combination) %>%
  summarise(treatment_combination_total = sum(response), treatment_combination_total_sq = treatment_combination_total^2)

temp_df <- df2

for (i in 1:8) {
  df2_index <- which(df2$treatment_combination == our_data$treatment_combination[1:8][i])
  temp_df[i,] <- df2[df2_index,]
}

df2 <- temp_df

View(df2)

sum(df1$block_total_sq)
sum(df2$treatment_combination_total_sq)

SSblock <- sum(df1$block_total_sq) / 8 - CF
SSblock

SStreatment_combinations <- sum(df2$treatment_combination_total_sq) / 4 - CF
SStreatment_combinations

SSE <- TSS - SSblock - SStreatment_combinations
SSE

library(yates.algo.doe.factorial.exp)

total_and_ss_of_treatment_combinations <- yates_algo_factorial_exp(df2$treatment_combination, df2$treatment_combination_total, n = 3, nreplicates = 4)

total_and_ss_of_treatment_combinations$sum_squares <- round(total_and_ss_of_treatment_combinations$sum_squares, digits = 2)

View(total_and_ss_of_treatment_combinations)

MSblock <- SSblock / 3
MSblock

MStreatment_combinations <- SStreatment_combinations / 7
MStreatment_combinations

MSE <- SSE / 21
MSE

MSblock / MSE

MStreatment_combinations / MSE

total_and_ss_of_treatment_combinations$sum_squares[2:8] / MSE

qf(0.05, 1, 21, lower.tail = FALSE)
