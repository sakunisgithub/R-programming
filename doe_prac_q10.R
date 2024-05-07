brownie_test_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/doe_prac_q10_data.csv", stringsAsFactors = TRUE)
dim(brownie_test_data)
names(brownie_test_data)
str(brownie_test_data)

brownie_test_data$A_code <- as.factor(brownie_test_data$A_code)
brownie_test_data$B_code <- as.factor(brownie_test_data$B_code)
brownie_test_data$C_code <- as.factor(brownie_test_data$C_code)

str(brownie_test_data)

View(brownie_test_data)

library(tidyverse)

brownie_test_data %>%
  ggplot(aes(x = result, y = test_panel, colour = test_panel)) +
  geom_point(pch = 15, size = 3) +
  theme(legend.position = "top")

brownie_test_data %>%
  ggplot(aes(x = result, y = treatment_combination, colour = treatment_combination)) +
  geom_point(pch = 15, size = 3) +
  theme(legend.position = "top")

summary(brownie_test_data)

shapiro.test(brownie_test_data$result)

brownie_test_data_anova <- aov(result ~ test_panel + factor_A * factor_B * factor_C, data = brownie_test_data)
summary(brownie_test_data_anova)

G <- sum(brownie_test_data$result)
G

CF <- G^2 / length(brownie_test_data$result)
CF

RSS <- sum(brownie_test_data$result^2)
RSS

TSS <- RSS - CF
TSS

df1 <- brownie_test_data %>%
  group_by(test_panel) %>%
  summarise(test_panel_total = sum(result), test_panel_total_sq = test_panel_total^2)

View(df1)

df2 <- brownie_test_data %>%
  group_by(treatment_combination) %>%
  summarise(treatment_combination_total = sum(result), treatment_combination_total_sq = treatment_combination_total^2)

temp_df <- df2

for (i in 1:8) {
  df2_index <- which(df2$treatment_combination == brownie_test_data$treatment_combination[1:8][i])
  temp_df[i,] <- df2[df2_index,]
}

df2 <- temp_df

View(df2)

sum(df1$test_panel_total_sq)
sum(df2$treatment_combination_total_sq)

SStest_panel <- sum(df1$test_panel_total_sq) / 8 - CF
SStest_panel

# SStreatment_combination <- sum(df2$treatment_combination_total_sq) / 8 - CF
# SStreatment_combination


library(yates.algo.doe.factorial.exp)

total_and_ss_of_factorial_effects <- yates_algo_factorial_exp(df2$treatment_combination, df2$treatment_combination_total, n = 3, nreplicates = 8)

total_and_ss_of_factorial_effects$sum_squares <- round(total_and_ss_of_factorial_effects$sum_squares, digits = 2)

View(total_and_ss_of_factorial_effects)

SStreatment_combination <- sum(total_and_ss_of_factorial_effects$sum_squares[2:8])
SStreatment_combination

SSE <- TSS - SStest_panel - SStreatment_combination
SSE

MStest_panel <- SStest_panel / 7
MStest_panel

MStreatment_combination <- SStreatment_combination / 7
MStreatment_combination

MSE <- SSE / 49
MSE <- round(MSE, digits = 2)
MSE

MStest_panel / MSE

variance_ratios <- total_and_ss_of_factorial_effects$sum_squares[2:8] / MSE
variance_ratios <- round(variance_ratios, digits = 2)
variance_ratios

qf(0.05, 7, 49, lower.tail = FALSE)
qf(0.05, 1, 49, lower.tail = FALSE)
