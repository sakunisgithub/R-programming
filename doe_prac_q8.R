our_data <- read.csv("D:\\data_sets\\doe_prac_q8_data.csv", stringsAsFactors = TRUE)
View(our_data)
dim(our_data)
names(our_data)
str(our_data)
our_data$N_code <- as.factor(our_data$N_code)
our_data$S_code <- as.factor(our_data$S_code)
str(our_data)
summary(our_data)

two_sq_factorial_anova <- aov(response ~ block + factor_N * factor_S, data = our_data)
summary(two_sq_factorial_anova)

our_data$adjusted_response <- our_data$response - 100
View(our_data)

G <- sum(our_data$adjusted_response)
G

CF <- G^2 / length(our_data$adjusted_response)
CF

RSS <- sum(our_data$adjusted_response^2)
RSS

TSS <- RSS - CF
TSS

library(tidyverse)

df1 <- our_data %>%
  group_by(block) %>%
  summarise(block_total = sum(adjusted_response), block_total_sq = block_total^2)
View(df1)

df2 <- our_data %>%
  group_by(treatment_combination) %>%
  summarise(treatment_combination_total = sum(adjusted_response), treatment_combination_total_sq = treatment_combination_total^2)
View(df2)

swap_data_frame_rows <- function(your_data_frame, row_1, row_2){
  
  temp <- your_data_frame[row_1,]
  your_data_frame[row_1,] <- your_data_frame[row_2,]
  your_data_frame[row_2,] <- temp
  
  return(your_data_frame)
}

df2 <- swap_data_frame_rows(df2, 3, 4)

View(df2)

sum(df1$block_total_sq)
sum(df2$treatment_combination_total_sq)

SSblocks <- sum(df1$block_total_sq) / 4 - CF
SSblocks

SStreatments <- sum(df2$treatment_combination_total_sq) / 6 - CF
SStreatments

SSE <- TSS - SSblocks - SStreatments
SSE

library(yates.algo.doe.factorial.exp)

yates_method <- yates_algo_factorial_exp(df2$treatment_combination, df2$treatment_combination_total, n = 2, nreplicates = 6)
View(yates_method)

MSblocks <- SSblocks / 5
MSblocks

MStreatments <- SStreatments / 3
MStreatments

MSE <- SSE / 15
MSE

MSblocks / MSE

yates_method$sum_squares[2:4] / MSE

qf(0.05, 1, 15, lower.tail = FALSE)

qf(0.05, 5, 15, lower.tail = FALSE)
