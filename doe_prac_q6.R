our_data <- read.csv("D:\\data_sets\\doe_prac_q6_data.csv", stringsAsFactors = TRUE)
View(our_data)
dim(our_data)
names(our_data)
summary(our_data)
which(is.na(our_data$response))
# block2, treatment1 response is unknown

library(tidyverse)

all_known_obs <- our_data %>%
  drop_na()

View(all_known_obs)

df1 <- all_known_obs %>%
  group_by(block) %>%
  summarise(row_total = sum(response))
View(df1)

df2 <- all_known_obs %>%
  group_by(treatment) %>%
  summarise(column_total = sum(response))
View(df2)

sum(all_known_obs$response)

x_hat <- (4 * df1$row_total[2] + 3 * df2$column_total[1] - sum(all_known_obs$response)) / (3 * 2)
x_hat

adjusted_data <- our_data
adjusted_data$response[which(is.na(adjusted_data$response))] = x_hat
View(adjusted_data)

G <- sum(adjusted_data$response)
G

CF <- G^2 / length(adjusted_data$response)
CF

RSS <- sum(adjusted_data$response^2)
RSS

TSS <- RSS - CF
TSS

df3 <- adjusted_data %>%
  group_by(block) %>%
  summarise(row_total = sum(response), row_total_sq = row_total^2)
View(df3)

df4 <- adjusted_data %>%
  group_by(treatment) %>%
  summarise(column_total = sum(response), column_total_sq = column_total^2)
View(df4)

sum(df3$row_total_sq)
sum(df4$column_total_sq)

SSblocks <- (sum(df3$row_total_sq) / 3) - CF
SSblocks

SStreatments <- (sum(df4$column_total_sq) / 4) - CF
SStreatments

adjustment_factor <- (df1$row_total[2] + 3 * df2$column_total[1] - sum(all_known_obs$response))^2 / (3 * 2 * 3^2)
adjustment_factor

SStreatments_adjusted <- SStreatments - adjustment_factor
SStreatments_adjusted

SSE <- TSS - SStreatments - SSblocks
SSE

MSblocks <- SSblocks / 3
MSblocks

MStreatments_adjusted <- SStreatments_adjusted / 2
MStreatments_adjusted

MSE <- SSE / 5
MSE

MStreatments_adjusted / MSE
qf(0.05, 2, 5, lower.tail = FALSE)

critical_differene <- qt(0.025, 5, lower.tail = FALSE) * sqrt( MSE * ( 2/4 + 3/(4 * 3 * 2) ) )
critical_differene

T_1_T_2_total_difference <- df4$column_total[1] - df4$column_total[2]
T_1_T_2_mean_difference <- T_1_T_2_total_difference / 4
T_1_T_2_mean_difference
abs(T_1_T_2_mean_difference) > critical_differene
