wheat_data <- read.csv("D:\\data_sets\\doe_prac_q7_data.csv", stringsAsFactors = TRUE)
View(wheat_data)
dim(wheat_data)
names(wheat_data)
summary(wheat_data)
which(is.na(wheat_data$yield))
# row3 column4 yield is not available

a <- which(wheat_data$row == "R3")
a
R <- sum(wheat_data$yield[a], na.rm = TRUE)
R

b <- which(wheat_data$column == "C4")
b
C <- sum(wheat_data$yield[b], na.rm = TRUE)
C

c <- which(wheat_data$treatment == "E")
c
T1 <- sum(wheat_data$yield[c], na.rm = TRUE)
T1

S <- sum(wheat_data$yield, na.rm = TRUE)
S

m <- 5

x_hat <- (m * (R + C + T1) - 2 * S) / ((m - 1) * (m-2))
x_hat <- round(x_hat, digits = 2)
x_hat

adjusted_wheat_data <- wheat_data
adjusted_wheat_data$yield[14] = x_hat
View(adjusted_wheat_data)

G <- sum(adjusted_wheat_data$yield)
G

CF <- G^2 / 25
CF

RSS <- sum(adjusted_wheat_data$yield^2)
RSS

TSS <- RSS - CF
TSS

library(tidyverse)

df1 <- adjusted_wheat_data %>%
  group_by(row) %>%
  summarise(row_total = sum(yield), row_total_sq = row_total^2)
View(df1)

df2 <- adjusted_wheat_data %>%
  group_by(column) %>%
  summarise(column_total = sum(yield), column_total_sq = column_total^2)
View(df2)

df3 <- adjusted_wheat_data %>%
  group_by(treatment) %>%
  summarise(treatment_total = sum(yield), treatment_total_sq = treatment_total^2)
View(df3)

sum(df1$row_total_sq)
sum(df2$column_total_sq)
sum(df3$treatment_total_sq)

SSrow <- sum(df1$row_total_sq) / 5 - CF
SSrow

SScolumn <- sum(df2$column_total_sq) / 5 - CF
SScolumn <- round(SScolumn, digits = 2)
SScolumn

SStreatments <- sum(df3$treatment_total_sq) / 5 - CF
SStreatments <- round(SStreatments, digits = 2)
SStreatments

adjustment_factor <- ( ((m - 1) * T1 + R + C - S) / ((m - 1) * (m - 2)) )^2
adjustment_factor <- round(adjustment_factor, digits = 2)
adjustment_factor

SStreatments_adjusted <- SStreatments - adjustment_factor
SStreatments_adjusted

SSE_adjusted <- TSS - SSrow - SScolumn - SStreatments_adjusted
SSE_adjusted <- round(SSE_adjusted, digits = 2)
SSE_adjusted

MSrow <- SSrow / 4
MSrow

MScolumn <- SScolumn / 4
MScolumn

MStreatments_adjusted <- SStreatments_adjusted / 4
MStreatments_adjusted

MSE_adjusted <- SSE_adjusted / 11
MSE_adjusted

MStreatments_adjusted / MSE_adjusted
qf(0.05, 4, 11, lower.tail = FALSE)
