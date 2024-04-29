our_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/lsd_practical_missing_data_1.csv", stringsAsFactors = TRUE)
dim(our_data)
names(our_data)
str(our_data)
View(our_data)
summary(our_data)

which(is.na(our_data$yield))
# so the yield corresponding to row 2, column 4, treatment C is missing.

m <- 5

R <- sum(our_data$yield[which(our_data$row == "R2")], na.rm = TRUE)
R

C <- sum(our_data$yield[which(our_data$column == "C4")], na.rm = TRUE)
C

T1 <- sum(our_data$yield[which(our_data$treatment == "C")], na.rm = TRUE)
T1

S <- sum(our_data$yield, na.rm = TRUE)
S

x_hat <- (m * (R+C+T1) - 2*S) / ((m-1)*(m-2))
x_hat <- round(x_hat, digits = 2)
x_hat

our_data_adjusted <- our_data
our_data_adjusted$yield[which(is.na(our_data_adjusted$yield))] <- x_hat
View(our_data_adjusted)

G <- sum(our_data_adjusted$yield)
G

CF <- G^2 / m^2
CF

RSS <- sum(our_data_adjusted$yield^2)
RSS

TSS <- RSS - CF
TSS

library(tidyverse)

df1 <- our_data_adjusted %>%
  group_by(row) %>%
  summarise(row_total = sum(yield), row_total_sq = row_total^2)

View(df1)

df2 <- our_data_adjusted %>%
  group_by(column) %>%
  summarise(column_total = sum(yield), column_total_sq = column_total^2)

View(df2)

df3 <- our_data_adjusted %>%
  group_by(treatment) %>%
  summarise(treatment_total = sum(yield), treatment_total_sq = treatment_total^2)

View(df3)

sum(df1$row_total_sq)
sum(df2$column_total_sq)
sum(df3$treatment_total_sq)

SSrow <- sum(df1$row_total_sq) / m - CF
SSrow

SScolumn <- sum(df2$column_total_sq) / m - CF
SScolumn

SStreatment <- sum(df3$treatment_total_sq) / m - CF
SStreatment

adjustement_factor <- (((m-1) * T1 + R + C - S) / ((m - 1) * (m - 2)))^2
adjustement_factor

SStreatment_adjusted <- SStreatment - adjustement_factor
SStreatment_adjusted

SSE <- TSS - SSrow - SScolumn - SStreatment_adjusted
SSE

MStreatment_adjusted <- SStreatment_adjusted / (m-1)
MStreatment_adjusted

MSE <- SSE / ((m-1)*(m-2) - 1)
MSE

MStreatment_adjusted / MSE

qf(0.05, 4, 11, lower.tail = FALSE)
