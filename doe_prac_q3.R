barley_grain_yield_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/doe_prac_q3_data.csv", stringsAsFactors = TRUE)
View(barley_grain_yield_data)
dim(barley_grain_yield_data)
names(barley_grain_yield_data)
summary(barley_grain_yield_data)
library(tidyverse)
#normality checking
barley_grain_yield_data %>%
  ggplot(aes(sample = yield)) +
  stat_qq(size = 2, col = "blue") +
  stat_qq_line(linewidth = 1, col = "red") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")
shapiro.test(barley_grain_yield_data$yield)

barley_grain_yield_data_anova <- aov(yield ~ row + column + treatment, data = barley_grain_yield_data)
summary(barley_grain_yield_data_anova)

G <- sum(barley_grain_yield_data$yield)
G

CF <- G^2 / 16
CF

RSS <- sum(barley_grain_yield_data$yield^2)
RSS

TSS <- RSS - CF
TSS

df1 <- barley_grain_yield_data %>%
  group_by(row) %>%
  summarise(row_total = sum(yield), row_total_sq = row_total^2)
View(df1)
SSrow <- sum(df1$row_total_sq) / 4 - CF
SSrow

df2 <- barley_grain_yield_data %>%
  group_by(column) %>%
  summarise(column_total = sum(yield), column_total_sq = column_total^2)
View(df2)
SScolumn <- sum(df2$column_total_sq) / 4 - CF
SScolumn

df3 <- barley_grain_yield_data %>%
  group_by(treatment) %>%
  summarise(treatment_total = sum(yield), treatment_total_sq = treatment_total^2)
View(df3)
SStreatment <- sum(df3$treatment_total_sq) / 4 - CF
SStreatment

SSE <- TSS - SSrow - SScolumn - SStreatment
SSE

MSrow <- SSrow / 3
MSrow

MScolumn <- SScolumn / 3
MScolumn

MStreatment <- SStreatment / 3
MStreatment

MSE <- SSE / 6
MSE

critical_difference <- sqrt((2 * MSE) / 4) * qt(0.025, 6, lower.tail = FALSE)
critical_difference

# efficiency of LSD over RBD when rows are taken as blocks
E1 <- (MScolumn + 3 * MSE) / (4 * MSE)
E1

# efficiency of LSD over RBD when columns are taken as blocks
E2 <- (MSrow + 3 * MSE) / (4 * MSE)
E2

# efficiency of LSE over CRD
E3 <- (MSrow + MScolumn + 3 * MSE) / (5 * MSE)
E3



barley_grain_yield_data_part3 <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/doe_prac_q3_part3_data.csv", stringsAsFactors = TRUE)
View(barley_grain_yield_data_part3)
summary(barley_grain_yield_data_part3)
which(is.na(barley_grain_yield_data_part3$yield))

# the missing value is 9th yield i.e. 3rd row - 1st column, receiving treatment A

S <- sum(barley_grain_yield_data_part3$yield, na.rm = TRUE)
S

a <- which(barley_grain_yield_data_part3$row == "R3")
a
R <- sum(barley_grain_yield_data_part3$yield[a], na.rm = TRUE)
R

b <- which(barley_grain_yield_data_part3$column == "C1")
b
C <- sum(barley_grain_yield_data_part3$yield[b], na.rm = TRUE)
C

c <- which(barley_grain_yield_data_part3$treatment == "A")
c
T1 <- sum(barley_grain_yield_data_part3$yield[c], na.rm = TRUE)
T1

x_hat <- (4 * (R + C + T1) - 2 * S) / (3 * 2)
x_hat

barley_grain_yield_data_part3_adjusted <- barley_grain_yield_data_part3
barley_grain_yield_data_part3_adjusted$yield[9] = x_hat
View(barley_grain_yield_data_part3_adjusted)

G_adj <- sum(barley_grain_yield_data_part3_adjusted$yield)
G_adj

CF_adj <- G_adj^2 / 16
CF_adj

RSS_adj <- sum(barley_grain_yield_data_part3_adjusted$yield^2)
RSS_adj

TSS_adj <- RSS_adj - CF_adj
TSS_adj

df1_adj <- barley_grain_yield_data_part3_adjusted %>%
  group_by(row) %>%
  summarise(row_total = sum(yield), row_total_sq = row_total^2)
View(df1_adj)
SSrow_adj <- sum(df1_adj$row_total_sq) / 4 - CF_adj
SSrow_adj

df2_adj <- barley_grain_yield_data_part3_adjusted %>%
  group_by(column) %>%
  summarise(column_total = sum(yield), column_total_sq = column_total^2)
View(df2_adj)
SScolumn_adj <- sum(df2_adj$column_total_sq) / 4 - CF_adj
SScolumn_adj

df3_adj <- barley_grain_yield_data_part3_adjusted %>%
  group_by(treatment) %>%
  summarise(treatment_total = sum(yield), treatment_total_sq = treatment_total^2)
View(df3_adj)
adjustment_factor_for_treatment <- ((3 * T1 + R + C - S) / (3 * 2))^2
adjustment_factor_for_treatment
SStreatment_adj <- sum(df3_adj$treatment_total_sq) / 4 - CF_adj - adjustment_factor_for_treatment
SStreatment_adj

SSE_adj <- TSS_adj - SSrow_adj - SScolumn_adj - SStreatment_adj
SSE_adj

MSrow_adj <- SSrow_adj / 3
MSrow_adj

MScolumn_adj <- SScolumn_adj / 3
MScolumn_adj

MStreatment_adj <- SStreatment_adj / 3
MStreatment_adj

MSE_adj <- SSE_adj / 5
MSE_adj

MSrow_adj/MSE_adj
MScolumn_adj/MSE_adj
MStreatment_adj/MSE_adj
