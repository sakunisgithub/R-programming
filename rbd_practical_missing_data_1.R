treatment_recovery_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/rbd_practical_missing_data_1.csv", stringsAsFactors = TRUE)
dim(treatment_recovery_data)
names(treatment_recovery_data)
str(treatment_recovery_data)
View(treatment_recovery_data)
summary(treatment_recovery_data)

which(is.na(treatment_recovery_data$recovery_days))

# the recovery days corresponding to Doctor B, Treatment 3 is missing.

library(tidyverse)

all_known_obs <- treatment_recovery_data %>%
  drop_na()

View(all_known_obs)

df1 <- all_known_obs %>%
  group_by(doctor) %>%
  summarise(row_total = sum(recovery_days))

View(df1)

df2 <- all_known_obs %>%
  group_by(treatment) %>%
  summarise(column_total = sum(recovery_days))

View(df2)

t <- 5 ; r <- 4

# dash_row or dash_column implies the row or column containing the missing value

dash_row_total <- df1$row_total[which(df1$doctor == "B")]
dash_row_total

dash_column_total <- df2$column_total[which(df2$treatment == "T3")]
dash_column_total

total_of_all_known_obs <- sum(all_known_obs$recovery_days)
total_of_all_known_obs

x_hat <- (t * dash_column_total + r * dash_row_total - total_of_all_known_obs) / ((t-1) * (r-1))
x_hat

treatment_recovery_data_adjusted <- treatment_recovery_data
treatment_recovery_data_adjusted$recovery_days[8] = x_hat
View(treatment_recovery_data_adjusted)

df1_adjusted <- treatment_recovery_data_adjusted %>%
  group_by(doctor) %>%
  summarise(row_total = sum(recovery_days), row_total_sq = row_total^2)

View(df1_adjusted)  

df2_adjusted <- treatment_recovery_data_adjusted %>%
  group_by(treatment) %>%
  summarise(column_total = sum(recovery_days), column_total_sq = column_total^2)

View(df2_adjusted)

sum(df1_adjusted$row_total_sq)
sum(df2_adjusted$column_total_sq)

G <- sum(treatment_recovery_data_adjusted$recovery_days)
G

CF <- G^2 / length(treatment_recovery_data_adjusted$recovery_days)
CF

RSS <- sum(treatment_recovery_data_adjusted$recovery_days^2)
RSS

TSS <- RSS - CF
TSS

SSdoctor <- sum(df1_adjusted$row_total_sq) / 5 - CF
SSdoctor

SStreatment <- sum(df2_adjusted$column_total_sq) / 4 - CF
SStreatment

adjustment_factor <- (t * dash_column_total + dash_row_total - total_of_all_known_obs)^2 / (t * (t-1) * (r-1)^2)
adjustment_factor

SStreatment_adjusted <- SStreatment - adjustment_factor
SStreatment_adjusted

SSE <- TSS - SSdoctor - SStreatment_adjusted
SSE

MSdoctor <- SSdoctor / 3
MSdoctor

MStreatments_adjusted <- SStreatment_adjusted / 4
MStreatments_adjusted

MSE <- SSE / 11
MSE

MStreatments_adjusted / MSE
qf(0.05, 4, 11, lower.tail = FALSE)
