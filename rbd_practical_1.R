doctor_treatment_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/rbd_practical_1.csv", stringsAsFactors = TRUE)
dim(doctor_treatment_data)
View(doctor_treatment_data)
names(doctor_treatment_data)
str(doctor_treatment_data)
summary(doctor_treatment_data)

library(tidyverse)

doctor_treatment_data %>%
  ggplot(aes(sample = recovery_days)) +
  stat_qq(size = 2, col = "red") +
  stat_qq_line(linewidth = 1, col = "blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")

shapiro.test(doctor_treatment_data$recovery_days)

doctor_treatment_data_anova <- aov(recovery_days ~ doctor + treatment, data = doctor_treatment_data)
summary(doctor_treatment_data_anova)

G <- sum(doctor_treatment_data$recovery_days)
G

CF <- G^2 / length(doctor_treatment_data$recovery_days)
CF

RSS <- sum(doctor_treatment_data$recovery_days^2)
RSS

TSS <- RSS - CF
TSS

df1 <- doctor_treatment_data %>%
  group_by(doctor) %>%
  summarise(doctor_total = sum(recovery_days), doctor_total_sq = doctor_total^2)

View(df1)

df2 <- doctor_treatment_data %>%
  group_by(treatment) %>%
  summarise(treatment_total = sum(recovery_days), treatment_total_sq = treatment_total^2)

View(df2)

sum(df1$doctor_total_sq)
sum(df2$treatment_total_sq)

SSdoctor <- sum(df1$doctor_total_sq) / 5 - CF
SSdoctor

SStreatment <- sum(df2$treatment_total_sq) / 4 - CF
SStreatment

SSE <- TSS - SSdoctor - SStreatment
SSE

MSdoctor <- SSdoctor / 3
MSdoctor

MStreatment <- SStreatment / 4
MStreatment

MSE <- SSE / 12
MSE

MSdoctor / MSE
qf(0.05, 3, 12, lower.tail = FALSE)

MStreatment / MSE
qf(0.05, 4, 12, lower.tail = FALSE)
