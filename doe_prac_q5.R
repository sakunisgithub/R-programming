our_data <- read.csv("D:\\data_sets\\doe_prac_q5_data.csv", stringsAsFactors = TRUE)
class(our_data)
View(our_data)
dim(our_data)
names(our_data)
summary(our_data)
library(tidyverse)
#normality test
our_data %>%
  ggplot(aes(sample = yield)) +
  stat_qq(size = 2, col = "red") +
  stat_qq_line(linewidth = 1, col = "blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")

shapiro.test(our_data$yield)

our_data %>%
  ggplot(aes(x = village, y = yield)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(fill = "#DB2AF0", linewidth = 1) +
  labs(x = "Villages", y = "Yield", title = "Boxplot of Yield vs Villages")

our_data %>%
  ggplot(aes(x = treatment, y = yield)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(fill = "#0CECAF", linewidth = 1) +
  labs(x = "Treatment", y = "Yield", title = "Boxplot of Yield vs Treatments")

our_data_anova <- aov(yield ~ village + treatment, data = our_data)
summary(our_data_anova)

yield_adjusted <- (our_data$yield - 500) / 10
yield_adjusted

adjusted_data <- our_data
adjusted_data$yield_adjusted <- yield_adjusted
View(adjusted_data)

G <- sum(adjusted_data$yield_adjusted)
G

CF <- G^2 / length(adjusted_data$yield_adjusted)
CF

RSS <- sum(adjusted_data$yield_adjusted^2)
RSS

TSS <- (RSS - CF) * 10^2
TSS

df1 <- adjusted_data %>%
  group_by(village) %>%
  summarise(row_total = sum(yield_adjusted), row_total_sq = row_total^2)
View(df1)

df2 <- adjusted_data %>%
  group_by(treatment) %>%
  summarise(column_total = sum(yield_adjusted), column_total_sq = column_total^2)
View(df2)

df_3 <- our_data %>%
  group_by(treatment) %>%
  summarise(column_total = sum(yield), column_mean = mean(yield))
View(df_3)

sum(df1$row_total_sq)
sum(df2$column_total_sq)

SSvillage <- (sum(df1$row_total_sq) / 4 - CF) * 10^2
SSvillage

SStreatments <- (sum(df2$column_total_sq) / 3 - CF) * 10^2
SStreatments

SSE <- TSS - SSvillage - SStreatments
SSE

MSvillage <- SSvillage / 2
MSvillage

MStreatments <- SStreatments / 3
MStreatments

MSE <- SSE / 6
MSE

MStreatments / MSE
qf(0.05, 3, 6, lower.tail = FALSE)

a <- TukeyHSD(our_data_anova)$treatment[,1]
a

critical_difference <- qt(0.025, 6, lower.tail = FALSE) * sqrt((2 * MSE)/3)
critical_difference

which(abs(a) > critical_difference)
