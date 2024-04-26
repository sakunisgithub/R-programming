our_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/lsd_practical_2.csv", stringsAsFactors = TRUE)
dim(our_data)
names(our_data)
str(our_data)
View(our_data)
summary(our_data)

library(tidyverse)

our_data %>%
  ggplot(aes(sample = yield)) +
  stat_qq(size = 2, col = "red") +
  stat_qq_line(linewidth = 1, col = "blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")

shapiro.test(our_data$yield)

our_data_anova <- aov(yield ~ row + column + treatment, data = our_data)
summary(our_data_anova)

G <- sum(our_data$yield)
G

CF <- G^2 / length(our_data$yield)
CF

RSS <- sum(our_data$yield^2)
RSS

TSS <- RSS - CF
TSS

df1 <- our_data %>%
  group_by(row) %>%
  summarise(row_total = sum(yield), row_total_sq = row_total^2)

View(df1)

df2 <- our_data %>%
  group_by(column) %>%
  summarise(column_total = sum(yield), column_total_sq = column_total^2)

View(df2)

df3 <- our_data %>%
  group_by(treatment) %>%
  summarise(treatment_total = sum(yield), treatment_total_sq = treatment_total^2)

View(df3)

sum(df1$row_total_sq)
sum(df2$column_total_sq)
sum(df3$treatment_total_sq)

SSrow <- sum(df1$row_total_sq) / 5 - CF 
SSrow

SScolumn <- sum(df2$column_total_sq) / 5 - CF
SScolumn

SStreatment <- sum(df3$treatment_total_sq) / 5 - CF
SStreatment

SSE <- TSS - SSrow - SScolumn - SStreatment
SSE

MSrow <- SSrow / 4
MSrow

MScolumn <- SScolumn / 4
MScolumn

MStreatment <- SStreatment / 4
MStreatment

MSE <- SSE / 12
MSE

MStreatment / MSE
qf(0.05, 4, 12, lower.tail = FALSE)

critical_difference <- qt(0.025, 12, lower.tail = FALSE) * sqrt((2 * MSE) / 5)
critical_difference

df3$treatment_mean <- df3$treatment_total / 5
View(df3)

a <- TukeyHSD(our_data_anova)$treatment[,1]
a

abs(a)

which(abs(a) > critical_difference)
