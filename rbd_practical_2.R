our_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/rbd_practical_2.csv", stringsAsFactors = TRUE)
dim(our_data)
View(our_data)
names(our_data)
str(our_data)
summary(our_data)

library(tidyverse)

our_data %>%
  ggplot(aes(sample = yield)) +
  stat_qq(size = 2, col = "red") +
  stat_qq_line(linewidth = 1, col = "blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")

shapiro.test(our_data$yield)

our_data_anova <- aov(yield ~ block + treatment, data = our_data)
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
  group_by(block) %>%
  summarise(block_total = sum(yield), block_total_sq = block_total^2)

View(df1)

df2 <- our_data %>%
  group_by(treatment) %>%
  summarise(treatment_total = sum(yield), treatment_total_sq = treatment_total^2)

View(df2)

sum(df1$block_total_sq)
sum(df2$treatment_total_sq)

SSblock <- sum(df1$block_total_sq) / 6 - CF
SSblock

SStreatment <- sum(df2$treatment_total_sq) / 4 - CF
SStreatment

SSE <- TSS - SSblock - SStreatment
SSE

MSblock <- SSblock / 3
MSblock

MStreatment <- SStreatment / 5
MStreatment

MSE <- SSE / 15
MSE

MSblock / MSE
qf(0.05, 3, 15, lower.tail = FALSE)

MStreatment / MSE
qf(0.05, 5, 15, lower.tail = FALSE)

df2$treatment_mean <- df2$treatment_total / 4
View(df2)

critical_difference <- qt(0.025, 15, lower.tail = FALSE) * sqrt((2 * MSE) / 4)
critical_difference

a <- TukeyHSD(our_data_anova)$treatment[,1]

round(a, digits = 3)

round(abs(a), digits = 3)

which(abs(a) > critical_difference)

