our_data <- read.csv("D:\\data_sets\\doe_prac_q2_data.csv", stringsAsFactors = TRUE)
View(our_data)
dim(our_data)
names(our_data)
summary(our_data)
library(tidyverse)
#normality checking
our_data %>%
  ggplot(aes(sample = yield)) +
  stat_qq(size = 2, col = "blue") +
  stat_qq_line(linewidth = 1, col = "red") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")
shapiro.test(our_data$yield)
#homoscedasticity checking
our_data %>%
  ggplot(aes(x = block, y = yield)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(fill = "#F93C09", linewidth = 1) +
  labs(x = "Block", y = "Yield", title = "Boxplot of Block vs Yield")
our_data %>%
  ggplot(aes(x = treatment, y = yield)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(fill = "#097AF9", linewidth = 1) +
  labs(x = "Treatment", y = "Yield", title = "Boxplot of Treatment vs Yield")
fit1 <- lm(yield ~ block + treatment, data = our_data)
ggplot(fit1, aes(x = fit1$fitted.values, y =fit1$residuals)) +
  geom_point(size = 2, col = "red") +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted Values")
our_anova <- aov(yield ~ block + treatment, data = our_data)
summary(our_anova)

df1 <- our_data %>%
  group_by(block) %>%
  summarise(rowtotal = sum(yield), rowtotal_sq = rowtotal^2)
View(df1)
df2 <- our_data %>%
  group_by(treatment) %>%
  summarise(columntotal = sum(yield), columntotal_sq = columntotal^2, treatment_means = mean(yield))
View(df2)
G <- sum(our_data$yield)
G
G^2
CF <- G^2 / length(our_data$yield)
CF
sum(df1$rowtotal_sq)
sum(df1$rowtotal_sq) / 6
SS_due_to_blocks <- sum(df1$rowtotal_sq) / 6 - CF
SS_due_to_blocks
sum(df2$columntotal_sq)
sum(df2$columntotal_sq) / 4
SS_due_to_treatments <- sum(df2$columntotal_sq) / 4 - CF
SS_due_to_treatments
RSS <- sum(our_data$yield^2)
RSS
TSS <- RSS - CF
TSS
SSE <- TSS - SS_due_to_blocks - SS_due_to_treatments
SSE
MS_due_to_blocks <- SS_due_to_blocks / 3
MS_due_to_blocks
MS_due_to_treatments <- SS_due_to_treatments / 5
MS_due_to_treatments
MSE <- SSE / 15
MSE
VR <- MS_due_to_treatments / MSE
VR
qf(0.05, 5, 15, lower.tail = FALSE)
VR > qf(0.05, 3, 5, lower.tail = FALSE)
qt(0.025, 15, lower.tail = FALSE)
critical_difference <- qt(0.025, 15, lower.tail = FALSE) * sqrt((2 * MSE) / 4)
critical_difference
TukeyHSD(our_anova)$treatment
imp <- which(abs(TukeyHSD(our_anova)$treatment[,1]) > critical_difference)
imp
TukeyHSD(our_anova)$treatment[,1][imp]
E <- ((4 * 5 * MSE) + (3 * MS_due_to_blocks))/(23 * MSE)
E
