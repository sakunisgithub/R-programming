varietal_trial_data <- read.csv("D:\\data_sets\\doe_prac_q4_data.csv", stringsAsFactors = TRUE)
View(varietal_trial_data)
dim(varietal_trial_data)
names(varietal_trial_data)
summary(varietal_trial_data)

library(tidyverse)

varietal_trial_data %>%
  ggplot(aes(sample = yield)) +
  stat_qq(size = 2, col = "red") +
  stat_qq_line(linewidth = 1, col = "blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")

shapiro.test(varietal_trial_data$yield)

varietal_trial_data %>%
  ggplot(aes(x = blocks, y = yield)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(fill = "#1379ED", linewidth = 1) +
  labs(x = "Blocks", y = "Yield", title = "Yield vs Blocks Boxplot")

varietal_trial_data %>%
  ggplot(aes(x = varieties, y = yield)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(fill = "#13ED41", linewidth = 1) +
  labs(x = "Varieties", y = "Yield", title = "Yield vs Varieties Boxplot")

varietal_trial_data_anova <- aov(yield ~ blocks + varieties, data = varietal_trial_data)
summary(varietal_trial_data_anova)

G <- sum(varietal_trial_data$yield)
G

CF <- G^2 / length(varietal_trial_data$yield)
CF

RSS <- sum(varietal_trial_data$yield^2)
RSS

TSS <- RSS - CF
TSS

df1 <- varietal_trial_data %>%
  group_by(blocks) %>%
  summarise(row_total = sum(yield), row_total_sq = row_total^2)
View(df1)
sum(df1$row_total_sq)
SSblocks <- sum(df1$row_total_sq) / 6 - CF
SSblocks

df2 <- varietal_trial_data %>%
  group_by(varieties) %>%
  summarise(column_total = sum(yield), column_total_sq = column_total^2)
View(df2)
sum(df2$column_total_sq)
SSvarieties <- sum(df2$column_total_sq) / 5 - CF
SSvarieties

SSE <- TSS - SSblocks - SSvarieties
SSE

MSblocks <- SSblocks / 4
MSblocks

MSvarieties <- SSvarieties / 5
MSvarieties

MSE <- SSE / 20
MSE

MSvarieties / MSE
qf(0.05, 5, 20, lower.tail = FALSE)
