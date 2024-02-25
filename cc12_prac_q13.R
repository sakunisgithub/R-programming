plant_data <- read.csv("D:\\data_sets\\cc12_prac_q13_data.csv", stringsAsFactors = TRUE)
dim(plant_data)
names(plant_data)
View(plant_data)
library(tidyverse)
plant_data %>%
  ggplot(aes(sample = yield)) +
  geom_qq(size = 2, col = "blue") +
  geom_qq_line(linewidth = 1, col = "red") +
  labs(y = "Sample Qualtile", x = "Theoretical Quantile", title = "Q-Q Plot")
plant_data %>%
  ggplot(aes(x = plot_no, y = yield)) +
  stat_boxplot(geom = "errorbar", linewidth = 1) +
  geom_boxplot(fill = "green") +
  labs(x = "Plot Number", y = "Yield", title = "Box-plot of Yield for differenct plots")
plant_data %>%
  ggplot(aes(x = temperature, y = yield)) +
  stat_boxplot(geom = "errorbar", linewidth = 1) +
  geom_boxplot(fill = "orange") +
  labs(x = "Temperature", y = "Yield", title = "Box-plot of Yield of different temperatures")
df1 <- plant_data %>%
  group_by(plot_no, temperature) %>%
  summarise(average_yield = mean(yield))
df1 %>%
  ggplot(aes(x = plot_no, y = average_yield)) +
  geom_line(aes(group = temperature, color = temperature), linewidth = 1.5) +
  geom_point(aes(group = temperature, color = temperature, size = 2)) +
  labs(x = "Plot Number", y = "Average Yield", title = "Interaction Plot of Plots and Temperatures")
plant_data_anova <- aov(yield ~ plot_no + temperature + plot_no : temperature, data = plant_data)
summary(plant_data_anova)


df2 <- plant_data %>%
  group_by(plot_no) %>%
  summarise(total_yield = sum(yield))
view(df2)

df3 <- plant_data %>%
  group_by(plot_no, temperature) %>%
  summarise(cell_total = sum(yield))
View(df3)

df4 <- plant_data %>%
  group_by(temperature) %>%
  summarise(total_yield = sum(yield))
view(df4)

df2$total_yield^2
df4$total_yield^2
sum(df2$total_yield^2)
sum(df4$total_yield^2)
sum(plant_data$yield)
cf <- sum(plant_data$yield)^2 / (2 * 10 * 3)
cf
ssa <- sum(df2$total_yield^2) / 6 - cf
ssa
ssb <- sum(df4$total_yield^2) / 20 - cf
ssb
ssab <- sum(df3$cell_total^2) / 2 - cf - ssa - ssb
ssab
tss <- sum(plant_data$yield^2) - cf
tss
sse <- tss - ssa - ssb - ssab
sse
msa <- ssa / 9
msa
msb <- ssb / 2
msb
msab <- ssab / 18
msab
mse <- sse / 30
mse
msa / mse
msb / mse
msab / mse