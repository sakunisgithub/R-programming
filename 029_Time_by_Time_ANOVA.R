df <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/refs/heads/master/msc_semester_4/Weight_of_Calves.csv', stringsAsFactors = TRUE)

View(df)

dim(df)

colnames(df)

# need some renaming
colnames(df) <- c("Animal", "Treatment.Group", sprintf('%02d', 1:11))

View(df)

library(tidyverse)

# melting for further calculations
df_melted <- df %>%
  pivot_longer(cols = 3:13,
               values_to = "Weight",
               names_to = "Time")

View(df_melted)

# computing observed mean response profiles
df1 <- df_melted %>%
  group_by(Treatment.Group, Time) %>%
  summarise(avg.weight = mean(Weight), .groups = "keep") %>%
  arrange(Time)

View(df1)

# mean response profile plot
df1 %>%
  ggplot(aes(x = Time, y = avg.weight, 
             group = Treatment.Group, 
             linetype = Treatment.Group)) +
  geom_line(linewidth = 1) +
  labs(x = "Time Point", y = "Average Weight") +
  theme_minimal() +
  theme(legend.position = "top")

# Time-by-Time ANOVA
test.statistic <- c()

for (i in 1:11) {
  temp.df <- df_melted %>%
    filter(as.numeric(Time) == i)
  
  test.statistic[i] <- t.test(Weight ~ Treatment.Group, data = temp.df)$statistic
}

test.statistic <- round(test.statistic, 2)
test.statistic

Reject_H0 <- ifelse(abs(test.statistic) > qt(0.025, df = 58, lower.tail = FALSE), TRUE, FALSE)
Reject_H0

# Time-by-Time ANOVA with derived variable

weight_gain <- df[, 4:13] - df[, 3:12]
weight_gain

df.weight_gain <- cbind(df[, 1:2], weight_gain)

View(df.weight_gain)

df.weight_gain.melted <- df.weight_gain %>%
  pivot_longer(cols = 3:12,
               values_to = "Weight.Gain",
               names_to = "Time")

View(df.weight_gain.melted)

test.statistic.d <- c()

for (i in 2:11) {
  temp.df <- df.weight_gain.melted %>%
    filter(as.numeric(Time) == i)
  
  test.statistic.d[i-1] <- t.test(Weight.Gain ~ Treatment.Group, data = temp.df)$statistic
}

test.statistic.d <- round(test.statistic.d, digits = 2)
test.statistic.d

Reject_H0.d <- ifelse(abs(test.statistic.d) > qt(0.025, df = 58, lower.tail = FALSE), TRUE, FALSE)
Reject_H0.d
