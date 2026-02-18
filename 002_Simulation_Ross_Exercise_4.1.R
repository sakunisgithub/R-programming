p <- c(0.3, 0.2, 0.35, 0.15)

n <- 10000

u <- runif(n)

x <- findInterval(u, cumsum(p))

df <- data.frame(x = 1:4,
                 theoretical.probability = p,
                 empirical.probability = as.numeric(table(x)) / n)

View(df)

library(tidyverse)

df_melted <- df %>%
                pivot_longer(cols = c("theoretical.probability", "empirical.probability"),
                             names_to = "probability_type",
                             values_to = "probability")

View(df_melted)

df_melted %>%
  ggplot(aes(x = x, y = probability, fill = probability_type)) +
  geom_col(position = "dodge") +
  theme(legend.position = "top",
        legend.title = element_blank())
