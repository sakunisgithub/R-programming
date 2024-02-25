election_data <- read.csv("seats_won_in_general_election.csv")
election_data
str(election_data)
election_data$year <- factor(election_data$year)
library(tidyverse)
election_data %>%
  ggplot(aes(x = year, y = seats_won, fill = party)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#FC8E03", "#05BC05")) +
  labs(x = "Year", y = "Seats Won", title = "Seats won in General Elections")