ge_2019_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/performance_of_national_parties_in_general_election_2019.csv", stringsAsFactors = TRUE)

names(ge_2019_data)

View(ge_2019_data)

library(tidyverse)

ge_2019_data %>%
  ggplot(aes(x = "", y = number_of_votes_secured, fill = name_of_the_party)) +
  geom_col(color = "black") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("green", "orange", "grey", "red", "pink", "blue", "yellow")) +
  theme_void()
