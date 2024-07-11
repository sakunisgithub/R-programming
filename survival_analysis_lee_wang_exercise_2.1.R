survival_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/survival_analysis_lee_wang_exercise_2.1.csv", stringsAsFactors = TRUE)

names(survival_data)

View(survival_data)

total_number_of_patients <- survival_data$number_alive_at_the_beginning_of_the_interval[1]

S_t_hat <- survival_data$number_alive_at_the_beginning_of_the_interval / total_number_of_patients 

interval_width <- 1

f_t_hat <- survival_data$number_dying_in_the_interval / (total_number_of_patients * interval_width) 

f_t_hat[length(f_t_hat)] = NA

number_of_patients_dying_per_unit_time_in_the_interval <- survival_data$number_dying_in_the_interval / interval_width

h_t_hat_method_1 <- number_of_patients_dying_per_unit_time_in_the_interval / ( survival_data$number_alive_at_the_beginning_of_the_interval * interval_width)

h_t_hat_method_1[length(h_t_hat_method_1)] = NA

h_t_hat_method_2 <- number_of_patients_dying_per_unit_time_in_the_interval / (survival_data$number_alive_at_the_beginning_of_the_interval - survival_data$number_dying_in_the_interval / 2)

h_t_hat_method_2[length(h_t_hat_method_2)] = NA

analysis_table <- data.frame(survival_data, 
                             t = 0:9,
                             S_t_hat = round(S_t_hat, digits = 3), 
                             f_t_hat = round(f_t_hat, digits = 3), 
                             h_t_hat_method_1 = round(h_t_hat_method_1, digits = 3),
                             h_t_hat_method_2 = round(h_t_hat_method_2, digits = 3))

View(analysis_table)

library(tidyverse)

analysis_table %>%
  ggplot(aes(x = t, y = S_t_hat)) +
  geom_step(direction = "hv", linewidth = 1, color = "blue") +
  geom_point(size = 2, col = "red", shape = 4, stroke = 2) +
  scale_x_discrete(limits = 0:9) +
  labs(x = "t(years)", y = "Estimated Survival Function", title = "Plot of Estimated Survival Function")

analysis_table %>%
  ggplot(aes(x = t, y = f_t_hat)) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(size = 2, col = "red", shape = 4, stroke = 2) +
  scale_x_discrete(limits = 0:9) +
  labs(x = "t(years)", y = "Estimated Desnity Function", title = "Plot of Estimated Density Function")

analysis_table %>%
  ggplot(aes(x = t, y = h_t_hat_method_2)) +
  geom_line(linewidth = 1, col = "blue") +
  geom_point(size = 2, col = "red", shape = 4, stroke = 1.5) +
  scale_size_discrete(limits = 0:9) +
  labs(x = "t(years)", y = "Estimated Hazard Function", title = "Plot of Estimated Hazard Function")
