survival_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/survival_analysis_lee_wang_exercise_2.2.csv')
dim(survival_data)
names(survival_data)

total_patients <- survival_data$number_living_at_beginning[1]

S_t_hat <- round(survival_data$number_living_at_beginning / total_patients, digits = 4)

interval_width <- c(1, 4, rep(5, 16), NA)

f_t_hat <- round(survival_data$number_dying_in_interval / (total_patients * interval_width), digits = 5)

numerator <- survival_data$number_dying_in_interval / interval_width
denominator <- survival_data$number_living_at_beginning - survival_data$number_dying_in_interval / 2

h_t_hat <- round(numerator / denominator, digits = 5)

df <- data.frame(survival_data, 
                 interval_width = interval_width,
                 S_t_hat = S_t_hat,
                 f_t_hat = f_t_hat,
                 h_t_hat = h_t_hat)
View(df)
