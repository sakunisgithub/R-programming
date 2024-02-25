student_scores <- read.csv("D:\\data_sets\\cc12_prac_q6_data.csv")
dim(student_scores)
names(student_scores)
fit1 <- lm(marks_in_university ~ marks_in_college, data = student_scores)
summary(fit1)
x_diff <- student_scores$marks_in_college - mean(student_scores$marks_in_college)
x_diff
x_diff_sq <- round(x_diff^2, digits = 2)
x_diff_sq
sxx <- sum(x_diff_sq)
sxx
y_diff <- student_scores$marks_in_university - mean(student_scores$marks_in_university)
x_diff_y_diff <- x_diff * y_diff
x_diff_y_diff
sxy <- sum(x_diff_y_diff)
sxy
sum(student_scores$marks_in_college)
sum(student_scores$marks_in_university)
x_bar <- mean(student_scores$marks_in_college)
x_bar
y_bar <- mean(student_scores$marks_in_university)
y_bar
beta_hat <- sxy / sxx
beta_hat
alpha_hat <- y_bar - beta_hat * x_bar
alpha_hat