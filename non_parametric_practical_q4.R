marks_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/non_parametric_prac_q4_data.csv', stringsAsFactors = TRUE)
dim(marks_data)
names(marks_data)

# paired sign test
Z <- marks_data$college_test_marks - marks_data$public_examination_marks
Z

S <- length(which(Z > 0))
S

pbinom(S, 20, 0.5)

binom.test(x = S, n = 20, alternative = "less")


# wilcoxon sign rank test
deviations <- Z

absolute_deviations <- abs(deviations)

sorted_absolute_deviations <- sort(absolute_deviations)

ranks_of_uniques <- c()

for (i in 1:length(unique(sorted_absolute_deviations))) {
  ranks_of_uniques[i] <- mean(which(sorted_absolute_deviations == unique(sorted_absolute_deviations[i])))
}

ranks <- c()

for (i in 1:length(deviations)) {
  ranks[i] <- ranks_of_uniques[which(unique(sorted_absolute_deviations) == abs(deviations[i]))]
}

df <- data.frame(marks_data, deviations = deviations, absolute_deviations = absolute_deviations, ranks = ranks)
View(df)

T_plus <- sum(df$ranks[which(df$deviations > 0)])
T_plus

T_minus <- sum(df$ranks[which(df$deviations < 0)])
T_minus

observed_T <- min(T_plus, T_minus)
observed_T
