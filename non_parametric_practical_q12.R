blood_pressure_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/non_parametric_prac_q12_data.csv', stringsAsFactors = TRUE)
dim(blood_pressure_data)
names(blood_pressure_data)

deviations <- blood_pressure_data$Before - blood_pressure_data$After

absolute_deviations <- abs(deviations)

sorted_absolute_deviations <- sort(absolute_deviations)
sorted_absolute_deviations <- sorted_absolute_deviations[-which(sorted_absolute_deviations == 0)]
sorted_absolute_deviations
ranks_of_uniques <- c()

for (i in 1:length(unique(sorted_absolute_deviations))) {
  ranks_of_uniques[i] <- mean(which(sorted_absolute_deviations == unique(sorted_absolute_deviations)[i]))
}

ranks <- c()

for (i in 1:length(deviations)) {
  if(deviations[i] != 0){
    ranks[i] <- ranks_of_uniques[which(unique(sorted_absolute_deviations) == abs(deviations[i]))]
  }
  else{
    ranks[i] <- NA
  }
}

df <- data.frame(blood_pressure_data, deviations = deviations, absolute_deviations = absolute_deviations, ranks = ranks)
View(df)

T_minus <- sum(df$ranks[which(df$deviations < 0)])
T_minus
