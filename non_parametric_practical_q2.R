ear_head_measurements <- c(9.3, 8.8, 10.7, 11.5, 8.2, 9.7, 10.3, 8.6, 11.3, 10.7, 11.2, 9.0, 9.8, 9.3, 9.9, 10.3, 10, 10.1, 9.6, 10.4)

deviations <- ear_head_measurements - 9.9
deviations

absolute_deviations <- abs(deviations)
absolute_deviations

sorted_abs_deviations <- sort(absolute_deviations)
sorted_abs_deviations

sorted_abs_deviations <- sorted_abs_deviations[-1]

sorted_abs_deviations <- round(sorted_abs_deviations, digits = 1) # must do it

rank_of_uniques <- c()

for (i in 1:length(unique(sorted_abs_deviations))) {
  rank_of_uniques[i] <- mean( which( sorted_abs_deviations == unique(sorted_abs_deviations)[i] ) )
}

rank_of_uniques

ranks <- c()

for (i in 1:length(deviations)) {
  if(deviations[i] != 0){
    temp <- which( unique(sorted_abs_deviations) == round(absolute_deviations[i], digits = 1) )
    ranks[i] <- rank_of_uniques[temp]
  }
  else{
    ranks[i] <- NA
  }
}

ranks

df1 <- data.frame(ear_head_measurements, deviations, absolute_deviations, ranks)
View(df1)

T_plus <- sum(df1$ranks[which(df1$deviations > 0)])
T_plus

T_minus <- sum(df1$ranks[which(df1$deviations < 0)])
T_minus

observed_T <- min(T_plus, T_minus)
observed_T
