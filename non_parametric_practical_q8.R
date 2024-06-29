values <- c(2.240, -0.400, -1.152, 0.980, 0.361, -0.123, -0.625, 0.682, 2.323, -1.053, -0.870, -0.164, -0.340, -0.041, 1.405, 1.187, 0.323, 0.270, -0.128, 0.101)

sorted_values <- sort(values)

emperical_CDF <- (1:length(values))/length(values)

CDF <- round(pnorm(sorted_values), digits = 4)

df <- data.frame(x = sorted_values, 
                 F_x = CDF, 
                 S_x = emperical_CDF, 
                 abs_diff = abs(emperical_CDF - CDF))
View(df)

max(df$abs_diff)
df$x[which(df$abs_diff == max(df$abs_diff))]
