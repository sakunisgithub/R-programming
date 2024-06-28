values <- c(0.414, 0.523, 0.229, 0.942, 0.097, 0.394, 0.572, 0.486, 0.273, 0.358)

sorted_values <- sort(values)

emperical_CDF <- (1:length(values))/length(values)

CDF <- sorted_values

df <- data.frame(x = sorted_values, 
                 F_x = CDF, 
                 S_x = emperical_CDF, 
                 abs_diff = abs(emperical_CDF - CDF))
View(df)

max(df$abs_diff)
df$x[which(df$abs_diff == max(df$abs_diff))]
