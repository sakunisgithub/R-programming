mass_points <- 0:10

probabilities <- dpois(mass_points, lambda = 2)

cumulative_probabilities <- ppois(mass_points, lambda = 2)

plot(probabilities ~ mass_points, 
     col = "red", 
     pch = 19)

plot(cumulative_probabilities ~ mass_points, 
     col = "blue", 
     pch = 19, 
     type = "o", 
     lwd = 2)
