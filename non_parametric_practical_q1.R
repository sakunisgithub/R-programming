bulb_efficiencies <- c(9.29, 10.15, 8.69, 11.25, 11.47, 9.76, 12.05, 12.38, 9.08, 10.25, 8.93, 9.02, 10.87, 10.00, 11.56)

length(which(bulb_efficiencies > 9.03))

length(bulb_efficiencies)

pbinom(q = 11, size = 15, prob = 0.5, lower.tail = FALSE)

binom.test(x = 12, n = 15, p = 0.5, alternative = "greater")
