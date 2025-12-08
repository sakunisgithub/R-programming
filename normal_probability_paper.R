n <- 30

a <- seq(0, 1, length.out = n+2)
a <- a[-c(1, length(a))]

plot(NA,
     NA,
     xlim = c(min(qnorm(a)) - 0.05, max(qnorm(a)) + 0.05),
     ylim = c(min(qnorm(a)) - 0.05, max(qnorm(a)) + 0.05),
     xlab = "",
     ylab = "",
     yaxt = "n")

abline(h = qnorm(a),
       v = qnorm(a),
       col = 'green',
       lwd = 2)

axis(side = 2, 
     at = qnorm(a), 
     labels = paste0("p = ", round(a, 2)), 
     las = 1)
