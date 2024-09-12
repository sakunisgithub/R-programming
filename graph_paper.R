par(bg = '#e3f8d4')

plot(0, 0, 
     xlim = c(0, 10), 
     ylim = c(0, 10),
     type = "n",
     xlab = "x-axis",
     ylab = "y-axis")

abline(h = seq(0, 10, 0.2), v = seq(0, 10, 0.2))
abline(h = seq(0, 10, 1), v = seq(0, 10, 1), col = "limegreen", lwd = 2)
