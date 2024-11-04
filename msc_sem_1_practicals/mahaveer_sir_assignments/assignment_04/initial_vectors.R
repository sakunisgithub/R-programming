n <- 100

angles <- seq(0, 2 * pi, length.out = n)

x <- cos(angles)
y <- sin(angles)

par(bg = "black")
plot(NA, NA, xlim = c(-1, 1), ylim = c(-1, 1))

points(x, y, type = "p", asp = 1, xlab = "X", ylab = "Y", main = "Points on a Circle", col = "green", pch = 19)
segments(0, 0, x, y, col = "red", lwd = 2)
abline(h = 0, col = "#f5fc02")
abline(v = 0, col = "#f5fc02")
