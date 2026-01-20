n <- 361

angles <- seq(0, 2 * pi, length.out = n)

numbers <- runif(n)

x <- cos(angles) * numbers
y <- sin(angles) * numbers

plot(NA, NA, 
     xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = "", ylab = "")

points(x, y)
segments(0, 0, x, y)
