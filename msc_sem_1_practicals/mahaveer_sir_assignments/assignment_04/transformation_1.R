n <- 100

angles <- seq(0, 2 * pi, length.out = n)

x <- cos(angles)
y <- sin(angles)

par(bg = "black")
plot(NA, NA, xlim = c(-2, 2), ylim = c(-2, 2))

points(x, y, col = "green", pch = 19)
segments(0, 0, x, y, col = "red", lwd = 2)
abline(h = 0, col = "#f5fc02")
abline(v = 0, col = "#f5fc02")

a <- matrix(c(x, y), nrow = 2, byrow = TRUE)

trans <- matrix(c(1, 0, 0, 2), nrow = 2, byrow = TRUE)

b <- trans %*% a

pause <- function(seconds){
  start <- Sys.time()
  while((Sys.time() - start) < seconds){}
}

for (i in 1:100) {
  points(x[i], y[i], col = "black", pch = 19)
  segments(0, 0, x[i], y[i], col = "black", lwd = 2)
  
  points(b[1, i], b[2, i], col = "green", pch = 19)
  points(b[1, 1:i], b[2, 1:i], col = "green", pch = 19)
  
  segments(0, 0, b[1, i], b[2, i], col = "red", lwd = 2)
  segments(0, 0, b[1, 1:i], b[2, 1:i], col = "red", lwd = 2)
  
  abline(h = 0, col = "#f5fc02")
  abline(v = 0, col = "#f5fc02")
  
  pause(0.25)
}
