w <- rnorm(50)
y <- rnorm(50)
z <- rnorm(50)

x <- 2 * w - 3 * z + rnorm(50) / 5

dat <- data.frame(w, x, y, z)
plot(dat)

fit <- lm(x ~ ., data = dat) # regress x on everything else
summary(fit)

library(rgl)
plot3d(x, w, z)
