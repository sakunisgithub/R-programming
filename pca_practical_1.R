sigma <- matrix(data = c(4.735, 0.562, 1.469,
                         0.562, 0.142, 0.217,
                         1.469, 0.217, 0.570), nrow = 3, ncol = 3, byrow = TRUE)

y0 <- matrix(data = rep(1, 3), nrow = 3, ncol = 1)

y1 <- (sigma %*% y0) / as.numeric(sqrt(t(y0) %*% y0))
y1 <- round(y1, digits = 4)
y1

y2 <- (sigma %*% y1) / as.numeric(sqrt(t(y1) %*% y1))
y2 <- round(y2, digits = 4)
y2

y3 <- (sigma %*% y2) / as.numeric(sqrt(t(y2) %*% y2))
y3 <- round(y3, digits = 4)
y3

beta_1 <- y3 / as.numeric(sqrt(t(y3) %*% y3))
beta_1 <- round(beta_1, digits = 4)
beta_1

lambda_1 <- as.numeric(sqrt(t(y3) %*% y3))
lambda_1

trace_sigma <- sum(diag(sigma))
trace_sigma

pc1_v <- lambda_1 / trace_sigma
pc1_v
