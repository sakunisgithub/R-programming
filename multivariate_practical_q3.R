sigma <- matrix(c(0.12692, 0.03030, 0.04410, 0.030629,
                  0.03030, 0.01875, 0.00848, 0.00684,
                  0.04410, 0.00848, 0.02904, 0.00878,
                  0.030629, 0.00684, 0.00878, 0.02886), byrow = TRUE, nrow = 4, ncol = 4)
sigma

means <- matrix(c(3.1685, 2.2752, 2.1523, 2.1128), nrow = 4, ncol = 1)
means

library(matlib)

sigma_11 <- cofactor(sigma, 1, 1)

beta_2 <- -cofactor(sigma, 1, 2) / sigma_11
beta_2 <- round(beta_2, digits = 4)
beta_2

beta_3 <- -cofactor(sigma, 1, 3) / sigma_11
beta_3 <- round(beta_3, digits = 4)
beta_3

beta_4 <- -cofactor(sigma, 1, 4) / sigma_11
beta_4 <- round(beta_4, digits = 4)
beta_4

beta_curl <- matrix(c(beta_2, beta_3, beta_4), nrow = 3, ncol = 1)
beta_curl

alpha <- means[1] - t(beta_curl) %*% means[2:4,]
alpha <- round(alpha, digits = 4)
alpha

temp <- det(sigma) / (sigma[1, 1] * det(sigma[2:4, 2:4]))
r_1_dot_234 <- sqrt(1 - temp)
r_1_dot_234

r_12_dot_34 <- beta_2 * sqrt(cofactor(sigma, 1, 1) / cofactor(sigma, 2, 2))
r_12_dot_34

r_13_dot_24 <- beta_3 * sqrt(cofactor(sigma, 1, 1) / cofactor(sigma, 3, 3))
r_13_dot_24

r_14_dot_23 <- beta_4 * sqrt(cofactor(sigma, 1, 1) / cofactor(sigma, 4, 4))
r_14_dot_23
