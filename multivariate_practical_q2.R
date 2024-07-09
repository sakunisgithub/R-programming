r_12 <- 0.578; r_13 <- 0.581; r_23 <- 0.974
r <- matrix(data = c(1, r_12, r_13, r_12, 1, r_23, r_13, r_23, 1), byrow = TRUE, nrow = 3, ncol = 3)
r

s1 <- 2.26; s2 <- 4.39; s3 <- 4.41

x1_bar <- 55.95; x2_bar <- 51.48; x3_bar <- 56.03

library(matlib)

b_1_hat <- (-1) * (s3/s1) * (cofactor(r, 3, 1) / cofactor(r, 3, 3))
b_1_hat <- round(b_1_hat, digits = 4)
b_1_hat

b_2_hat <- (-1) * (s3/s2) * (cofactor(r, 3, 2) / cofactor(r, 3, 3))
b_2_hat <- round(b_2_hat, digits = 4)
b_2_hat

alpha_hat <- x3_bar - b_1_hat * x1_bar - b_2_hat * x2_bar
alpha_hat <- round(alpha_hat, digits = 4)
alpha_hat

pred <- alpha_hat + b_1_hat * 58 + b_2_hat * 52.5
pred

r_3_dot_12 <- sqrt(1 - det(r) / det(r[1:2, 1:2]))
r_3_dot_12

r_23_dot_1 <- (-1) * cofactor(r, 2, 3) / (sqrt(cofactor(r, 2, 2)) * sqrt(cofactor(r, 3, 3)))
r_23_dot_1
