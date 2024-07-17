R <- matrix(data = c(1, 0.8025, 0.8270, 0.0929, -0.1327,
                     0.8025, 1, 0.6605, -0.2876, -0.0236,
                     0.8270, 0.6605, 1, 0.1127, -0.0253,
                     0.0929, -0.2876, 0.1127, 1, 0.0789,
                     -0.1327, -0.0236, -0.0253, 0.0789, 1), nrow = 5, ncol = 5, byrow = TRUE)

R

library(matlib)

r_12_dot_345 <- -cofactor(R, 1, 2) / sqrt(cofactor(R, 1, 1) * cofactor(R, 2, 2))
r_12_dot_345

(r_12_dot_345 * sqrt(12 - 5)) / sqrt(1 - r_12_dot_345^2)
qt(0.025, 7, lower.tail = FALSE)


new_R <- R[-c(2,3), -c(2,3)]
new_R

r_1_dot_34 <- sqrt(1 - det(new_R) / det(new_R[2:3, 2:3]))
r_1_dot_34

(r_1_dot_34^2 / (3-1)) / ((1 - r_1_dot_34^2) / (12-3))
qf(0.05, 2, 9, lower.tail = FALSE)
