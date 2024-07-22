X <- matrix(data = c(7, 4, 3,
                     4, 1, 8,
                     6, 3, 5,
                     8, 6, 1,
                     8, 5, 7,
                     7, 2, 9,
                     5, 3, 3,
                     9, 5, 8,
                     7, 4, 5,
                     8, 2, 2), ncol = 3, byrow = TRUE)

options(digits = 4)

adjusted_X <- round(scale(X), digits = 4)
adjusted_X  

sigma_mat <- cov(adjusted_X)
sigma_mat

y0 <- matrix(data = rep(1, 3), ncol = 1, byrow = TRUE)

y1 <- (sigma_mat %*% y0) / as.numeric(sqrt(t(y0) %*% y0))
y1

y2 <- (sigma_mat %*% y1) / as.numeric(sqrt(t(y1) %*% y1))
y2

y3 <- (sigma_mat %*% y2) / as.numeric(sqrt(t(y2) %*% y2))
y3

y4 <- (sigma_mat %*% y3) / as.numeric(sqrt(t(y3) %*% y3))
y4

y5 <- (sigma_mat %*% y4) / as.numeric(sqrt(t(y4) %*% y4))
y5

beta_1 <- y5 / as.numeric(sqrt(t(y5) %*% y5))
beta_1

alpha_1 <- as.numeric(sqrt(t(y5) %*% y5))
alpha_1

sigma_mat_2 <- sigma_mat - alpha_1 * (beta_1 %*% t(beta_1))
sigma_mat_2

y1 <- (sigma_mat_2 %*% y0) / as.numeric(sqrt(t(y0) %*% y0))
y1

y2 <- (sigma_mat_2 %*% y1) / as.numeric(sqrt(t(y1) %*% y1))
y2

y3 <- (sigma_mat_2 %*% y2) / as.numeric(sqrt(t(y2) %*% y2))
y3

beta_2 <- y3 / as.numeric(sqrt(t(y3) %*% y3))
beta_2

alpha_2 <- as.numeric(sqrt(t(y3) %*% y3))
alpha_2

sigma_mat_3 <- sigma_mat - alpha_1 * (beta_1 %*% t(beta_1)) - alpha_2 * (beta_2 %*% t(beta_2))
sigma_mat_3

y1 <- (sigma_mat_3 %*% y0) / as.numeric(sqrt(t(y0) %*% y0))
y1

y2 <- (sigma_mat_3 %*% y1) / as.numeric(sqrt(t(y1) %*% y1))
y2

y3 <- (sigma_mat_3 %*% y2) / as.numeric(sqrt(t(y2) %*% y2))
y3

beta_3 <- y3 / as.numeric(sqrt(t(y3) %*% y3))
beta_3

alpha_3 <- as.numeric(sqrt(t(y3) %*% y3))
alpha_3

eigenvectors <- cbind(beta_1, beta_2, beta_3)
eigenvectors

new_variables <- adjusted_X %*% eigenvectors
new_variables <- round(new_variables, digits = 4)
new_variables

eigenvalues <- c(alpha_1, alpha_2, alpha_3)

for (i in 1:3) {
  for (j in 1:3) {
    if(i != j){
      print((eigenvectors[j, i] * sqrt(eigenvalues[i]))/sqrt(sigma_mat[j, j]))
    }
  }
}
