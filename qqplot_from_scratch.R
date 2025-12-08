s1 <- rnorm(25, 0, 1); s1 <- sort(s1)

prob_points <- seq(0, 1, length.out = 27)
prob_points <- prob_points[-c(1, length(prob_points))]
prob_points

normal_25 <- qnorm(prob_points, 0, 1, lower.tail = TRUE)
normal_25

plot(s1 ~ normal_25)
abline(a = 0, b = 1)


s2 <- rnorm(25, 1, 2); s2 <- sort(s2)

prob_points <- seq(0, 1, length.out = 27)
prob_points <- prob_points[-c(1, length(prob_points))]
prob_points

normal_25 <- qnorm(prob_points, 0, 1, lower.tail = TRUE)
normal_25

plot(s2 ~ normal_25)
abline(a = 0, b = 1, col = 'red')
abline(b = IQR(s2)/IQR(normal_25),
       a = quantile(s2, 0.25) - (IQR(s2)/IQR(normal_25)) * quantile(normal_25, 0.25), col = 'green')


qqnorm(s2); qqline(s2)




s3 <- rexp(25, rate = 1); s3 <- sort(s3)

exp_25 <- qexp(prob_points, rate = 1, lower.tail = TRUE)

plot(s3 ~ exp_25)
abline(b = IQR(s3)/IQR(exp_25),
       a = quantile(s3, 0.25) - (IQR(s3)/IQR(exp_25)) * quantile(exp_25, 0.25), col = 'green')
