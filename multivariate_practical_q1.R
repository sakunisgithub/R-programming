plant_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/multivariate_prac_q1_data.csv', stringsAsFactors = TRUE)
dim(plant_data)
names(plant_data)
View(plant_data)
str(plant_data)
summary(plant_data)

plant_data_mlr <- lm(yield_of_dry_bark ~ height + girth, data = plant_data)
summary(plant_data_mlr)

yield_sq <- plant_data$yield_of_dry_bark^2
height_sq <- plant_data$height^2
girth_sq <- plant_data$girth^2
yield_height <- plant_data$yield_of_dry_bark * plant_data$height
yield_girth <- plant_data$yield_of_dry_bark * plant_data$girth
height_girth <- plant_data$height * plant_data$girth

df1 <- data.frame(plant_data, yield_sq, height_sq, girth_sq, yield_height, yield_girth, height_girth)
View(df1)

totals <- sapply(df1, sum)
totals
means <- round(sapply(df1, mean), digits = 2)
means

cov_yield_height <- round(means["yield_height"] - means["yield_of_dry_bark"] * means["height"], digits = 2)
names(cov_yield_height) <- NULL
cov_yield_height

cov_yield_girth <- round(means["yield_girth"] - means["yield_of_dry_bark"] * means["girth"], digits = 2)
names(cov_yield_girth) <- NULL
cov_yield_girth

cov_height_girth <- round(means["height_girth"] - means["height"] * means["girth"], digits = 2)
names(cov_height_girth) <- NULL
cov_height_girth

var_yield <- round(means["yield_sq"] - means["yield_of_dry_bark"]^2, digits = 2)
names(var_yield) <- NULL
var_yield

var_height <- round(means["height_sq"] - means["height"]^2, digits = 2)
names(var_height) <- NULL
var_height

var_girth <- round(means["girth_sq"] - means["girth"]^2, digits = 2)
names(var_girth) <- NULL
var_girth


sigma <- matrix(data = c(var_yield, cov_yield_height, cov_yield_girth, cov_yield_height, var_height, cov_height_girth, cov_yield_girth, cov_height_girth, var_girth), nrow = 3, ncol = 3, byrow = TRUE)
sigma

library(matlib)

beta_height <- round(-cofactor(sigma, 1, 2) / cofactor(sigma, 1, 1), digits = 2)
beta_height

beta_girth <- round(-cofactor(sigma, 1, 3) / cofactor(sigma, 1, 1), digits = 2)
beta_girth

alpha <- mean(plant_data$yield_of_dry_bark) - beta_height * mean(plant_data$height) - beta_girth * mean(plant_data$girth)
alpha

temp <- det(sigma) / (sigma[1,1] * det(sigma[2:3, 2:3]))
r_1_dot_23 <- sqrt(1 - temp)
r_1_dot_23

r_12_dot_3 <- -cofactor(sigma, 1, 2) / (sqrt(cofactor(sigma, 1, 1)) * sqrt(cofactor(sigma, 2, 2)))
r_12_dot_3

r_13_dot_2 <- -cofactor(sigma, 1, 3) / (sqrt(cofactor(sigma, 1, 1)) * sqrt(cofactor(sigma, 3, 3)))
r_13_dot_2

library(ppcor)

pcor(plant_data) # $estimate gives the partial correlations
