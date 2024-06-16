plant_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/multivariate_prac_q1_data.csv', stringsAsFactors = TRUE)
dim(plant_data)
names(plant_data)
View(plant_data)
str(plant_data)
summary(plant_data)

plant_data_mlr <- lm(yield_of_dry_bark ~ height + girth, data = plant_data)
summary(plant_data_mlr)


height_sq <- plant_data$height^2
girth_sq <- plant_data$girth^2
yield_height <- plant_data$yield_of_dry_bark * plant_data$height
yield_girth <- plant_data$yield_of_dry_bark * plant_data$girth
height_girth <- plant_data$height * plant_data$girth

df1 <- data.frame(plant_data, height_sq, girth_sq, yield_height, yield_girth, height_girth)
View(df1)

totals <- sapply(df1, sum)
totals
means <- round(sapply(df1, mean), digits = 2)
means

cov_yield_height <- round(means["yield_height"] - means["yield_of_dry_bark"] * means["height"], digits = 2)
cov_yield_height

cov_yield_girth <- round(means["yield_girth"] - means["yield_of_dry_bark"] * means["girth"], digits = 2)
cov_yield_girth

cov_height_girth <- round(means["height_girth"] - means["height"] * means["girth"], digits = 2)
cov_height_girth

var_height <- round(means["height_sq"] - means["height"]^2, digits = 2)
var_height

var_girth <- round(means["girth_sq"] - means["girth"]^2, digits = 2)
var_girth

sigma <- matrix(data = c(var(plant_data$yield_of_dry_bark)*(17/18), cov_yield_height, cov_yield_girth, cov_yield_height, var_height, cov_height_girth, cov_yield_girth, cov_height_girth, var_girth), nrow = 3, ncol = 3, byrow = TRUE)
sigma

library(matlib)

beta_height <- round(-cofactor(sigma, 1, 2) / cofactor(sigma, 1, 1), digits = 2)
beta_height

beta_girth <- round(-cofactor(sigma, 1, 3) / cofactor(sigma, 1, 1), digits = 2)
beta_girth

alpha <- mean(plant_data$yield_of_dry_bark) - beta_height * mean(plant_data$height) - beta_girth * mean(plant_data$girth)
alpha
