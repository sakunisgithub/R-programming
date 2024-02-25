tractor_maintainance_cost_data <- read.csv("D:\\data_sets\\cc12_prac_q14_data.csv")
names(tractor_maintainance_cost_data)
xy <- tractor_maintainance_cost_data$years * tractor_maintainance_cost_data$cost
xy
x_sq <- tractor_maintainance_cost_data$years^2
x_sq
y_sq <- tractor_maintainance_cost_data$cost^2
y_sq
sum(tractor_maintainance_cost_data$years)
sum(tractor_maintainance_cost_data$cost)
sum(xy)
sum(x_sq)
sum(y_sq)
x_bar <- round(mean(tractor_maintainance_cost_data$years), digits = 2)
x_bar
y_bar <- round(mean(tractor_maintainance_cost_data$cost), digits = 2)
y_bar
beta_hat <- (sum(xy) - 17 * x_bar * y_bar) / (sum(x_sq) - 17 * x_bar^2)
beta_hat
alpha_hat <- y_bar - beta_hat * x_bar
alpha_hat
fit1 <- lm(cost ~ years, data = tractor_maintainance_cost_data)
summary(fit1)
fit1$fitted.values
round((fit1$residuals)^2, digits = 2)
sum(round(fit1$residuals^2), digits = 2)
tractor_maintainance_cost_data$years <- as.factor(tractor_maintainance_cost_data$years)
levels(tractor_maintainance_cost_data$years)
df2 <- tractor_maintainance_cost_data %>%
  group_by(years) %>%
  summarise(average = mean(cost))
df2
df2$average
n <- table(tractor_maintainance_cost_data$years)
n
n * df2$average^2
sum(n * df2$average^2)
sum(y_sq) - sum(n * df2$average^2)
13302826-12616970
2*(1203912-685856)/685856