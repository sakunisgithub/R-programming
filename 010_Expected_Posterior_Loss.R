# a_1
a <- integrate(f = function(x) (x - 90) * dnorm(x, mean = 110.38, sd = sqrt(69.23)), lower = 90, upper = 110)

b <- integrate(f = function(x) 2 * (x - 90) * dnorm(x, mean = 110.38, sd = sqrt(69.23)), lower = 110, upper = Inf)

a$value + b$value


# a_2
a <- integrate(f = function(x) (90 - x) * dnorm(x, mean = 110.38, sd = sqrt(69.23)), lower = -Inf, upper = 90)

b <- integrate(f = function(x) (x - 110) * dnorm(x, mean = 110.38, sd = sqrt(69.23)), lower = 110, upper = Inf)

a$value + b$value


# a_3
a <- integrate(f = function(x) 2 * (110 - x) * dnorm(x, mean = 110.38, sd = sqrt(69.23)), lower = -Inf, upper = 90)

b <- integrate(f = function(x) (110 - x) * dnorm(x, mean = 110.38, sd = sqrt(69.23)), lower = 90, upper = 110)

a$value + b$value
