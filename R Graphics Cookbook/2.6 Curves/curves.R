# using Base R
curve(exp(1)^x, from = -5, to = 5)

my_function <- function(temp){
  1 / (1 + exp(1)^temp)
}

curve(my_function(x), from = -5, to = 5)
curve(1 - my_function(x), from = -5, to = 5, add = TRUE, col = "red")

# using ggplot
new_function <- function(temp){
  1 - my_function(temp)
}
ggplot(data = data.frame(input = -5:5), aes(x = input)) +
  stat_function(fun = my_function, geom = "line", col = "red", linewidth = 1) +
  stat_function(fun = new_function, geom = "line", col = "green", linewidth = 1)

