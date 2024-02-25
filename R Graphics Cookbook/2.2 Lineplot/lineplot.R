# using Base R
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure/2, col = "red")
lines(pressure$temperature, pressure$pressure/2, col = "red")

# using ggplot
pressure %>%
  ggplot(aes(x = temperature, y = pressure)) +
  geom_point(color = "blue", size = 2) +
  geom_line(color = 'red', linewidth = 1)
