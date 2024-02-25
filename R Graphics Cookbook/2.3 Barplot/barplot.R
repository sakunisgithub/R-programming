BOD
# using Base R
barplot(BOD$demand, names.arg = BOD$Time)
# using ggplot
BOD %>%
  ggplot(aes(x = Time, y = demand)) +
  geom_col()

BOD %>%
  ggplot(aes(x = factor(Time), y = demand)) +
  geom_col()


mtcars
# using Base R
table(mtcars$cyl)
barplot(table(mtcars$cyl))
# using ggplot
mtcars %>%
  ggplot(aes(x = cyl)) +
  geom_bar()

## See that, while using geom_col(), we have to explicitly use factor() function. When we use geom_bar(), we do not have to use that.