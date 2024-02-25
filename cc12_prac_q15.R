cotton_yield_data <- read.csv("D:\\data_sets\\cc12_prac_q15_data.csv")
dim(cotton_yield_data)
names(cotton_yield_data)
cotton_yield_data
a <- aov(yield_of_cotton ~ nitrogen_levels + replicates, data = cotton_yield_data)
summary(a)
b <- lm(yield_of_cotton ~ nitrogen_levels + replicates + number_of_plants, data = cotton_yield_data)
summary(b)
sum(b$residuals^2)
df1 <- cotton_yield_data %>%
  group_by(nitrogen_levels) %>%
  summarise(x = sum(number_of_plants), y = sum(yield_of_cotton))
View(df1)
df2 <- cotton_yield_data %>%
  group_by(replicates) %>%
  summarise(x = sum(number_of_plants), y = sum(yield_of_cotton))
View(df2)
sum(cotton_yield_data$number_of_plants)
sum(cotton_yield_data$yield_of_cotton)
(574*397)/20
sum(df1$x * df1$y) / 4 - 11393.9
sum(df2$x * df2$y) / 5 - 11393.9
sum(cotton_yield_data$number_of_plants * cotton_yield_data$yield_of_cotton) - 11393.9
310.1-78.725-10.3
574^2/20
sum(df1$x^2) / 4 - 16473.8
sum(df2$x^2) / 5 - 16473.8
sum(cotton_yield_data$number_of_plants^2) - 16473.8
612.2-17.2-67.4
221.075/527.6
397^2/20
sum(df1$y^2) / 4 - 7880.45
sum(df2$y^2) / 5 - 7880.45
sum(cotton_yield_data$yield_of_cotton^2) - 7880.45
772.05-624.3-24.25
123.5-221.075^2/527.6
(sqrt(527.6)*0.419)/sqrt(30.86513/11)
17.2+527.6
624.3+123.5
78.725+221.075
747.8-299.8^2/544.8
((582.822-30.86513)/30.86513)*(11/4)
