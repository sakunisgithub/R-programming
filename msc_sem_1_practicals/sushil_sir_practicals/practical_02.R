View(airquality)

dim(airquality)

names(airquality)

library(tidyverse)

# Q1
airquality %>%
  drop_na() %>%
    ggplot(aes(x = Wind, y = Ozone)) +
    geom_point(col = "blue", shape = 5) +
    labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Scatterplot of Ozone vs Wind")

airquality %>%
  drop_na() %>%
    ggplot(aes(x = Wind, y = Ozone)) +
    geom_point(col = "blue", shape = 12) +
    labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Scatterplot of Ozone vs Wind")

airquality %>%
  drop_na() %>%
    ggplot(aes(x = Wind, y = Ozone)) +
    geom_point(col = "blue", shape = 21) +
    labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Scatterplot of Ozone vs Wind")

# Q2
airquality %>%
  drop_na() %>%
    ggplot(aes(x = Wind, y = Ozone)) +
    geom_point(col = "darkgreen", shape = 17, size = 1.3) +
    labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Customized Scatterplot of Ozone vs Wind") +
    coord_cartesian(xlim = c(0, 20), ylim = c(0, 200))


# Q3
df1 <- airquality %>%
        drop_na()

View(df1)

for (i in 1:(length(df1$Wind)-1)) {
  for (j in (i+1):length(df1$Wind)) {
    if(df1[i, 3] > df1[j, 3]){
      temp <- df1[i, ]
      df1[i, ] <- df1[j, ]
      df1[j, ] <- temp
    }
  }
}

View(df1)

df1 %>%
  ggplot(aes(x = Wind, y = Ozone)) +
  geom_line(lty = 1, linewidth = 2, col = "blue") +
  labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Line Plot of Ozone vs Wind with Multiple Line Types")

df1 %>%
  ggplot(aes(x = Wind, y = Ozone)) +
  geom_line(lty = 2, linewidth = 2, col = "blue") +
  labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Line Plot of Ozone vs Wind with Multiple Line Types")

df1 %>%
  ggplot(aes(x = Wind, y = Ozone)) +
  geom_line(lty = 3, linewidth = 2, col = "blue") +
  labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Line Plot of Ozone vs Wind with Multiple Line Types")

df1 %>%
  ggplot(aes(x = Wind, y = Ozone)) +
  geom_line(lty = 4, linewidth = 2, col = "blue") +
  labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Line Plot of Ozone vs Wind with Multiple Line Types")

df1 %>%
  ggplot(aes(x = Wind, y = Ozone)) +
  geom_line(lty = 5, linewidth = 2, col = "blue") +
  labs(x = "Wind (mph)", y = "Ozone (ppb)", title = "Line Plot of Ozone vs Wind with Multiple Line Types")

# Q4
airquality %>%
  drop_na() %>%
    ggplot(aes(x = Temp, y = Solar.R)) +
    geom_point(col = "red", shape = 16) +
    labs(x = "Temperature (F)", y = "Solar Radiation (lang)", title = "Scatter Plot of Temperature vs Solar Radiation")
