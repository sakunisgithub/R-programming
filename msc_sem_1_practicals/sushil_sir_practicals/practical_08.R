shopping_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/refs/heads/master/shopping_trends.csv', stringsAsFactors = TRUE)

attach(shopping_data)

dim(shopping_data)

names(shopping_data)

str(shopping_data)

table(Item.Purchased, Review.Rating)

# 1
age_group <- c()

for (i in 1:3900) {
  if(Age[i] <= 30){
    age_group[i] <- "adult"
  } else if(Age[i] <= 50){
    age_group[i] <- "midage"
  } else{
    age_group[i] <- "oldage"
  }
}

age_group

payment_method_vs_age <- as.data.frame(table(Payment.Method, age_group))

View(payment_method_vs_age)

library(tidyverse)

payment_method_vs_age %>%
  ggplot(aes(x = Payment.Method, y = Freq, fill = age_group)) +
  geom_col(position = "dodge") +
  theme(legend.position = "top")

# 2
summary(Size)
summary(Gender)

size_vs_gender <- as.data.frame(table(Size, Gender))

View(size_vs_gender)

size_vs_gender %>%
  ggplot(aes(x = Size, y = Freq, fill = Gender)) +
  geom_col(position = "dodge") +
  theme(legend.position = "top")

# 3
shopping_data %>%
  ggplot(aes(x = Gender, y = Purchase.Amount..USD.)) +
  stat_boxplot(geom = "errorbar", linewidth = 1) +
  geom_boxplot(fill = c("#fc90da", "#12acea"), linewidth = 1) +
  labs(title = "Amount Spent vs Gender")

# 5
review_rating_df <- shopping_data %>%
  group_by(Item.Purchased) %>%
  summarise(average_rating = mean(Review.Rating))

View(review_rating_df)

summary(review_rating_df$average_rating)
