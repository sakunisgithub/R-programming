# data simulation

number_of_children <- sample(1:5, 10000, replace = TRUE)

n_male <- c()

n_female <- c()

gender <- c()

for (i in 1:10000) {
  
  temp <- c()
  
  if(number_of_children[i] == 1){
    temp <- rbinom(1, 1, 0.5)
  } else if(number_of_children[i] == 2){
    temp <- rbinom(2, 1, 0.5)
  } else if(number_of_children[i] == 3){
    temp <- rbinom(3, 1, 0.5)
  } else if(number_of_children[i] == 4){
    temp <- rbinom(4, 1, 0.5)
  } else{
    temp <- rbinom(5, 1, 0.5)
  }
  
  n_male[i] <- sum(temp)
  n_female[i] <- length(temp) - sum(temp)
  
  gender_temp <- ifelse(temp == 1, "M", "F")
  
  gender[i] <- paste(gender_temp, collapse = "-")
}

family_df <- data.frame(family_no = 1:10000, 
                        num_children = number_of_children,
                        sex = gender,
                        male =  n_male,
                        female = n_female)

View(family_df)


# descriptive analysis

library(tidyverse)

# frequency distribution of number of children

family_df %>%
  ggplot(aes(x = as.factor(num_children))) +
  geom_bar(fill = "#0c9cf3", 
           col = "black") +
  geom_text(stat = "count",
            aes(label = after_stat(count)), 
            vjust = -0.5,
            size = 4) + 
  labs(x = "Number of Children", 
       y = "Number of Families", 
       title = "Frequency Distribution of Number of Children")

# gender distribution

total_male <- sum(family_df$male)
total_female <- sum(family_df$female)

sex_df <- data.frame(sex = c("Male", "Female"), 
                     count = c(total_male, total_female))

View(sex_df)

sex_df %>%
  ggplot(aes(x = as.factor(sex), y = count)) +
  geom_col(fill = c("#068ee1", "#f87ac8"),
           col = "black") +
  geom_text(aes(label = count),
            vjust = -0.5,
            size = 3) +
  labs(x = "Sex of Children", 
       y = "Frequency", 
       title = "Sex Distribution of Children")
