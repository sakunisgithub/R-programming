# data simulation
gender_simulator <- function(){
  
  genders <- c()
  
  i <- 1
  
  repeat{
    
    g <- sample(c("M", "F"), 1)
    genders <- append(genders, g)
    
    if(g == "M" || i >= 5) break
    
    i <- i + 1
  }
  
  genders
  
  num <- length(genders)
  
  num_male <- length(which(genders == "M"))
  
  num_female <- length(which(genders == "F"))
  
  genders <- paste(genders, collapse = "-")
  
  l <- list(num, genders, num_male, num_female)
  
  return(l)
}

number_of_children <- c()

sex <- c()

n_male <- c()

n_female <- c()

for (i in 1:10000) {
  temp <- gender_simulator()
  
  number_of_children[i] <- temp[[1]]
  
  sex[i] <- temp[[2]]
  
  n_male[i] <- temp[[3]]
  
  n_female[i] <- temp[[4]]
  
}

child_df <- data.frame(family_id = 1:10000,
                       num_child = number_of_children,
                       child_sex = sex,
                       num_male = n_male,
                       num_female = n_female)

View(child_df)

# descriptive analysis

# average number of children
mean(child_df$num_child)

library(tidyverse)

# frequency distribution of number of children

child_df %>%
  ggplot(aes(x = as.factor(num_child))) +
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

total_male <- sum(child_df$num_male)
total_female <- sum(child_df$num_female)

sex_df <- data.frame(sex = c("Male", "Female"), 
                     count = c(total_male, total_female))

View(sex_df)

sex_df %>%
  ggplot(aes(x = as.factor(sex), y = count)) +
  geom_col(fill = c("#068ee1", "#f87ac8")) +
  geom_text(aes(label = count),
            vjust = -0.5,
            size = 4) +
  labs(x = "Sex of Children", 
       y = "Frequency",
       title = "Sex Distribution of Children")
