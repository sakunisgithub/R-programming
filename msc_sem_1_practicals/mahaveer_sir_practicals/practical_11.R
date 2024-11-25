# data simulation
gender_simulator <- function(){
  
  genders <- c()
  
  probs_of_boy <- c(0.5, 0.7, 0.9)
  
  i <- 1
  
  repeat{
    
    p <- c(probs_of_boy[i], 1 - probs_of_boy[i])
    
    g <- sample(c("M", "F"), 1, prob = p)
    
    genders <- append(genders, g)
    
    if(g == "M" || i >= 3) break
    
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

dynamic_child_df <- data.frame(family_id = 1:10000,
                       num_child = number_of_children,
                       child_sex = sex,
                       num_male = n_male,
                       num_female = n_female)

View(dynamic_child_df)


# descriptive analysis

library(tidyverse)

# average number of children
mean(dynamic_child_df$num_child)

# frequency distribution of number of children

dynamic_child_df %>%
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

total_male <- sum(dynamic_child_df$num_male)
total_female <- sum(dynamic_child_df$num_female)

sex_df <- data.frame(sex = c("Male", "Female"), 
                     count = c(total_male, total_female))

View(sex_df)

sex_df %>%
  ggplot(aes(x = as.factor(sex), y = count)) +
  geom_col(fill = c("#068ee1", "#f87ac8"),
           col = "black") +
  geom_text(aes(label = count),
            vjust = -0.5,
            size = 4) +
  labs(x = "Sex of Children", 
       y = "Frequency", 
       title = "Sex Distribution of Children")

# frequency distribution of number of children and number of male children
tab1 <- table(dynamic_child_df$num_child, dynamic_child_df$num_male)

colnames(tab1) <- c("0 Male", "1 Male")
rownames(tab1) <- c("1 Child", "2 Children", "3 Children")

tab1

# frequency distribution of number of children and number of female children

tab2 <- table(dynamic_child_df$num_child, dynamic_child_df$num_female)

colnames(tab2) <- c("0 Female", "1 Female", "2 Female", "3 Female")
rownames(tab2) <- c("1 Child", "2 Children", "3 Children")

tab2
