depression_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/non_parametric_prac_q11_data.csv', stringsAsFactors = TRUE)
dim(depression_data)
names(depression_data)

names(depression_data) <- c("Group 1", "Group 2", "Group 3")

library(tidyverse)

# Kruskal Wallis Test
df <- gather(depression_data, key = "group", value = "rating")
View(df)

sort_df_col <- function(dataframe, column){
  for (i in 1:(length(dataframe[,1]) - 1)) {
    for (j in (i+1):length(dataframe[,1])) {
      if(dataframe[i, column] > dataframe[j, column]){
        temp <- dataframe[i,]
        dataframe[i,] <- dataframe[j,]
        dataframe[j,] <- temp
      }
    }
  }
  
  return(dataframe)
}

sorted_df <- sort_df_col(df, 2)
View(sorted_df)

rank_of_uniques <- c()

for (i in 1:length(unique(sorted_df$rating))) {
  rank_of_uniques[i] <- mean( which( sorted_df$rating == unique(sorted_df$rating)[i] ) )
}

rank_of_uniques

ranks <- c()

for (i in 1:length(sorted_df$rating)) {
  temp <- which( unique(sorted_df$rating) == sorted_df$rating[i] )
  ranks[i] <- rank_of_uniques[temp]
}

ranks

sorted_df <- cbind(sorted_df, ranks)
View(sorted_df)

R1 <- sum(sorted_df$ranks[which(sorted_df$group == "Group 1")])
R2 <- sum(sorted_df$ranks[which(sorted_df$group == "Group 2")])
R3 <- sum(sorted_df$ranks[which(sorted_df$group == "Group 3")])

rank_sums <- c(R1, R2, R3)
rank_sums

n <- length(sorted_df$group)

H <- ((12) / (n * (n+1))) * (sum(rank_sums^2)/8) - 3 * (n+1)
H

qchisq(0.05, 2, lower.tail = FALSE)





# Mann Whitney U Test
U1 <- 0
for (i in 1:length(depression_data$`Group 1`)) {
  for (j in 1:length(depression_data$`Group 2`)) {
    if(depression_data$`Group 1`[i] > depression_data$`Group 2`[j]) {
      U1 <- U1 + 1
    }
  }
}

U1

U2 <- 0
for (i in 1:length(depression_data$`Group 1`)) {
  for (j in 1:length(depression_data$`Group 2`)) {
    if(depression_data$`Group 1`[i] < depression_data$`Group 2`[j]) {
      U2 <- U2 + 1
    }
  }
}

U2

U <- min(U1, U2)
U
