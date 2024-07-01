boys <- c(96, 65, 74, 78, 82, 121, 68, 79, 111, 48, 53, 92, 81, 31, 40)
girls <- c(12, 47, 32, 59, 83, 14, 32, 15, 17, 82, 21, 34, 9, 15, 51)

# U Test
U1 <- 0
for (i in 1:length(boys)) {
  for (j in 1:length(girls)) {
    if(boys[i] > girls[j]) {
      U1 <- U1 + 1
    }
  }
}

U1

U2 <- 0
for (i in 1:length(boys)) {
  for (j in 1:length(girls)) {
    if(boys[i] < girls[j]) {
      U2 <- U2 + 1
    }
  }
}

U2

U <- min(U1, U2)
U




# Run Test
X <- boys
Y <- girls

XY <- c(rep("X", length(X)), rep("Y", length(Y)))

df <- data.frame(XY = XY, values = c(X, Y))
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

number_of_runs <- function(dataframe, column){
  temp <- 0
  for (i in 1:(length(dataframe[,1])-1)) {
    if(dataframe[i, column] != dataframe[i+1, column]){
      temp <- temp + 1
    }
  }
  
  return(temp+1)
}

runs <- number_of_runs(sorted_df, 1)
runs
