X <- c(1, 5, 7, 9, 15, 17, 21, 23)
Y <- c(2, 6, 10, 12, 18, 20, 26, 28, 32)

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
