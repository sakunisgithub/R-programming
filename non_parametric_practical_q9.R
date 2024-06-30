observations <- c(24, 35, 12, 50, 60, 70, 68, 49, 80, 25, 69, 28, 28, 11, 83, 31, 37, 34, 54, 75, 45, 95, 75, 26, 43, 57, 94, 48, 63, 45)
length(observations)

sorted_observations <- sort(observations)
sorted_observations

mean(c(sorted_observations[15], sorted_observations[16]))

m <- median(observations)

codes <- ifelse(observations > m, "A", "B")

df <- data.frame(observations, codes)
View(df)

number_of_runs <- function(dataframe, column){
  temp <- 0
  for (i in 1:(length(dataframe[,1])-1)) {
    if(dataframe[i, column] != dataframe[i+1, column]){
      temp <- temp + 1
    }
  }
  
  return(temp+1)
}

runs <- number_of_runs(df, 2)
runs

length(which(df$codes == "A"))
length(which(df$codes == "B"))
