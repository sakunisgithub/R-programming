# mean
my_mean_function <- function(x){
  
  sample_sum <- 0
  
  for (i in 1:length(x)) {
    sample_sum <- sample_sum + x[i]
  }
  
  return(sample_sum / length(x))
}

# selection sort
my_selection_sort <- function(x){
  for (i in 1:(length(x)-1)) {
    for (j in (i+1):length(x)) {
      
      if(x[i] > x[j]){
        x[c(i, j)] <- x[c(j, i)]
      }
    }
  }
  return(x)
}

# median
my_median_function <- function(x){
  
  x <- my_selection_sort(x)
  
  if(length(x) %% 2 == 0){
    return((x[length(x)/2] + x[length(x)/2 + 1]) / 2)
  }
  else{
    return(x[(length(x) + 1) / 2])
  }
}

# sample central moments
my_sample_central_moments_function <- function(x, r){
  temp <- 0
  
  for (i in 1:length(x)) {
    temp <- temp + (x[i] - my_mean_function(x))^r
  }
  
  return(temp / (length(x) - 1))
}

