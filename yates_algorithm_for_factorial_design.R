yates_algo_factorial_exp <- function(trt.comb, trt.comb.total, n, nreplicates){
  
  all_numbers <- c()
  
  previous <- trt.comb.total
  
  new_num <- c()
  
  for (j in 1:n) {
    for (i in 1:length(previous)) {
      if(i <= length(previous) / 2){
        new_num[i] <- previous[2*i - 1] + previous[2*i]
      }
      else{
        temp <- i - (length(previous) / 2)
        new_num[i] <- previous[2*temp] - previous[2*temp - 1]
      }
    }
    
    all_numbers <- append(all_numbers, new_num)
    
    previous <- new_num
  }
  
  my_mat <- matrix(data <- all_numbers, nrow = length(previous), ncol = n, byrow = FALSE)
  colnames(my_mat) <- sprintf("run_%d", 1:n)
  
  ss <- my_mat[,n]^2 / (2^n * nreplicates)
  
  my_df <- data.frame(treatment_combinations = trt.comb, treatment_combination_total = trt.comb.total, my_mat, sum_squares = ss)
  
  return(my_df)
  
}

new_df <- yates_algo_factorial_exp(c('1', 'k', 'p', 'kp'), c(-10, -4, -10, 24), 2, nreplicates = 4)
View(new_df)
