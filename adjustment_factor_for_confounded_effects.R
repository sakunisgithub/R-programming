adjustment_factor_for_confounded_effect <- function(data, confounded_effect, confounded_in_replicate) {
  
  d <- data %>%
    filter(replicate == confounded_in_replicate)
  
  t <- which(d$treatment_combination == "(1)")
  index <- d$block[t]
  index
  
  d1 <- d %>%
    group_by(block) %>%
    summarise(block_total = sum(response))
  
  T1 <- d1$block_total[which(d1$block == index)]
  T2 <- d1$block_total[which(d1$block != index)]
  
  l <- nchar(confounded_effect)
  
  adjustment_factor <- c()
  
  if(l %% 2 == 0) {
    return(T1 - T2)
  }
  else {
    return(T2 - T1)
  }
}