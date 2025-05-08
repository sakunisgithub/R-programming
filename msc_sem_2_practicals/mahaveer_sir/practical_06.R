estimate_lambda <- function(s, alpha) mean(s^alpha)^(1/alpha)

estimate_alpha <- function(s, initial, epsilon = 0.0001, iterations = 100){
  
  alphas <- c(initial)
  
  for (i in 2:iterations) {
    l <- estimate_lambda(s, alphas[i-1])
    
    alphas[i] <- alphas[i-1] - u(alphas[i-1], l, s) / u_alpha(alphas[i-1], l, s)
    
    if(abs((alphas[i] - alphas[i-1])) < epsilon) break
  }
  
  return(alphas[length(alphas)])
}

# true values
true_alpha <- 3; true_lambda <- 2

# sample
x <- rweibull(200, shape = true_alpha, scale = true_lambda)

# ML Estimates
alpha_hat <- estimate_alpha(x, 1); alpha_hat

lambda_hat <- estimate_lambda(x, alpha_hat); lambda_hat

# u is derivative of the log-likelihood wrt alpha
# v is derivative of the log-likelihood wrt lambda

# u_alpha is derivative of u wrt alpha
u_alpha <- function(alpha, lambda, s){
  
  a <- - length(s) / alpha^2
  
  b <- sum((s / lambda)^alpha * log(s / lambda)^2)
  
  return(a - b)
}

# u_lambda is derivative of u wrt lambda
u_lambda <- function(alpha, lambda, s){
  
  a <- - length(s) / lambda
  
  b <- sum(s^alpha * (alpha * log(s / lambda) + 1)) / lambda^(alpha + 1)
  
  return(a + b)
}

# v_alpha is derivative of v wrt alpha
v_alpha <- function(alpha, lambda, s){
  
  a <- - length(s) / lambda
  
  b <- sum(s^alpha) / lambda^(alpha + 1)
  
  c <- sum( (s / lambda)^alpha * log(s / lambda) ) * (alpha / lambda)
  
  return(a + b + c)
}

# v_lambda is derivative of v wrt lambda
v_lambda <- function(alpha, lambda, s){
  
  a <- (length(s) * alpha) / lambda^2
  
  b <- sum(s^alpha) * (alpha * (alpha + 1)) / lambda^(alpha + 2)
  
  return(a - b)
}

a <- alpha_hat; l <- lambda_hat

# Hessian Matrix
J <- matrix(c(u_alpha(a, l, x), u_lambda(a, l, x),
              v_alpha(a, l, x), v_lambda(a, l, x)), nrow = 2, ncol = 2, byrow = TRUE)

J

# inv(-Hessian)
solve(-J)

sqrt(solve(-J))

diag(sqrt(solve(-J)))
