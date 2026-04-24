# To estimate E[exp(U)] where U ~ U(0, 1)

true.theta <- exp(1) - 1; true.theta

set.seed(14)

size <- c(30, 50, 100, 200, 500, 1000)

# Monte Carlo Estimates

theta.hat.monte.carlo <- c()

for(i in 1:length(size)){
  n <- size[i]
  u <- runif(n)
  x <- exp(u)
  theta.hat.monte.carlo[i]<-mean(x)
}

theta.hat.monte.carlo

# Variance of Monte Carlo Estimator

var.theta.hat.monte.carlo <- c()

for(j in 1:length(size)){
  
  theta.hat.mc <- c()
  
  for(i in 1:1000){
    n <- size[j]
    u <- runif(n)
    x <- exp(u)
    theta.hat.mc[i]<-mean(x)
  }
  var.theta.hat.monte.carlo[j] <- var(theta.hat.mc)
}

var.theta.hat.monte.carlo




# Estimates from Control Variates Technique

# Control Variate Y ~ U(0, 1)

c.star <- - (0.14086) / (1 / 12); c.star

theta.hat.control <- c()

for(i in 1:length(size)){
  u <- runif(size[i])
  x <- exp(u)
  y <- u
  theta.hat.control[i] <- mean(x + c.star * (y - 0.5))
}

theta.hat.control


# Variance of the estimates from Control Variates Technique

var.theta.hat.control <- c()

for(j in 1:length(size)){
  
  theta.hat.ctrl <- c()
  
  for(i in 1:1000){
    n <- size[j]
    u <- runif(n)
    x <- exp(u)
    y <- u
    
    theta.hat.ctrl[i] <- mean(x + c.star * (y - 0.5))
  }
  
  var.theta.hat.control[j] <- var(theta.hat.ctrl)
}

var.theta.hat.control

# relative gain from Control Variates Technique

relative.gain <- ((var.theta.hat.monte.carlo - var.theta.hat.control) / var.theta.hat.monte.carlo ) * 100; relative.gain

# results

df <- data.frame(sample.size = size,
                 monte.carlo.estimates = theta.hat.monte.carlo,
                 monte.carlo.variance = var.theta.hat.monte.carlo,
                 control.estimates = theta.hat.control,
                 control.variance = var.theta.hat.control,
                 relative.gain = relative.gain)

View(df)
