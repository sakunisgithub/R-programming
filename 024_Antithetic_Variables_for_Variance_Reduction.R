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




# Estimates from Antithetic Variable Technique

theta.hat.antithetic <- c()

for(i in 1:length(size)){
  u <- runif(size[i] / 2)
  x1 <- exp(u)
  x2 <- exp(1-u)
  theta.hat.antithetic[i] <- mean(c(x1, x2))
}

theta.hat.antithetic


# Variance of the estimates from Antithetic Variable Technique

var.theta.hat.antithetic <- c()

for(j in 1:length(size)){
	
  theta.hat.anti <- c()
  
	for(i in 1:1000){
		n <- size[j]
		u <- runif(n / 2)
		x1 <- exp(u)
		x2 <- exp(1-u)
		
		theta.hat.anti[i] <- mean(c(x1,x2))
	}

	var.theta.hat.antithetic[j] <- var(theta.hat.anti)
}

var.theta.hat.antithetic

# relative gain from Antithetic Variable Technique

relative.gain <- ((var.theta.hat.monte.carlo - var.theta.hat.antithetic) / var.theta.hat.monte.carlo ) * 100; relative.gain

# results

df <- data.frame(sample.size = size,
			monte.carlo.estimates = theta.hat.monte.carlo,
			monte.carlo.variance = var.theta.hat.monte.carlo,
			antithetic.estimates = theta.hat.antithetic,
			antithetic.variance = var.theta.hat.antithetic,
			relative.gain = relative.gain)

View(df)
