n <- 50000

x <- runif(n, -1, 1); y <- runif(n, -1, 1)

favourable <- 0

colors_pi <- c()

for (i in 1:n) {
  if(x[i]^2 + y[i]^2 <= 1){
    favourable <- favourable + 1
    colors_pi[i] <- "green"
  }else{
    colors_pi[i] <- "red"
  }
}

favourable

favourable / n

(favourable / n) * 4

plot(x, y, col = colors_pi)
