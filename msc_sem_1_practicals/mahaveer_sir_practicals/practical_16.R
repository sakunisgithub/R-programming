n <- 50000

x <- runif(n, 0, 2); y <- runif(n, 0, 1)

favourable <- 0

colors_func <- c()

func <- function(x) 1 - (1-x)^2

for (i in 1:n) {
  if(y[i] <= func(x[i])){
    favourable <- favourable + 1
    colors_func[i] <- "green"
  }else{
    colors_func[i] <- "red"
  }
}

favourable

favourable / n

plot(x, y, col = colors_func)
