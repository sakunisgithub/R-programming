# progress indicator by a pie-chart
for (i in 1:100) {
  pie(c(i, 100 - i), 
      c(as.character(i), as.character(100-i)), 
      col = c("green", "pink"))
  Sys.sleep(1)
}

# progress indicator by a box filling up
plot(NA, NA, 
     xlim = c(1, 10), 
     ylim = c(1, 10),
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "")
axis(side = 1, at = 1:10, labels = 1:10)
axis(side = 4, at = 1:10, labels = seq(10, 100, 10))

for (j in 1:10) {
  for (i in seq(1, 10, 1)) {
    points(x = i, y = j, pch = 19, col = "#23e705", cex = 4)
    Sys.sleep(1)
  }
}


# progress indicator by a box filling up in a different way
pause <- function(seconds) {
  start <- Sys.time()
  while((Sys.time() - start) < seconds) {}
}

par(bg = "black")
plot(NA, NA, 
     xlim = c(1, 10), 
     ylim = c(1, 10),
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "")

ball_colors <- rep(c("#DB4437", "#F4B400", "#08e2f0", "#4285F4", "#0F9D58"), rep(2, 5))

for (t in 1:10) {
  pop <- 1:10
  
  for(k in 1:10){
    
    if(k < 10){
      num <- sample(pop, 1)
    } else{
      num <- pop[1]
    }
    
    cl <- rep("black", 10-t+1)
    
    for (m in 1:(10-t+1)) {
      cl[m] <- ball_colors[t]
      if(t != 10){
        points(x = rep(num, length(10:t)), y = 10:t, col = cl, pch = 19, cex = seq(1, 4, length = 10-t+1))
      } else{
        points(x = num, y = 10, col = cl, pch = 19, cex = 4)
      }
      pause(0.15)
      cl <- rep("black", 10-t+1)
    }
    
    index <- which(pop == num)
    pop <- pop[-index]
  }
}
