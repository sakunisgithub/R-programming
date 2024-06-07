batch_A <- c(26, 27, 31, 26, 19, 21, 20, 25, 30)
batch_B <- c(23, 28, 26, 24, 22, 19)

U1 <- 0

for (i in 1:length(batch_A)) {
  for (j in 1:length(batch_B)) {
    if(batch_A[i] > batch_B[j]){
      U1 <- U1 + 1
    }
  }
}

U2 <- 0

for (i in 1:length(batch_A)) {
  for (j in 1:length(batch_B)) {
    if(batch_A[i] < batch_B[j]){
      U2 <- U2 + 1
    }
  }
}

U1
U2

U <- min(U1, U2)
U
