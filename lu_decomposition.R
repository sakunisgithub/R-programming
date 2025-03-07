LU_decomposer <- function(A){
  
  if(det(A) == 0) stop("Matrix must be non-singular.")
  
  I <- matrix(data = 0, nrow = nrow(A), ncol = ncol(A))
  
  for (i in 1:nrow(I)) {
    I[i, i] <- I[i, i] + 1
  }
  
  r <- dim(A)[1]
  c <- dim(A)[2]
  
  i <- 1; j <- 1
  
  while(j <= c) {
    
    while(i <= r) {
      
      if(i != r){
        a1 <- as.matrix(A[(i+1):r, j] / A[i, j])
        
        a2 <- t(as.matrix(A[i, ]))
        
        A[(i+1):r, ] <- A[(i+1):r, ] - a1 %*% a2
        
        I[(i+1):r, j] <- as.vector(a1)
        
        break
      }
      i <- i + 1
    }
    j <- j + 1
    
    i <- j
  }
  
  return(list(I, A))
}

A <- matrix(data = c(2, 4, 3, 5,
                     -4, -7, -5, -8,
                     6, 8, 2, 9,
                     4, 9, -2, 14),
            nrow = 4, ncol = 4, byrow = TRUE)

LU_decomposer(A)
