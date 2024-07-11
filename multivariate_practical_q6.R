# Define a function to calculate the cofactor of a matrix
cofactor <- function(mat) {
  n <- nrow(mat)
  cofac <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      minor <- mat[-i, -j]
      cofac[i, j] <- (-1)^(i + j) * det(minor)
    }
  }
  
  return(cofac)
}

# Example matrix
A <- matrix(data = c(7, 3, 2, 3, 4, 1, 2, 1, 2), nrow = 3, ncol = 3, byrow = TRUE)
A

# Compute the cofactor matrix
cofac_A <- cofactor(A)

# Compute the adjoint matrix (transpose of cofactor matrix)
adj_A <- t(cofac_A)

# Print the adjoint matrix
print(adj_A)
