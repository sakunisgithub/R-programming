diagonal_dominance <- function(A){
  
  if(dim(A)[1] != dim(A)[2]) stop("Input must be a square matrix.")
  
  rest <- rowSums(abs(A)) - diag(abs(A))
  
  if(all( (diag(abs(A)) > rest)  == TRUE)){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}