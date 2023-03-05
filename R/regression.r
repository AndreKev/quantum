source("matrices.r")

linear_model <- function(Y, X){
  keys <- names(X)
  lk = length(keys)  # Number of independent vars
  i <- 1
  x <- list()
  for (key in keys){
    x[[i]] <- X[[key]]
    i <- i+1
  }
  A = array(NA, dim=c(lk,lk))
  results = c()
  mY = mean(Y)
  for (k in 1:lk){
    results[k] <- sum(mY*x[[k]]-Y*x[[k]])
    for (i in 1:lk){
      mXi = mean(x[[i]])
      A[k, i] = sum(mXi*x[[k]]-x[[i]]*x[[k]])
    }
  }
  b1_n <- matrices.linearsolve(A, results)
  b0 <- mY
  for (k in 1:lk){
    b0 <- b0 - mean(x[[k]])*b1_n[k]
  }
  b <- array(c(b0, b1_n), dim=c(1, lk+1))
  names_b = c("intercept")
  for (k in 2:(lk+1)){
    names_b[k] <- paste("x", as.character(k-1), sep="")
  }
  colnames(b) <- names_b
  return(b)
}
