# addition
# multiplication - OK
# Reduce  - OK
# Inverse  - OK
# Determinant
# Substraction

# elementary operations
# Switch two lines
# Switch two columns
# multiply by a constant

# generate a permutation matrix
# generate an identity matrix

#' Computes matrix product of two matrices
#'
#' @param A - matrix (M x N)
#' @param B - matrix (N x P)
#'
#' @return matrix (M x P) which is the product of and and B
#' @export
#'
#' @examples
#' A <- matrix(1:4, nrow=1, ncol=4)
#' B <- matrix(1:16, nrow=4, ncol=4)
#' matrices.product(A, B)
#'
#' @seealso \link{matrices.power}
matrices.product <- function(A, B){
  if (is.null(dim(A))){
    dim(A) <- c(1, length(A))
  }
  if (is.null(dim(B))){
    dim(B) <- c(1, length(B))
  }

  C <- array(0, dim=c(dim(A)[1], dim(B)[2]), dimnames = list(rep("", dim(A)[1]), rep("", dim(B)[2])))
  rows <- dim(C)[1]
  cols <- dim(C)[2]
  for (i in 1:rows){
    for (j in 1:cols){
      for (k in 1:dim(A)[2])
        C[i,j] <- C[i,j] + A[i,k]*B[k,j]
    }
  }
  return(C)
}

# Computes matrix to a certain power
#' Computes matrix to a given power
#'
#' @param A - matrix
#' @param ext - power
#'
#' @return Matrix computed to the given power
#' @export
#'
#' @examples
#' A <- array(c(1, 2, 3, 4), dim=c(2,2))
#' matrices.power(A, 9)
#' I <- diag(1, 4)
#' matrices.power(I, 4)  # remains the same - Identity :)
#'
#' @seealso \link{matrices.product}
matrices.power <- function(A, ext){
  if (dim(A)[1] != dim(A)[2]) return(cat("Argument must be a square matrix"))
  output <- diag(1, dim(A)[1])
  #Start
  for (i in 1:ext)
    output <- matrices.product(output, A)
  return(output)
}


# Element by element modulus
#' computes de modulus in a given base of each element of a matrix
#'
#' @param A - a numeric vector or matrix
#' @param number - the given base used for operation
#'
#' @return Matrix or vector with all elements in mod[number]
#' @export
#'
#' @examples
#' A <- 1:6
#' matrices.mod(A, 3)
#' B <- array(c(2,3,4,6,7,-1,-1,4), dim=c(4,2))
#' matrices.mod(B, 27)
#'
#' @seealso \link{matrices.E}
matrices.mod <- function(A, number){
  output <- A
  for (i in 1:length(A)){
    output[i] <- A[i]%%number
  }
  return(output)
}

# Integer part function
#' Get integer part of a number
#'
#' @param X - number
#'
#' @return Integer part of X
#' @export
#'
#' @examples
#' integer_part(4.5)  # E(4.5)  = 4
#' integer_part(-4.5) # E(-4.5) = -5
#'
#' @seealso \link{matrices.mod}, \link{matrices.integerpart}
integer_part <- function(X){
  I <- as.integer(X)
  if ((X-I)>=0)
    return(I)
  else
    return(I-1)
}

# Get integer part of all elements of a matrix
#' matrices.E or matrices.integer_part computes integer part of all elements of the matrix
#'
#' @param matrix - Given numeric matrix to be computed
#'
#' @return Matrix containing integer part of elements of given matrix in exact position
#' @export
#'
#' @examples
#' A <- array(rnorm(16, 8, 56), dim=c(4,4))
#' matrices.integerpart(A)
#'
#' @seealso \link{matrices.mod}
matrices.integerpart <- function(matrix){
  output <- matrix
  length_matrix <- length(matrix)
  # Start
  for (i in 1:length_matrix)
    output[i] <- integer_part(matrix[i])
  return(output)
}

# Get integer part of all elements of a matrix
#' matrices.E or matrices.integer_part computes integer part of all elements of the matrix
#'
#' @param matrix - Given numeric matrix to be computed
#'
#' @return Matrix containing integer part of elements of given matrix in exact position
#' @export
#'
#' @examples
#' A <- array(rnorm(16, 8, 56), dim=c(4,4))
#' matrices.integerpart(A)
#'
#' @seealso \link{matrices.mod}
matrices.E <- matrices.integerpart

# Gauss-Jordan reduction
#' Gauss-Jordan reduction of matrix
#'
#' @details If the output is an Identity matrix then the determinant of this matrix is non null, hence inversible
#' @param A - matrix
#'
#' @return Reduced matrix
#' @export
#'
#' @examples
#' A <- array(c(4,8,9,2,7,9,2,6,0), dim=c(3,3))
#' matrices.reduce(A)
matrices.reduce <- function(A){
  pr = F
  if (isTRUE(pr)) print("Verbose mode on (will print all steps)")

  copy <- A
  rows <- dim(A)[1]
  cols <- dim(A)[2]
  lap <- min(rows, cols)
  # Start
  i <- 1; j <- i
  while ((i <= rows)&(j<=cols)){
    if (pr) print(paste(i,j))
    if (copy[i,j]!=0){
      copy[i,] <- (copy[i,]/copy[i,j])
      if (pr) print(copy)
      for (k in 1:rows){
        if ((k!=i)&(copy[k,j]!=0)){
          copy[k,] <- copy[k,] - copy[k,j]*copy[i,]
          if (pr) print(copy)
        }
      }
      i <- i +1 ; j <- j + 1
    } else {
      for (k in i:rows){
        if (copy[k,j]!=0){
          aux <- copy[k,]
          copy[k,] <- copy[i,]
          copy[i,] <- aux
          # break
          break
        }
      }
      if (pr) print(copy)
      if (copy[i,j]==0){
        j <- j+1
      }
    }
  }
  return(copy)
}

# Inverse of a matrix using compagnion method
#' Computes the inverse of an invertible square matrix
#'
#' @param A - square matrix
#'
#' @return Inverse of the matrix A
#' @export
#'
#' @examples
#' A <- array(c(4,8,9,2,7,9,2,6,0), dim=c(3,3))
#' matrices.inverse(A)
#'
#' @seealso \link{matrices.reduce}
matrices.inverse <- function(A){
  compagnion <- cbind(A, diag(1, dim(A)[1]))
  reduced <- matrices.reduce(compagnion)

  return(reduced[ , (dim(A)[1]+1):dim(compagnion)[2]])
}



#' Trace of a matrix
#'
#' @description Product of elements of the diagonal
#' @param A : A square matrix
#'
#' @return Integer
#' @export
#'
#' @examples
#' A <- matrix(c(1:4), ncol=2, nrow=2)
#' matrices.trace(A)
matrices.trace <- function(A){
  if (!is.null(dim(A)))
    if (!(dim(A)[1]==dim(A)[2]))
      cat("Not a square Matrix. Returning NULL")
    else
      return(prod(diag(A)))
  else
    cat("Not a matrix. Return NULL")
}


#' Solve systems of linear equations
#'
#' @param A A square matrix with coefficients of variables
#' @param B The matrix of results
#'
#' @return The matrix of solution for each result
#' @export
#'
#' @examples
#'
#'
#' # Example 1 - with a vector of results
#' A <- array(c(1,3,2,1), dim=c(2,2))
#' B <- c(1, 2)
#' matrices.linearsolve(A, B)
#'
#' # Example 2 - with a matrix of results
#' B <- array(c(1, 2, 2, 1, 5, 0), dim=c(2,3))
#' matrices.linearsolve(A, B)
matrices.linearsolve <- function(A, B){
  if (is.null(dim(B))){
    dim(B) <- c(length(B), 1)
  }

  start <- dim(A)[2]+1
  end   <- dim(A)[2]+dim(B)[2]
  reduce <- matrices.reduce(cbind(A, B))
  solution <- reduce[,start:end]
  if (start==end){
    dim(solution) <- c(length(solution), 1)
  }
  # Name dimensions
  rownames(solution) <- paste("x", 1:(dim(solution)[1]), sep="")
  colnames(solution) <- paste("y", 1:(dim(solution)[2]), sep="")

  return(solution)
}




#' Determinant of a real or complex matrix
#'
#' @description Use this function to compute the determinant of square matrix
#' @param A : a square matrix
#'
#' @return Determinant of the given matrix
#' @export
#'
#' @examples
#' S <- array(c(1:10, 1, 5, 5, 3, 4, 1), dim=c(4,4))  # Exemple with a 4x4 matrix
#' matrices.determinant(S)
#'
#' @seealso \link{matrices.reduce}, \link{matrices.inverse}
matrices.determinant <- function(A){
  if ((dim(A)[1]!=dim(A)[2])||(is.null(dim(A)))) return(NULL)
  copy <- A
  rows <- dim(A)[1]
  cols <- dim(A)[2]
  lap <- min(rows, cols)
  DETERMINANT <- 1
  # Start
  i <- 1; j <- i
  while ((i <= rows)&(j<=cols)){
    if (copy[i,j]!=0){
      DETERMINANT <- DETERMINANT * copy[i,j]
      copy[i,] <- (copy[i,]/copy[i,j])
      for (k in i:rows){
        if ((k!=i)&(copy[k,j]!=0)){
          copy[k,] <- copy[k,] - copy[k,j]*copy[i,]
        }
      }
      i <- i +1 ; j <- j + 1
    } else {
      for (k in i:rows){
        if (copy[k,j]!=0){
          aux <- copy[k,]
          copy[k,] <- copy[i,]
          copy[i,] <- aux
          # break
          break
        }
      }
      if (copy[i,j]==0){
        j <- j+1
      } else
        DETERMINANT <- DETERMINANT * (-1)
    }
  }
  DETERMINANT <- DETERMINANT * matrices.trace(copy)
  return((DETERMINANT))
}


#' Build Identity matrix
#'
#' @param M : First dimension
#' @param N : Second dimension
#'
#' @return Identity matrix of space MxN
#' @export
#'
#' @examples
#' matrices.identity(3)
#' matrices.identity(4, 3)
matrices.identity <- function(M, N=M){
  A <- array(0, dim=c(M,N))
  lap <- min(M, N)
  diagonal <- matrix(rep(1:lap, each=2), nrow=lap, ncol=2, byrow=TRUE)
  A[diagonal] <- 1
  return(A)
}
