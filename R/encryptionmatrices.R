source("R/paulimatrices.R")    # Pauli matrices

# Base blocks
a <- diag(1,2)
b <- pauli.rx
c <- -i*pauli.ry;  c <- matrix(as.integer(c), ncol=2, nrow=2, dimnames=empty_dim(2,2))
d <- pauli.rz

# Encryption matrices
B <- list()

# Create matrix from blocks
B.from_blocks.description <- "Creates a square 4x4 matrix with the given 2x2 matrices (a, b, c, d) in the order \n\n\t| a  b |\n\t| c  d |\n\n"

# Create a 4x4 matrix from four block of 2x2 matrices
#' Build encryption matrix from blocks of 2x2 matrices
#'
#' @param a - 2x2 matrix
#' @param b - 2x2 matrix
#' @param c - 2x2 matrix
#' @param d - 2x2 matrix
#'
#' @return 4x4 matrix
#' @export
#'
B.from_blocks <- function(a, b, c, d){
  output <- array(0, dim=c(4,4), dimnames=empty_dim(4,4))
  # Initialise the blocks
  output[1:2, 1:2] <- a
  output[1:2, 3:4] <- b
  output[3:4, 1:2] <- c
  output[3:4, 3:4] <- d
  # Return the encryption matrix
  return(output)
}

# Our twelve 4x4 singular matrices
B[["01"]] <- B.from_blocks(a, b, c, d)
B[["02"]] <- B.from_blocks(a, b, d, c)
B[["03"]] <- B.from_blocks(a, c, d, b)
B[["04"]] <- B.from_blocks(b, a, c, d)
B[["05"]] <- B.from_blocks(b, a, d, c)
B[["06"]] <- B.from_blocks(b, d, c, a)
B[["07"]] <- B.from_blocks(c, a, b, d)
B[["08"]] <- B.from_blocks(c, d, a, b)
B[["09"]] <- B.from_blocks(c, d, b, a)
B[["10"]] <- B.from_blocks(d, b, a, c)
B[["11"]] <- B.from_blocks(d, c, a, b)
B[["12"]] <- B.from_blocks(d, c, b, a)

