# Pauli matrices
# Preliminary

#' Return a list of 2 vectors with <a> empty strings in the first and <b> empty strings in the secons
#'
#' @param a - integer
#' @param b - integer
#'
#' @return list
#' @export
#'
#' @examples
#' I <- matrix(c(1,0,0,1), ncol=2, nrow=2, dimnames=empty_dim(2,2))
#' I
#'
#' @seealso \link{pauli.getrx}, \link{pauli.getry}, \link{pauli.getrz},
empty_dim <- function(a, b){
  return(list(rep("", a), rep("", b)))
}

i <- complex(imaginary=1) # Complex number  i = sqrt(-1)
I <- matrix(c(1,0,0,1), ncol=2, nrow=2, dimnames=empty_dim(2,2))           # Identity matrix of M2(C)


# Pauli matrices describes the spin of 1/2 spin particles in X, Y, Z axis

pauli.rx <- matrix(c(0, 1, 1, 0), ncol=2, nrow=2, dimnames=empty_dim(2,2))     # spin in X direction
pauli.ry <- matrix(c(0, i, -i, 0), ncol=2, nrow=2, dimnames=empty_dim(2,2))    # spin in Y direction
pauli.rz <- matrix(c(1, 0, 0, -1), ncol=2, nrow=2, dimnames=empty_dim(2,2))    # spin in Z direction

# Getter methods for pauli matrices

#' Getter method for Pauli rx spin matrix
#'
#' @return Pauli rx spin matrix
#' @export
#'
#' @examples
#' pauli.getrx()
pauli.getrx <- function() {
  return(pauli.rx)
}

#' Getter method for Pauli rx spin matrix
#'
#' @return Pauli ry spin matrix
#' @export
#'
#' @examples
#' pauli.getry()
pauli.getry <- function(){
  return(pauli.ry)
}

#' Getter method for Pauli rx spin matrix
#'
#' @return Pauli r\ spin matrix
#' @export
#'
#' @examples
#' pauli.getrz()
pauli.getrz <- function(){
  return(pauli.rz)
}

# Properties of Pauli matrices

# They are Hermitian and unitary - All their squares gives the Identitu
# The rx spin
# Determinant
# Eigen values
# Eigen Vectoer phixT, phix|
# Anti - commutativity
#pauli.rx.properties.
