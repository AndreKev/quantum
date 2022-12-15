library(stringr)

# load these files
source("R/codetable.R")
source("R/encryptionmatrices.R")
source("R/matrices.R")
source("R/binaryconverters.R")
source("R/RcppExports.R")


#' Encrypt a message
#'
#' @param message - text
#' @param S - a singular 4x4 matrix
#'
#' @return Encrypted message
#' @export
#'
#' @examples
#' S <- array(as.integer(c(2,1,3,1,3,1,3,2,4,2,3,3,1,1,1,1)), dim=c(4,4), dimnames=empty_dim(4,4))
#' Encryption <- encrypt_message("Hello", S)
#' @seealso \link{decrypt_message}
encrypt_message <- function(message, S){
  # Vars
  pow <- sample(4:6, 1);
  base <- 27
  # Start
  M <- codetable1.numerise_char.as_matrix(message) # Convert message to num matrix
  encr_matrices <- c(sample(names(B), 2)) # randomly select two matrices
  #cat("\nRR: "); print(encr_matrices)
  B1 <- B[[encr_matrices[1]]]; B2 <- B[[encr_matrices[2]]]  #
  A = B1%*%B2    # Multiply the two matrices
  private_key <- matrix(
    c(
      segment_digits(as.integer(encr_matrices[1]), fill=2),
      segment_digits(as.integer(encr_matrices[2]), fill=2)
    ),
    nrow=1
  )   # the private key corresponding to two encr_matrices
  # cat("Private")
  # print(private_key)
  k1 <- decimal_to_bcd(private_key)   # we convert this private key to bcd
  k2 <- bcd_to_gray(k1)       # Then convert this bcd to gray
  k3 <- bcd_to_decimal(k2)    # bcd decode the gray code number

  KE <- as.integer(matrices.product(k3, S))   # Public key
  E <- matrices.product(M, matrices.power(A, pow))

  C <- matrices.mod(E, base)
  cipher <- c(
    codetable1.characterise_num.from_matrix(C),    # The last digit of this sequence corresponds to the power to which the A matrix was raised
    codetable1.characterise_num(pow)
  )
  I <- c(
    ravel_matrix(matrices.E(E/base)),
    pow        # Pow
  )

  #print("Encrypted")
  return(list(cipher, I, KE))
}



#' Decrypt a coded message
#'
#' @param cipher - Crypted text
#' @param I  -  Special matrix
#' @param KE - Public key
#' @param S  - Singular matrix
#'
#' @return  Decrypted message
#' @export
#'
#' @examples
#' S <- array(as.integer(c(2,1,3,1,3,1,3,2,4,2,3,3,1,1,1,1)), dim=c(4,4), dimnames=empty_dim(4,4))
#' Encryption <- encrypt_message("Hello Guy !", S)
#' message <- decrypt_message(Encryption[[1]], Encryption[[2]], Encryption[[3]], S)
#'
#' @seealso \link{encrypt_message}
decrypt_message <- function(cipher, I, KE, S){
  S.inverse <- matrices.inverse(S)
  k5 <- matrices.product(KE, S.inverse); k5 <- round(k5)#k5[k5 < 1e-10] <- 0
  k6 <- decimal_to_bcd(k5);   #k6[k6 < 1e-10] <- 0; k6[k6 > 0.9] <- 1
  k7 <- gray_to_binary(k6)
  K <- bcd_to_decimal(k7)

  #print(S.inverse); print(k5); print(k6); print(k7); print(K)
  #cat("\n")

  decrypt_indices <- paste(c(K[1], K[3]), c(K[2], K[4]), sep="")
  i1 <- decrypt_indices[1]
  i2 <- decrypt_indices[2]

  #print(decrypt_indices); print(B[[i1]]); print(B[[i2]]);

  A = matrices.product(B[[i1]], B[[i2]])
  #print(A)

  pow <- codetable1.numerise_char(cipher[length(cipher)])
  C <- codetable1.numerise_char.as_matrix(cipher[1:(length(cipher)-1)])

  A.inverse <- matrices.inverse(A)
  I <- matrix(I[1:(length(I)-1)], nrow=4, ncol=4, byrow=T)
  D <- 27*I + C
  M <- matrices.product(D, matrices.power(A.inverse, pow))
  # Decode the message
  return(
    sentence(
      codetable1.characterise_num.from_matrix(M))
  )
}

test_encr_decr <- function(){
  #set.seed(0)
  ALPHABET <- c(LETTERS, letters)
  message <- sentence(sample(ALPHABET, sample(5:16, 1)))
  cat("%s",message)
  S <- array(as.integer(c(2,1,3,1,3,1,3,2,4,2,3,3,1,1,1,1)), dim=c(4,4), dimnames=empty_dim(4,4))

  encr <- encrypt_message(message, S)

  decr <- decrypt_message(encr[[1]], encr[[2]], encr[[3]], S)
  print(decr)
  return(message==decr)
}




#' Send an encrypted message to a peer receiver on a network
#'
#' @description The receiver hosts a server at an address and you want to send an encrypted message to the server. The message is crypted and sent. A prerequisite is that the recepter must be waiting the message
#' @param message : String message to be sent
#' @param S : Singular matrix for encryption
#' @param address : Address of peer receiver
#' @param PORT : PORT at which receiver listens
#'
#' @return 1 if succeeded else 0
#' @export
#'
#' @examples
#' S <-  array(as.integer(c(2,1,3,1,3,1,3,2,4,2,3,3,1,1,1,1)), dim=c(4,4), dimnames=empty_dim(4,4))
#' send_encrypted_message("Catch It", S, "localhost", "27014")   # This will work only if peer is waiting message at that location
#' @seealso \link{receive_decrypt_message}
send_encrypted_message <- function(message, S, address="localhost", PORT="27015"){
  crypt <- encrypt_message(message, S)
  #print(crypt)
  scrypt <- sentence(
    c(crypt[[1]], as.character(crypt[[2]]), as.character(crypt[[3]])),
    sep="~"
  )
  scrypt <- sentence(c(38, scrypt, ""), sep="~")
  return(csend(message=scrypt, address=address, PORT=PORT))
}


#' Run server to receive a message
#'
#' @param S : Singular matrix for decryption
#' @param PORT : Port to listen
#'
#' @return Received and decrypted message
#' @export
#'
#' @examples
#' #receive_decrypt_message("27015")    # Wait message
#' @seealso \link{send_encrypted_message}
receive_decrypt_message <- function(S, PORT="27015"){
  crypt <- split(cwait(PORT))
  len <- as.integer(crypt[1])   # if bug remove
  #print(len)
  crypt <- crypt[2:(len+1)]         # if bug remove
  cipher <- crypt[1:17]
  I <- as.integer(crypt[18:34])
  KE <- as.integer(crypt[35:38])

  decrypt <- decrypt_message(cipher, I, KE, S)
  return(decrypt)
}


daemon <- function(PORT){
  while(TRUE){
    recv <- cwait(PORT)
    print(recv)
    if (recv=="STOP") break
  }
}
