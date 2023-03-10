% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encryption.R
\name{decrypt_message}
\alias{decrypt_message}
\title{Decrypt a coded message}
\usage{
decrypt_message(cipher, I, KE, S)
}
\arguments{
\item{cipher}{- Crypted text}

\item{I}{-  Special matrix}

\item{KE}{- Public key}

\item{S}{- Singular matrix}
}
\value{
Decrypted message
}
\description{
Decrypt a coded message
}
\examples{
S <- array(as.integer(c(2,1,3,1,3,1,3,2,4,2,3,3,1,1,1,1)), dim=c(4,4), dimnames=empty_dim(4,4))
Encryption <- encrypt_message("Hello Guy !", S)
message <- decrypt_message(Encryption[[1]], Encryption[[2]], Encryption[[3]], S)

}
\seealso{
\link{encrypt_message}
}
