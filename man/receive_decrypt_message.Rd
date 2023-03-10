% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encryption.R
\name{receive_decrypt_message}
\alias{receive_decrypt_message}
\title{Run server to receive a message}
\usage{
receive_decrypt_message(S, PORT = "27015")
}
\arguments{
\item{S}{: Singular matrix for decryption}

\item{PORT}{: Port to listen}
}
\value{
Received and decrypted message
}
\description{
Run server to receive a message
}
\examples{
#receive_decrypt_message("27015")    # Wait message
}
\seealso{
\link{send_encrypted_message}
}
