% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrices.R
\name{matrices.power}
\alias{matrices.power}
\title{Computes matrix to a given power}
\usage{
matrices.power(A, ext)
}
\arguments{
\item{A}{- matrix}

\item{ext}{- power}
}
\value{
Matrix computed to the given power
}
\description{
Computes matrix to a given power
}
\examples{
A <- array(c(1, 2, 3, 4), dim=c(2,2))
matrices.power(A, 9)
I <- diag(1, 4)
matrices.power(I, 4)  # remains the same - Identity :)

}
\seealso{
\link{matrices.product}
}
