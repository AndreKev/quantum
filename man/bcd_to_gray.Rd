% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binaryconverters.R
\name{bcd_to_gray}
\alias{bcd_to_gray}
\title{Binary Code Decimal to gray code conversion}
\usage{
bcd_to_gray(bcd, sep = NULL)
}
\arguments{
\item{bcd}{- BCD number}

\item{sep}{- how many digits represent a number in this BCD ?}
}
\value{
Given BCD number converted to gray
}
\description{
Binary Code Decimal to gray code conversion
}
\examples{
bcd_number  <- c(c(0, 0, 1, 0), c(1, 0, 0, 1))   # bcd 29 represented with 4 digits per number
bcd_to_gray(bcd_number, sep=4)  # returns c(c(0,0,1,1), c(1,1,0,1))
bcd_to_gray(bcd_number)         # Direct gray conversion of all digits

}
\seealso{
\link{bcd_to_decimal}, \link{decimal_to_bcd}
}
