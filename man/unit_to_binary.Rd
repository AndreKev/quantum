% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binaryconverters.R
\name{unit_to_binary}
\alias{unit_to_binary}
\title{Unit to binary conversion}
\usage{
unit_to_binary(unit, fill = 4)
}
\arguments{
\item{unit}{- The unit number to ne converted}

\item{fill}{- The number of digits of the output}
}
\value{
Unit converted to binary, formated to the required number of digits
}
\description{
Unit to binary conversion
}
\examples{
unit_to_binary(4, 3)  # returns  100
unit_to_binary(4, 4)  # returns 0100

}
\seealso{
\link{decimal_to_binary}, \link{binary_to_decimal}
}
