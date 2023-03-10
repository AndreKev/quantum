% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binaryconverters.R
\name{segment_digits}
\alias{segment_digits}
\title{Segment a number in it's base 'base' form}
\usage{
segment_digits(number, base = 10, fill = 0)
}
\arguments{
\item{number}{- Number in decimal format}

\item{base}{- Base to convert to}

\item{fill}{- How many digits do you want at output ?}
}
\value{
vector of digits of number in the given base
}
\description{
Segment a number in it's base 'base' form
}
\examples{
segment_digits(8)  # returns c(8)
segment_digits(89, 10)   # returns c(8, 9)
segment_digits(3, 2)     # returns c(1,1)
segment_digits(3, 2, 4)  # returns c(0,0,1,1)

}
\seealso{
\link{decimal_to_binary} for direct conversion to binary
}
