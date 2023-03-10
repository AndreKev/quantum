% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binaryconverters.R
\name{gray_to_binary}
\alias{gray_to_binary}
\title{Gray code to Binary code conversion}
\usage{
gray_to_binary(gray)
}
\arguments{
\item{gray}{- number in gray format}
}
\value{
Given number converted from gray to binary
}
\description{
Gray code to Binary code conversion
}
\examples{
gray_three <- c(1,0)
gray_to_binary(gray_three)  # return c(1,1)

}
\seealso{
\link{binary_to_gray}, \link{gray_to_decimal}
}
