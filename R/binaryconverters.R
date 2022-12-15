
#' Decimal to Binary conversion
#'
#' @param number - number in decimal
#'
#' @return number in binary format
#' @export
#'
#' @examples
#' a <- 89
#' decimal_to_binary(a)   # Converts 89 to binary
#' decimal_to_binary(10)  # Output 1 0 1 0
#'
#' @seealso \link{binary_to_decimal}, \link{decimal_to_bcd}
decimal_to_binary <- function(number){
  output = 0
  i = 1
  while(number>0){
    output[i] <- number%%2
    number <- (number-output[i])/2
    i <- i+1
  }
  return(rev(output))
}

#' Unit to binary conversion
#'
#' @param unit  - The unit number to ne converted
#' @param fill  - The number of digits of the output
#'
#' @return Unit converted to binary, formated to the required number of digits
#' @export
#'
#' @examples
#' unit_to_binary(4, 3)  # returns  100
#' unit_to_binary(4, 4)  # returns 0100
#'
#' @seealso \link{decimal_to_binary}, \link{binary_to_decimal}
unit_to_binary <- function(unit, fill=4){
  output <- decimal_to_binary(unit)
  remainder <- fill-length(output)
  if (remainder>0) output <- c(rep(0, remainder), output)
  return(output)
}

# convert from binary to decimal
#' Binary to decimal convertion
#'
#' @param binary - Number in binary
#'
#' @return binary number converted to decimal
#' @export
#'
#' @examples
#' number <- c(1, 0, 1, 1)
#' binary_to_decimal(number)  # returns 7
#'
#' @seealso \link{decimal_to_binary}, \link{binary_to_gray}
binary_to_decimal <- function(binary){
  input <- rev(binary)
  i <- 0
  output <- 0
  for (digit in input){
    output <- output + digit*(2^i)
    i <- i + 1
  }
  return(output)
}

# convert a decimal number into a given base
#' Segment a number in it's base 'base' form
#'
#' @param number - Number in decimal format
#' @param base   - Base to convert to
#' @param fill   - How many digits do you want at output ?
#'
#' @return vector of digits of number in the given base
#' @export
#'
#' @examples
#' segment_digits(8)  # returns c(8)
#' segment_digits(89, 10)   # returns c(8, 9)
#' segment_digits(3, 2)     # returns c(1,1)
#' segment_digits(3, 2, 4)  # returns c(0,0,1,1)
#'
#' @seealso \link{decimal_to_binary} for direct conversion to binary
segment_digits <- function(number, base=10, fill=0){
  output = numeric()
  i = 1
  while(number>0){
    output[i] <- number%%base
    number <- (number-output[i])/base
    i <- i+1
  }
  remainder <- fill - length(output)
  if (remainder>0)
    return(c(rep(0, remainder), rev(output)))
  else
    return(rev(output))
}

# convert decimal number to BCD
#' Decimal number conversion to Binary Code Decimal
#'
#' @param number - Can be a decimal number or vector of unit numbers representing digits of the number
#'
#' @return Given number in BCD
#' @export
#'
#' @examples
#' decimal_to_bcd(23)       # returns 0010 0101
#' decimal_to_bcd(c(2, 3))  # returns 0010 0101
#'
#' @seealso \link{bcd_to_decimal}, \link{bcd_to_gray}
decimal_to_bcd <- function(number){
  if (length(number)==1)
    digits <- segment_digits(number)
  else
    digits <- number
  output <- numeric()
  for (digit in digits){
    output <- c(output, unit_to_binary(digit))
  }
  return(output)
}

# BCD to decimal conversion
#' Binary Code Decimal to Decimal code conversion
#'
#' @param bcd - A vector of numbers in BCD
#'
#' @return Given number converted in decimal
#' @export
#'
#' @examples
#' bcd_number <- c(0, 1, 1, 1, 1, 0, 0, 0)
#' bcd_to_decimal(bcd_number)  # returns 78
#'
#' @seealso \link{decimal_to_bcd}, \link{bcd_to_gray}
#'
bcd_to_decimal <- function(bcd){
  sep <- 4  # by default BCD are set to 4 bits
  #assertthat::are_equal(0, length(bcd)%%sep)    # Ensure that all the digits are given
  # Start
  output <- numeric()
  for (i in 1:(length(bcd)/sep)){
    interest <- bcd[(1+(i-1)*sep):(i*sep)]
    # print(interest) - view the region of interest at each turn
    output <- c(output, binary_to_decimal(interest))
  }
  return(output)
}

# Binary to gray conversion
#' Binary number to Gray number conversion
#'
#' @param number - number in binary format
#'
#' @return Given binary nunmber in gray format
#' @export
#'
#' @examples
#' binary_three <- c(1,1)
#' binary_to_gray(binary_three)   # returns c(1,0)
#'
#' @seealso \link{gray_to_binary}, \link{binary_to_decimal}
binary_to_gray <- function(number){
  input <- rev(number)
  output <- numeric()
  length_number <- length(number)
  for (i in 1:(length_number-1)){
    output[i] <- input[i]*(!input[i+1]) + (!input[i])*input[i+1]
  }
  output[length_number] <- input[length_number]
  # Converted binary to gray code
  return(rev(output))

}

# Convert gray to binary
#' Gray code to Binary code conversion
#'
#' @param gray - number in gray format
#'
#' @return Given number converted from gray to binary
#' @export
#'
#' @examples
#' gray_three <- c(1,0)
#' gray_to_binary(gray_three)  # return c(1,1)
#'
#' @seealso \link{binary_to_gray}, \link{gray_to_decimal}
gray_to_binary <- function(gray){
  bool <- gray == 1    # 0 and 1 are not stable for double representation
  output <- gray[1]
  i <- 2
  length_gray <- length(gray)
  while (i<=length_gray){
    if (isTRUE(bool[i])){  # Means in the binary, i and i-1 are different
      output[i] <- 1 - output[i-1]
    }  else{       # They are alike
      output[i] <- output[i-1]
    }
    i <- i + 1
  }
  return(output)
}

# BCD to gray
#' Binary Code Decimal to gray code conversion
#'
#' @param bcd - BCD number
#' @param sep - how many digits represent a number in this BCD ?
#'
#' @return Given BCD number converted to gray
#' @export
#'
#' @examples
#' bcd_number  <- c(c(0, 0, 1, 0), c(1, 0, 0, 1))   # bcd 29 represented with 4 digits per number
#' bcd_to_gray(bcd_number, sep=4)  # returns c(c(0,0,1,1), c(1,1,0,1))
#' bcd_to_gray(bcd_number)         # Direct gray conversion of all digits
#'
#' @seealso \link{bcd_to_decimal}, \link{decimal_to_bcd}
bcd_to_gray <- function(bcd, sep=NULL){
  if (is.null(sep)) sep <- length(bcd)
  output <- numeric()
  # Check if the bcd length is a multiple of the sep. Test fail returns an error
  #assertthat::are_equal(0, length(bcd)%%sep)
  # Start
  turns <- length(bcd)/sep
  for (i in 1:turns){
    # the zone of interest to this turns conversion
    interest <- bcd[(1+(i-1)*sep): (i*sep)]
    output <- c(output, binary_to_gray(interest))
  }
  return(output)
}

# Decimal to gray conversion
#' Decimal number to gray code conversion
#'
#' @param decimal - A number in decimal format
#' @return Given number in gray code
#' @export
#'
#' @seealso {\link{gray_to_binary}}, {\link{gray_to_decimal}}
#' @examples
#' decimal_to_gray(5)   # return c(1,1,1)
decimal_to_gray <- function(decimal){
  binary <- decimal_to_binary(decimal)
  return(binary_to_gray(binary))
}

# Convert gray code to decimal
#' Gray code to decimal conversion
#'
#' @param gray - Number in gray format
#'
#' @return Given number converted from gray to decimal
#' @export
#'
#' @examples
#' gray_to_decimal(c(1,0))  # returns 3
#' @seealso \link{decimal_to_gray}, \link{gray_to_binary}
gray_to_decimal <- function(gray){
  binary <- gray_to_binary(gray)
  decimal <- binary_to_decimal(binary)
  return(decimal)
}
