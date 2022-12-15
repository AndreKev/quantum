library(stringr) # import substring(), substr(), str_length

# Code table 1
DIGITS <- as.character(0:9)
SPECIAL <- c( " ", "!", "?", "!", "/", "*", "-", "+", ".", "@", ":", ")", "(", ",")
labs <- c(LETTERS, letters, DIGITS, SPECIAL, "")
codetable1 <-  rep(0:(length(labs)-1), 1)
names(codetable1) <- labs
# Inverse code table 1
codetable1.inverse <- names(codetable1)
names(codetable1.inverse) <- codetable1


# Convert a char text into numerals following a code table
#' Convert a character text to numerals following the code table
#'
#' @param char - Text to be converted
#'
#' @return  Numbers representing the given text character in the code table
#' @export
#'
#' @examples
#' codetable1.numerise_char("Hello You")
#'
#' @seealso \link{codetable1.numerise_char.as_matrix}
codetable1.numerise_char <- function(char){
  if (length(char)==1)
    length_string <- stringr::str_length(char)
  else
    length_string <- length(char)

  output <- integer()
  # Conversion
  for (index in 1:length_string){
    if (length(char)==1)
      c <- substring(char, index, index)
    else
      c <- char[index]
    output[index] <- codetable1[c]
  }
  # Converted
  return(output)
}

# Numerise char and convert to 4x4 matrix
#' Numerise character and convert it into a 4x4 matrix filled by rows
#'
#' @param char - Text to be converted
#'
#' @return 4x4 Matrix containing the numerised char
#' @export
#'
#' @examples
#' codetable1.numerise_char.as_matrix("Quantum Mechanic")
#'
#' @seealso \link{codetable1.numerise_char}
codetable1.numerise_char.as_matrix <- function(char){
  M <- codetable1.numerise_char(char)
  if (length(M)<16)
    M[(length(M)+1):16] <- length(codetable1)-1   # Position of "" in the table
  M <- matrix(M, ncol=4, nrow=4, byrow=T)
  return(M)
}


# Ravel matrix to to vector by rows
#' Redimesion a MxN matrix into a 1x(M*N) matrix by columns
#'
#' @param matrix Matrix to be ravelled
#'
#' @return Ravelled matrix
#' @export
#'
#' @examples
#' A <- array(c(1,2,3,4), dim=c(2,2))
#' ravel_matrix(A)  # return c(1,3,2,4)
#'
#' @seealso \link{codetable1.numerise_char}, \link{codetable1.numerise_char.as_matrix}
ravel_matrix <- function(matrix){
  # Output object
  ravel <- numeric()
  mode(ravel) <- mode(matrix)

  # Number of rows
  rows <- nrow(matrix)
  for (i in 1:rows)
    ravel <- c(ravel, matrix[i,])
  return(ravel)
}

# Convert numerals to characters following code table
#' Convert numerals to characters following code table
#'
#' @param num - Numerals to be converted
#'
#' @return Vector containing characters
#' @export
#'
#' @examples
#' numerals <- c(8, 45, 52, 34, 44, 52,  4, 37, 30, 28, 45, 43, 34, 28, 52, 53)
#' converted <- codetable1.characterise_num(numerals)
#' converted    # Converted from numerals to character
#' paste(converted, collapse="")  # Merge all
#'
#' @seealso \link{codetable1.numerise_char}, \link{codetable1.characterise_num.from_matrix}
codetable1.characterise_num <- function(num){
  length_nums <- length(num)
  output <- rep("", length_nums)
  # Conversion
  for (index in 1:length_nums){
    number <- as.character(num[index])
    output[index] <- codetable1.inverse[[number]]
  }
  # Converted
  return(output)
}



# Convert numeric matrix to character following codetable
#' Characterise numerals from a matrix
#'
#' @param numerals - Matrix of numerals
#'
#' @return raveled messaged containing characters
#' @export
#'
#' @examples
#' numerals <- array(
#'     c(8, 52, 31, 30, 30, 37, 52, 43, 30, 26, 29, 50, 54, 54, 54, 54),
#'     dim = c(4,4)
#' )
#' characters <- codetable1.characterise_num(numerals)
#' sentence(characters)
#' @seealso \link{codetable1.numerise_char}, \link{codetable1.characterise_num}
codetable1.characterise_num.from_matrix <- function(numerals){

  ravel <- ravel_matrix(numerals)
  output <- codetable1.characterise_num(ravel)
  # Converted to character following the code table
  return(output)

}

# Merge all character
#' Merge all the elements of a vector into a string
#'
#' @param vector - vector containing elements to be concatenated into a string
#' @param sep - seperator
#'
#' @return String of all the vector elements merged
#' @export
#'
#' @examples
#' sentence(c("H", "e", "l", "l", "o"))
sentence <- function(vector, sep=""){
  return(paste(vector, collapse=sep))
}


#' Split a String into a character vector
#'
#' @param string : text to be parsed
#' @param sep : seperator
#'
#' @return Character vector containg the elements splitted according to the seperator
#' @export
#'
#' @examples
#' split("Hello world ! Guess my name", sep=" ")
split <- function(string, sep="~"){
  buffer <- character()
  overflow <- stringr::str_length(string) + 1
  i <- 1; start <- 1; end <- 1
  while (end < overflow){
    if (substring(string, end, end)==sep){
      buffer[i] <- substring(string, start, end-1)
      i <- i + 1;
      start <- end + 1
    }
    end <- end + 1
  }
  buffer[i] <- substring(string, start, end-1)
  return(buffer)
}
