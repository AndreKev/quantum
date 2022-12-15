#' Send a message to a network address
#'
#' @description This function enables to send a message to a particular address, on a particular port. This requires that a server is listening to the port
#' @param message : String
#' @param address : IP address
#' @param PORT : Port to communicate
#'
#' @return Status of communication
#' @export
#'
#' @examples
#' message <- "The Open QUANTUM Security"
#' address <- "localhost"
#' PORT    <- "27015"
#' send_message(message, address, PORT)
#'
#' @seealse \link{wait_message}
send_message <- function(message, address, PORT){
  return(csend(message, address, PORT))
}


#' Wait message from a port
#'
#' @description
#' Listen to a port on your machine to wait for a message. Ensure the port is free. The sender will communicate to this port
#' @param PORT : PORT to listen
#'
#' @return received message
#' @export
#'
#' @examples
#' wait_message(27015)
#'
#' @seealso \link{send_message}
wait_message <- function(PORT){
  return(cwait(as.character(PORT)))
}
