# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

csend <- function(message, address, PORT) {
    .Call(`_quantum_csend`, message, address, PORT)
}

cwait <- function(R_PORT) {
    .Call(`_quantum_cwait`, R_PORT)
}

