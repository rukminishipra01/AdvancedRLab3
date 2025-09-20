#' Euclidean Algorithm
#'
#' Finds the greatest common divisor of two integers using the Euclidean algorithm.
#' The algorithm repeatedly applies the division algorithm until the remainder is zero.
#'
#' @param a An integer or numeric scalar
#' @param b An integer or numeric scalar
#' @return The greatest common divisor of a and b as a numeric value
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)

euclidean <- function(a, b) {
  # checking the arguments are numeric scalars or integers
  stopifnot(is.numeric(a), length(a) == 1, is.numeric(b), length(b) == 1)
  stopifnot(a == floor(a), b == floor(b))

  # make positive
  a <- abs(a)
  b <- abs(b)

  # Euclidean algorithm implementation
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }

  return(a)
}
