#' print hello world plus some facts.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)

#' @export
dummy <- function(m, n) {
  m <- as.integer(m)
  n <- as.integer(n)
  #print("you should do some type checking here")
  invisible(.Call("surveygraphr_dummy", m, n))
}

#' @export
helloWorldNum <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  #print("you should do some type checking here")
  invisible(.Call("surveygraphr_hw_num", a, b))
}

#' @export
helloWorldInt <- function(a, b) {
  a <- as.integer(a)
  b <- as.integer(b)
  #print("you should do some type checking here")
  invisible(.Call("surveygraphr_hw_int", a, b))
}

#' @export
dataframeCheck <- function(x) {
  #print("you should do some type checking here")
  invisible(.Call("surveygraphr_df_check", x))
}

#' @export
inputdf <- function(m) {
  #print("you should do some type checking here")
  invisible(.Call("surveygraphr_inputdf", m))
}

#' @export
vecmanip <- function(m) {
  #print("you should do some type checking here")
  invisible(.Call("surveygraphr_vecmanip", m))
}
