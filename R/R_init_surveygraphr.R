#' print hello world plus some facts.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)

#' @export
sweep_thresholds <- function(df){
  lccdata <- .Call("surveygraphr_explore_graphs", df)
  return(lccdata)
}

#' @export
list_graphs <- function(df) {
  # returns a list containing respondent and item graphs
  # we should do some rigorous type checking here...
  elist <- .Call("surveygraphr_list_graphs", df)
  return(elist)
}

#' @export
generate_survey <- function(m, n) {
  df <- data.frame(matrix(NA, nrow = m, ncol = n))
  for(i in 1:m) {
    for(j in 1:n) {
      df[i,j] <- as.numeric(sample(1:10, 1))
    }
  }
  return(df)
}

#' @export
generate_polarised_survey <- function(m, n) {
  df <- data.frame(matrix(NA, nrow = m, ncol = n))
  for(i in 1:m) {
    for(j in 1:n) {
      df[i,j] <- as.numeric(sample(1:10, 1))
    }
  }
  return(df)
}
