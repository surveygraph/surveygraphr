#' print hello world plus some facts.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)

#' @export
buildgraphs <- function(d) {
  # pilots the building of the respondent and item graphs
  #print("you should do some type checking here")
  #invisible(.Call("surveygraphr_pilot", d))
  .Call("surveygraphr_pilot", d)
  # returns a list of size two, containing respondent and item graphs
}

#' @export
surveydummy <- function(m, n) {
  df <- data.frame(matrix(NA, nrow = m, ncol = n))
  for(i in 1:m) {
    for(j in 1:n) {
      df[i,j] <- as.numeric(sample(1:5, 1))
    }
  }
  return(df)
}

