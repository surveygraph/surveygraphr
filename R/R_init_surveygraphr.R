#' print hello world plus some facts.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)

#' @export
listgraphs <- function(df) {
  # returns a list containing respondent and item graphs
  # we should do some rigorous type checking here...
  .Call("surveygraphr_list_graphs", df)
  # returns a list of size two, containing respondent and item graphs
}

#' @export
gensurvey <- function(m, n) {
  df <- data.frame(matrix(NA, nrow = m, ncol = n))
  for(i in 1:m) {
    for(j in 1:n) {
      df[i,j] <- as.numeric(sample(1:10, 1))
    }
  }
  return(df)
}

#' @export
explore <- function(){
  S <- gensurvey(1000, 25)
  elists <- buildgraphs(S)
}
