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
generate_survey <- function(m = 20, n = 5) {
  df <- data.frame(matrix(NA, nrow = m, ncol = n))
  for(i in 1:m) {
    for(j in 1:n) {
      df[i,j] <- as.numeric(sample(1:10, 1))
    }
  }
  return(df)
}

#' @export
generate_survey_polarised <- function(m = 20, n = 5, p = 0.5) {
  df <- data.frame(matrix(NA, nrow = m, ncol = n + 1))
  response_hi = 8
  response_lo = 2
  for(i in 1:m) {
    average_response = response_hi
    user_group = 1
    if(runif(1) < p){
      average_response = response_lo
      user_group = 0
    }
    df[i,1] = 
    for(j in 2:n) {
      df[i,j] <- as.numeric(rpois(1, average_response))
      while(df[i,j] < 1 | df[i,j] > 10){
        df[i,j] <- as.numeric(rpois(1, average_response))
      }
    }
  }
  return(df)
}
