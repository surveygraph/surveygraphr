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
# this is the main function, needs way better documentation and exception handling
graph_edgelists <- function(df, respondents_lcc = 0.95, items_lcc = 1.00) {
  # need to throw error if df not supplied
  edgelists <- .Call("surveygraphr_graph_edgelists", df, respondents_lcc, items_lcc)
  return(edgelists)
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

generate_survey_errors <- function(){
  # generate survey with all types of errors, for use with testing and 
  # demonstration of package
}

#' @export
generate_survey_polarised <- function(m = 200, n = 15, minority = 0.3, polarisation = 2){
  if(minority < 0.0 | minority > 1.0) minority = 0.3
  if(minority > 0.5 & minority < 1.0) minority = 1 - minority

  df <- data.frame(matrix(NA, nrow = m, ncol = n))
  respondent_mdata <- vector(mode = "integer", length = m)

  response_hi = 5 + polarisation
  response_lo = 5 - polarisation

  for(i in 1:m) {
    if(i < minority * m){
      average_response = response_lo
      respondent_mdata[i] = 0
    }else{
      average_response = response_hi
      respondent_mdata[i] = 1
    }
    for(j in 1:n) {
      df[i,j] <- as.numeric(rpois(1, average_response))
      while(df[i,j] < 1 | df[i,j] > 10){
        df[i,j] <- as.numeric(rpois(1, average_response))
      }
    }
  }
  return(df)
}
