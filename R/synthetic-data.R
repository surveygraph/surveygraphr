#' Outputs a synthetic survey using a simple model
#'
#' @description
#' `make_synthetic_data()` outputs a synthetic survey, generated using a simple, stochastic
#'   model of polarisation.
#' 
#' @return
#' A data frame corresponding to a survey.
#' 
#' @param nrow The number of rows in the survey
#' @param ncol The number of columns in the survey
#' @param minority The fraction of nodes in the smaller of the two polarised groups
#' @param polarisation The degree of polarisation among the system's agents
#' 
#' @export
#' @examples
#' S <- make_synthetic_data()
#' @export
make_synthetic_data <- function(nrow = 200, ncol = 15, minority = 0.5, polarisation = 0){
  if(minority < 0.0 | minority > 1.0) minority = 0.5
  if(minority > 0.5 & minority < 1.0) minority = 1 - minority

  data <- data.frame(matrix(NA, nrow = nrow, ncol = ncol + 1))
  groupinfo <- vector(mode = "integer", length = nrow) 

  response_hi = 5 + polarisation
  response_lo = 5 - polarisation

  for(i in 1:nrow){
    if(i < minority * nrow){
      if(runif(1) < 0.85){
        groupinfo[i] = 0
      }else{
        groupinfo[i] = 1
      }
      average_response = response_lo
      average_response = response_hi
    }else{
      if(runif(1) < 0.85){
        groupinfo[i] = 1
      }else{
        groupinfo[i] = 0
      }
      average_response = response_hi
      average_response = response_lo
    }

    data[i,1] = as.character(groupinfo[i])
    for(j in 2:(ncol+1)){
      data[i,j] <- as.numeric(rpois(1, average_response))
      while(data[i,j] < 1 | data[i,j] > 10){
        data[i,j] <- as.numeric(rpois(1, average_response))
      }
    }
  }

  # name columns "group" "item_1" "item_2" ... "item_ncol"
  newnames <- c("group")
  for(i in 1:ncol){
    cntemp <- paste(c("item_", i), collapse="")
    newnames <- append(newnames, cntemp) 
  }
  colnames(data) <- newnames

  return(data)
}
