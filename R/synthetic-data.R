#' @export
make_synthetic_data <- function(nrow = 200, ncol = 15, minority = 0.5, polarisation = 0){
  if(minority < 0.0 | minority > 1.0) minority = 0.3
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
