#' @export
make_projection <- function(data, layer = "both", respondents_lcc = 0.95, items_lcc = 0.95){
  if(layer == "both"){
    edgelists <- .Call("surveygraphr_make_projection", data, respondents_lcc, items_lcc)
    return(edgelists)
  }else if(layer == "agent"){
    edgelist <- .Call("surveygraphr_make_projection_agent", data, respondents_lcc)
    return(edgelist)
  }else if(layer == "symbolic"){
    edgelist <- .Call("surveygraphr_make_projection_symbolic", data, items_lcc)
    return(edgelist)
  }
}

#' @export
sweep_thresholds <- function(data){
  thresholdlist <- .Call("surveygraphr_sweep_thresholds", data)
  return(thresholdlist)
}

#' @export
make_synthetic_data <- function(nrow = 200, ncol = 15, minority = 0.5, polarisation = 0){
  if(minority < 0.0 | minority > 1.0) minority = 0.3
  if(minority > 0.5 & minority < 1.0) minority = 1 - minority

  data <- data.frame(matrix(NA, nrow = nrow, ncol = ncol + 1))
  metadata <- vector(mode = "integer", length = nrow)

  response_hi = 5 + polarisation
  response_lo = 5 - polarisation

  for(i in 1:nrow){
    if(i < minority * nrow){
      if(runif(1) < 0.85){
        metadata[i] = 0
      }else{
        metadata[i] = 1
      }
      average_response = response_lo
      average_response = response_hi
    }else{
      if(runif(1) < 0.85){
        metadata[i] = 1
      }else{
        metadata[i] = 0
      }
      average_response = response_hi
      average_response = response_lo
    }

    data[i,1] = as.character(metadata[i])
    for(j in 2:(ncol+1)){
      data[i,j] <- as.numeric(rpois(1, average_response))
      while(data[i,j] < 1 | data[i,j] > 10){
        data[i,j] <- as.numeric(rpois(1, average_response))
      }
    }
  }
  return(data)
}

#' @export
make_weight_distribution <- function(){
  print("hello from make_weight_distribution")
}
