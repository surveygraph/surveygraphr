#' @export
sweep_thresholds <- function(data, layer){
  if(layer == "agent"){
    tdata <- .call("rsweep_thresholds_agent", data)
    return(tdata)
  }else if(layer == "symbolic"){
    tdata <- .call("rsweep_thresholds_symbolic", data)
    return(tdata)
  }else{
    print("layer must be either agent or symbolic")
  }
}
