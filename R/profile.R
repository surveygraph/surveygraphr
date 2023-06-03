#' @export
make_threshold_profile <- function(data, layer){
  if(layer == "agent"){
    tdata <- .Call("rmake_threshold_profile_agent", data)
    return(tdata)
  }else if(layer == "symbolic"){
    tdata <- .Call("rmake_threshold_profile_symbolic", data)
    return(tdata)
  }else{
    print("layer must be either agent or symbolic")
  }
}
