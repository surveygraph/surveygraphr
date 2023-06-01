#' Outputs the survey projection onto the agent or symbolic layer
#'
#' @description
#' `make_projection()` outputs the agent network or the symbolic
#' network corresponding to a survey, i.e. the row or column projection.
#' 
#' @details
#' add some details here
#' 
#' @return
#' A data.frame corresponding to the edge list of the specified network. It contains
#' three columns, the first two being the indices of nodes adjacent to an edge, and the 
#' third column its weight.
#'
#' @param data A data frame corresponding to a survey
#' @param layer A string flag that specifies which layer to project, either agent or symbolic
#' @param threshold_method A string flag that specifies how to threshold the network
#' @param method_value A utility numerical value that we interpret according to the threshold_method chosen
#' @examples
#' make_check(df, "agent")
#' make_projection(df, "agent")
#' make_projection(df, "agent")
#' make_projection(df, "agent")
#' make_projection(df, "agent", threshold_method="target_lcc")
#' @export
make_projection <- function(data,
                            layer,
                            threshold_method = NULL,
                            method_value = NULL,
                            centre = NULL,
                            similarity_metric = NULL){

  # check that layer is either agent or symbolic
  if(layer != "agent" && layer != "symbolic")
    print("layer needs to be either agent or symbolic")

  # check the value of centre
  if(is.null(centre)){
    if(layer == "agent"){
      centre <- as.integer(1)
    }else if(layer == "symbolic"){
      centre <- as.integer(1)
    }
  }else if(centre == TRUE){
    centre <- as.integer(1)
  }else if(centre == FALSE){
    centre <- as.integer(0)
  }else{
    print("centre must be either TRUE or FALSE") 
  }

  # check the value of similarity_matric
  if(is.null(similarity_metric)){
    if(layer == "agent"){
      similarity_metric <- as.integer(0)
    }else if(layer == "symbolic"){
      similarity_metric <- as.integer(0)
    }
  }else if(similarity_metric == "manhattan"){
    similarity_metric <- as.integer(0)
  }else{
    print("overriding similarity metric to manhattan distance") 
    similarity_metric <- as.integer(0)
  }

  if(layer == "agent"){
    if(!is.null(threshold_method) && !is.null(method_value)){
      if(threshold_method == "target_lcc"){
        edgelist <- .Call("rmake_proj_agent_lcc", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else if(threshold_method == "target_ad"){
        edgelist <- .Call("rmake_proj_agent_ad", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else if(threshold_method == "raw_similarity"){
        edgelist <- .Call("rmake_proj_agent_similar", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else{
        print("threshold_method must be target_lcc, target_ad, or raw_similarity")
        print("defaulting to target_lcc with method_value = 1")
        edgelist <- .Call("rmake_proj_agent_lcc", data, 0.95, centre, similarity_metric)
        return(edgelist)
      }
    }else{
      print("threshold_method and method_value both need to be set, defaulting to...")
    }
  }

  if(layer == "symbolic"){
    if(!is.null(threshold_method) && !is.null(method_value)){
      if(threshold_method == "target_lcc"){
        edgelist <- .Call("make_proj_symbolic_lcc", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else if(threshold_method == "target_ad"){
        edgelist <- .Call("make_proj_symbolic_ad", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else if(threshold_method == "raw_similarity"){
        edgelist <- .Call("make_proj_symbolic_similar", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else{
        print("threshold method must be one of [...]")
        print("defaulting to target_ad with method_value = 1")
        edgelist <- .Call("make_proj_symbolic_ad", data, 1, centre, similarity_metric)
        return(edgelist)
      }
    }else{
      print("threshold_method and method_value both need to be set, defaulting to...")
    }
  }
}
