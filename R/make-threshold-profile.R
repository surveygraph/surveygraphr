#' Illustrates how network properties vary with the similarity threshold
#'
#' @description
#' **Note that a major update to the surveygraph package, as well as documentation on this page, is planned for July 2025.**
#'
#' `make_threshold_profile()` outputs properties of the agent or symbolic network
#' as a function of similarity threshold.
#' 
#' @return
#' A data frame containing properties of the `agent` or `symbolic` network as a
#' function of the similarity threshold. In particular, it contains three columns
#' named
#' 
#'   - `threshold`, the value of the similarity threshold
#' 
#'   - `ad`, the average degree resulting from `threshold`, and 
#' 
#'   - `lcc`, the size of the largest connected component resulting from
#'   `threshold`
#' 
#' @param data A data frame corresponding to the attitudes held by agents with
#'   respect to a number of items
#' @param layer A string flag specifying the type of network to be extracted,
#'   
#'   - `"agent"` produces the network corresponding to the agents, which we assume
#'   to be rows in `data`
#' 
#'   - `"symbolic"` produces the network corresponding to the symbols, or items,
#'   which we assume to be columns in `data`
#' 
#' @param mincompare An integer, minimum number of comparisons for valid distance.
#' @param metric A string option describing similarity metric used.
#' @details
#' Note that this routine is expensive on large graphs. We study networks over the
#' full range of similarity thresholds `[-1, 1]`, and as a result, produce
#' networks that are complete at the lower limit of that range. Note that by default we
#' will subsample the provided survey with the C++ implementation in order to
#' avoid memory issues. We could then allow a flag that turns off the subsampling
#' step, at the user's peril.
#' @export
#' @examples
#' S <- make_synthetic_data(20, 5)
make_threshold_profile <- function(
  data, 
  layer = NULL,
  mincompare = NULL,
  metric = NULL
){
  if(layer == "agent"){
    layer <- as.integer(0)
  }else if(layer == "symbolic"){
    layer <- as.integer(1)
  }else{
    message("layer must be either agent or symbolic, defaulting to agent")
    layer <- as.integer(0)
  }

  mincompare <- as.integer(1)
  metric <- as.integer(0)

  profile <- .Call(
    "rmake_threshold_profile",
    data,
    layer,
    mincompare,
    metric
  )
}
