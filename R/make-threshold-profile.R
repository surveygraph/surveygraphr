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
  metric = NULL,
  count = NULL,
  likert = NULL,
  dummycode = NULL,
  ...
){
  dots <- list(...)

  if(length(dots) > 0){
    warning("Unused arguments in ...: ", paste(names(dots), collapse = ", "), call. = F)
  }


  # Checking of data, likert and dummycode is done in data_preprocess()
  data <- data_preprocess(data, likert, dummycode)


  # TODO: avoid the redundancy with these checks and make_projection()
  # TODO: rename mincompare to comparisons
  # TODO: rename Euclidean and Manhattan to RMSE and MAE


  # Check that layer is either "agent" or "symbolic", mapping to
  # 0 for agent
  # 1 for symbolic
  if(is.null(layer)){
    layer <- as.integer(0)
  }else if(!is.character(layer)){
    stop("`layer` argument must be a character string.", call. = F)
  }else if(length(layer) != 1){
    stop("`layer` argument must be of length 1.", call. = F)
  }else if(is.na(layer)){
    stop("`layer` argument cannot be NA.", call. = F)
  }else if(layer %in% c("a", "Agent", "agent")){
    layer <- as.integer(0)
  }else if(layer %in% c("s", "Symbolic", "symbolic")){
    layer <- as.integer(1)
  }else{
    stop("`layer` option \"", layer, "\" unrecognised.", call. = F)
  }


  # Check mincompare, the minimum number of numerical pairwise comparisons for
  # computing similarity.
  bounds <- c(ncol(data), nrow(data))
  if(is.null(mincompare)){
    defaults <- as.integer(c(ceiling(ncol(data) / 2), ceiling(nrow(data) / 2)))
    mincompare <- defaults[layer + 1]
  }else if(!is.numeric(mincompare)){  
    stop("`mincompare` argument must be an integer.", call. = F)
  }else if(length(mincompare) != 1){
    stop("`mincompare` argument must be of length 1.", call. = F)
  }else if(is.na(mincompare)){  
    stop("`mincompare` argument cannot be NA.", call. = F)
  }else if(mincompare != as.integer(mincompare)){
    stop("`mincompare` argument must be an integer.", call. = F)
  }else if(mincompare < 1 || mincompare > bounds[layer + 1]){
    if(layer == 0)
      stop("`mincompare` must be between 1 and ncol(data) for agent layer.", call. = F)
    else if(layer == 1)
      stop("`mincompare` must be between 1 and nrow(data) for symbolic layer.", call. = F)
  }else{
    mincompare <- as.integer(mincompare)
  }


  # Check the value of the similarity metric.
  # 0 Manhattan distance
  # 1 Euclidean distance
  if(is.null(metric)){
    metric <- as.integer(0)
  }else if(!is.character(metric)){
    stop("`metric` argument must be a character string.", call. = F)
  }else if(length(metric) != 1){
    stop("`metric` argument must be of length 1.", call. = F)
  }else if(is.na(metric)){
    stop("`metric` argument cannot be NA.", call. = F)
  }else if(metric %in% c("manhattan", "Manhattan")){
    metric <- as.integer(0)
  }else if(metric %in% c("euclidean", "Euclidean")){
    metric <- as.integer(1)
  }else{
    stop("`metric` option \"", metric, "\" unrecognised; expecting \"Manhattan\" or \"Euclidean\".", call. = F)
  }


  # Check the value of the `count` argument, the number of similarity
  # thresholds over which we compute lcc and the average degree. `count` is
  # set to at least 3, such that when `count` is
  # 3, thresholds are 0, 0.5 and 1
  # 4, thresholds are 0, 0.3..., 0.6... and 1
  # 5, thresholds are 0, 0.25, 0.5, 0.75 and 1
  # and so on. If supplied 0, 1 or 2, warn that we compute the profile over at
  # least 3 values, and continue, setting count to 3.
  if(is.null(count)){
    count <- as.integer(21)
  }else if(!is.numeric(count)){  
    stop("`count` argument must be an integer.", call. = F)
  }else if(length(count) != 1){
    stop("`count` argument must be of length 1.", call. = F)
  }else if(is.na(count)){  
    stop("`count` argument cannot be NA.", call. = F)
  }else if(count != as.integer(count)){
    stop("`count` argument must be an integer.", call. = F)
  }else if(count < 3){
    warning("Setting `count` to 3.", call. = F)
    count <- as.integer(3)
  }else{
    count <- as.integer(count)
  }


  profile <- .Call(
    "rmake_threshold_profile",
    data,
    layer,
    mincompare,
    metric,
    count
  )
}
