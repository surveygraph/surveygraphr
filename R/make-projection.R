#' Outputs the survey projection onto the agent or symbolic layer
#'
#' @description
#' **Note that a major update to the surveygraph package, as well as documentation on this page, is planned for July 2025.**
#'
#' `make_projection()` outputs the agent or symbolic network corresponding
#' to a survey, i.e. the row or column projection.
#'
#' @return
#' A data frame corresponding to the edge list of the specified network. It
#' contains three columns named
#'
#'   - `u`, the first node adjacent to the edge
#'
#'   - `v`, the second node adjacent to the edge, and
#'
#'   - `weight`, the similarity between nodes `u` and `v`
#'
#' @param data A data frame corresponding to a survey
#' @param layer A string flag specifying which layer to project
#'
#'   - `"agent"` produces the network corresponding to the agents, which we assume
#'   to be rows in `data`
#'
#'   - `"symbolic"` produces the network corresponding to the symbols, or items,
#'   which we assume to be columns in `data`
#'
#' @param method A string flag specifying how edges are thresholded in
#' the network representation.
#'
#'   - `"similarity"` means we remove all edges whose weight, meaning node
#'   similarity, is below a threshold specified by `methodval`.
#'
#'   - `"lcc"` finds the value of the threshold that results in the network
#'   whose largest connected component is as close as possible to a specified
#'   value. In general a range of thresholds will satisfy this condition, and we
#'   choose the upper limit of this range. As such, `"lcc"` provided is a target.
#'
#'   - `"avgdegree"` finds the value of the threshold that results in the network
#'   whose average degree is as close as possible to a specified value. Like
#'   `"lcc"`, this is a target.
#'
#' @param methodval A utility variable that we interpret according to the
#'   `method` chosen.
#'
#'   - If `method = "similarity"`, then `methodval` is
#'   interpreted as the similarity threshold, and thus is in the range `[0, 1]`.
#'   A value of 0 means no edges are removed, and a value of 1 means all edges
#'   are removed.
#'
#'   - If `method = "lcc"`, then `methodval` is interpreted as
#'   the desired fractional size of the largest connected component, in the range
#'   `[0, 1]`. E.g., when set to 0, no nodes are connected, and if set to 1, the
#'   network is as sparse as possible while remaining fully connected.
#'
#'   - If `method = "avgdegree"`, then `methodval` is interpreted as
#'   the desired average degree. We assume that `methodval` is normalised to
#'   the range `[0, 1]` When `method_value = 0`, then no nodes are connected, and
#'   if `method_value = 1`, the network is complete, meaning it contains every
#'   possible edge.
#' @param centre If `TRUE`, we shift edge weights from `[0, 1]` to `[-1, 1]`.
#'   Defaults to FALSE, as most network analysis applications require positive
#'   edge weights.
#' @param dummycode flag that indicates whether we dummycode data.
#' @param likert Specifies the range of the Likert scale contained in `data`.
#' @param comparisons The minimum number of valid comparisons that must be
#' made when computing the similarity between rows or columns in the `data`. If at
#' least one of the entries in the fields being compared is NA, then the
#' comparison is invalid.
#' @param metric This currently has just one allowed value, namely the
#'   Manhattan distance, which is the default.
#' @param verbose This is a debugging flag that prints out survey data after a
#' pre-processing step, but before being supplied to the C++ routines that compute
#' the network representation.
#'
#' @export
#' @examples
#' S <- make_synthetic_data(20, 5)
make_projection <- function(
  data,
  layer = NULL,
  method = NULL,
  methodval = NULL,
  comparisons = NULL,
  metric = NULL,
  likert = NULL,
  dummycode = NULL,
  bootreps = NULL,
  bootval = NULL,
  bootseed = NULL,
  ...
){
  dots <- list(...)

  if(!is.null(dots$threshold_method)){
    warning("`threshold_method` argument is deprecated and will be removed in future versions; use `method`.", call. = F)
    method <- dots$threshold_method
    dots$threshold_method <- NULL
  }

  if(!is.null(dots$method_value)){
    warning("`method_value` argument is deprecated and will be removed future versions; use `methodval`.", call. = F)
    methodval <- dots$method_value
    dots$method_value <- NULL
  }

  if(!is.null(dots$similarity_metric)){
    warning("`similarity_metric` argument is deprecated and will be removed in future versions; use `methodval`.", call. = F)
    metric <- dots$similarity_metric
    dots$similarity_metric <- NULL
  }

  if(!is.null(dots$centre)){
    warning("`centre` argument is deprecated; outputting edge weights in range 0 to 1.", call. = F)
    dots$centre <- NULL
  }

  if(length(dots) > 0){
    warning("Unused arguments in ...: ", paste(names(dots), collapse = ", "), call. = F)
  }


  # TODO: need to check Inf values for double arguments. use is.finite()
  # TODO: just use if loops, don't need ifelse
  # TODO: explain default values in documentation. choices are conservative
  # TODO: deprecate "target_lcc", "target_ad", "raw_similarity"?


  # Checking of data, likert and dummycode is done in data_preprocess()
  data <- data_preprocess(data, likert, dummycode)


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


  # Check that method is either lcc, avgdegree or similarity, mapping to
  # 0 for lcc
  # 1 for avgdegree
  # 2 for similarity
  if(is.null(method)){
    method <- as.integer(0)
  }else if (!is.character(method)){
    stop("`method` argument must be a character string.", call. = F)
  }else if(length(method) != 1){
    stop("`method` argument must be of length 1.", call. = F)
  }else if(is.na(method)){
    stop("`method` argument cannot be NA.", call. = F)
  }else if(method %in% c("lcc", "target_lcc", "l")){
    method <- as.integer(0)
  }else if(method %in% c("avgdegree", "target_avgdegree", "target_ad", "a")){
    method <- as.integer(1)
  }else if(method %in% c("similarity", "rawsimilarity", "raw_similarity", "s")){
    method <- as.integer(2)
  }else{
    stop("`method` option \"", method, "\" unrecognised.", call. = F)
  }


  # Check methodval, a utility parameter whose interpretation depends on the
  # value of method. For all sparsification methods, methodval must be between
  # 0 and 1.
  if(is.null(methodval)){
    defaults <- c(1, 0, 1)
    methodval <- defaults[method + 1]
  }else if(!is.numeric(methodval)){
    stop("`methodval` argument must be a numerical value.", call. = F)
  }else if(length(methodval) != 1){
    stop("`methodval` argument must be of length 1.", call. = F)
  }else if(is.na(methodval)){
    stop("`methodval` argument cannot be NA.", call. = F)
  }else{
    if(method == 0){
      if(methodval < 0 || methodval > 1){
        stop("`methodval` must be between 0 and 1 inclusive for `lcc` method.", call. = F);
      }
    }else if(method == 1){
      if(methodval < 0 || methodval > 1){
        stop("`methodval` must be between 0 and 1 inclusive for `avgdegree` method.", call. = F);
      }
    }
  }


  # Check comparisons, the minimum number of numerical pairwise comparisons for
  # computing similarity.
  bounds <- c(ncol(data), nrow(data))
  if(is.null(comparisons)){
    defaults <- as.integer(c(ceiling(ncol(data) / 2), ceiling(nrow(data) / 2)))
    comparisons <- defaults[layer + 1]
  }else if(!is.numeric(comparisons)){  
    stop("`comparisons` argument must be an integer.", call. = F)
  }else if(length(comparisons) != 1){
    stop("`comparisons` argument must be of length 1.", call. = F)
  }else if(is.na(comparisons)){  
    stop("`comparisons` argument cannot be NA.", call. = F)
  }else if(comparisons != as.integer(comparisons)){
    stop("`comparisons` argument must be an integer.", call. = F)
  }else if(comparisons < 1 || comparisons > bounds[layer + 1]){
    if(layer == 0)
      stop("`comparisons` must be between 1 and ncol(data) for agent layer.", call. = F)
    else if(layer == 1)
      stop("`comparisons` must be between 1 and nrow(data) for symbolic layer.", call. = F)
  }else{
    comparisons <- as.integer(comparisons)
  }


  # Check the value of the similarity metric.
  # 0 Manhattan distance
  # 1 Euclidean distance
  # TODO: rename RMSE and MAE, since that is what they actually are
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


  # Check that correct combination of bootstrapping arguments are set.
  if(is.null(bootseed)){
    if(is.null(bootreps) && !is.null(bootval)){
      stop("`bootreps` argument must be set if `bootval` is set.", call. = F)
    }else if(!is.null(bootreps) && is.null(bootval)){
      stop("`bootval` argument must be set if `bootreps` is set.", call. = F)
    }
  }else{ 
    if(is.null(bootreps) && is.null(bootval)){
      stop("`bootval` argument must be set if `bootseed` is set.", call. = F)
    }else if(!is.null(bootreps) && is.null(bootval)){
      stop("`bootval` argument must be set if `bootseed` is set.", call. = F)
    }else if(!is.null(bootreps) && !is.null(bootval)){
      stop("`bootreps` argument must be NULL if `bootseed` is set.", call. = F)
    }
  }


  if(is.null(bootreps)){
    bootreps <- as.integer(1)
  }else if(!is.numeric(bootreps)){
    stop("`bootreps` argument must be a positive integer.", call. = F)
  }else if(length(bootreps) != 1){
    stop("`bootreps` argument must be of length 1.", call. = F)
  }else if(is.na(bootreps)){
    stop("`bootreps` argument cannot be NA.", call. = F)
  }else if(bootreps < 1){
    stop("`bootreps` argument must be a positive integer.", call. = F)
  }else if(bootreps != as.integer(bootreps)){
    stop("`bootreps` argument must be a positive integer.", call. = F)
  }else{
    bootreps <- as.integer(bootreps)
  }

  if(is.null(bootval)){
    bootval <- as.double(1)
  }else if(!is.numeric(bootval)){
    stop("`bootval` argument must be between 0 and 1, inclusive.", call. = F)
  }else if(length(bootval) != 1){
    stop("`bootval` argument must be of length 1.", call. = F)
  }else if(is.na(bootval)){
    stop("`bootval` argument cannot be NA.", call. = F)
  }else if(!is.finite(bootval)){
    stop("`bootval` argument must be between 0 and 1, inclusive.", call. = F)
  }else if(bootval < 0 || bootval > 1){
    stop("`bootval` argument must be between 0 and 1, inclusive.", call. = F)
  }else{
    bootval <- as.double(bootval)
  }

  if(is.null(bootseed)){
    bootseed <- integer()
  }else if(!is.numeric(bootseed)){
    stop("`bootseed` argument must be an integer vector.", call. = F)
  }else if(any(is.na(bootseed))){
    stop("`bootseed` argument must not contain NAs.", call. = F)
  }else if(length(bootseed) == 0){
    stop("`bootseed` argument, if provided, must contain at least one integer.", call. = F)
  }else if(!all(bootseed == as.integer(bootseed))){
    stop("`bootseed` argument must only contain integers.", call. = F)
  }else{
    bootseed <- as.integer(bootseed)
    bootreps <- length(bootseed)
  }


  e <- .Call(
    "rmake_projection",
    data,
    layer,
    method,
    methodval,
    comparisons,
    metric,
    bootreps,
    bootval,
    bootseed 
  )
}
