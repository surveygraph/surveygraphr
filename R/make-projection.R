#' Outputs the survey projection onto the agent or symbolic layer
#'
#' @description
#' **Note that a major update to the surveygraph package, as well as documentation here, is planned for July 2025.**
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
#' @param threshold_method A string flag specifying how edges are selected in
#' the network representation.
#' 
#'   - `"raw_similarity"` means we remove all edges whose weight, meaning node
#'   similarity, is below a specified threshold.
#' 
#'   - `"target_lcc"` finds the value of the threshold that results in the network
#'   whose largest connected component is as close as possible to a specified
#'   value. In general a range of thresholds will satisfy this condition, and we
#'   choose the upper limit of this range. 
#' 
#'   - `"target_ad"` finds the value of the threshold that results in the network
#'   whose average degree is as close as possible to a specified value.
#' 
#' @param method_value A utility variable that we interpret according to the
#'   `threshold_method` chosen.
#' 
#'   - If `threshold_method = "raw_similarity"`, then `method_value` is
#'   interpreted as the similarity threshold, and thus is in the range `[-1, 1]`.
#'   A value of -1 means no edges are removed, and a value of 1 means all edges
#'   are removed.
#' 
#'   - If `threshold_method = "target_lcc"`, then `method_value` is interpreted as
#'   the desired fractional size of the largest connected component, in the range
#'   `[0, 1]`. E.g., when set to 0, no nodes are connected, and if set to 1, the
#'   network is as sparse as possible while remaining fully connected.
#' 
#'   - If `threshold_method = "target_ad"`, then `method_value` is interpreted as
#'   the desired average degree. We assume that `method_value` is normalised to
#'   the range `[0, 1]` When `method_value = 0`, then no nodes are connected, and
#'   if `method_value = 1`, the network is complete, meaning it contains every
#'   possible edge.
#' @param centre If `TRUE`, we shift edge weights from `[0, 1]` to `[-1, 1]`. 
#'   Defaults to FALSE, as most network analysis applications require positive
#'   edge weights.
#' @param dummycode flag that indicates whether we dummycode data.
#' @param likert Specifies the range of the Likert scale contained in `data`.  
#' @param mincomps The minimum number of valid comparisons that must be
#' made when computing the similarity between rows or columns in the `data`. If at
#' least one of the entries in the fields being compared is NA, then the
#' comparison is invalid.
#' @param similarity_metric This currently has just one allowed value, namely the
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
  threshold_method = NULL, 
  method_value = NULL, 
  centre = NULL, 
  dummycode = NULL,
  likert = NULL,
  mincomps = NULL,
  similarity_metric = NULL,
  verbose = NULL
){

  # TODO do cleaning of data, ie coerce to numeric etc, set strings to NA
  if(!is.data.frame(data)){
    stop("Input data must be provided as a data frame.")
  }else if(ncol(data) == 0 || nrow(data) == 0){
    stop("Data frame cannot be empty.")
  }


  # check that layer is either agent or symbolic
  # 0 agent
  # 1 symbolic
  if(is.null(layer)){
    layer = as.integer(0)
  }else if(layer == "agent"){
    layer = as.integer(0)
  }else if(layer == "symbolic"){
    layer = as.integer(1)
  }else if(layer != "agent" && layer != "symbolic"){
    layer = as.integer(0)
    message("Warning: `layer` needs to be either \"agent\" or \"symbolic\". Defaulting to \"agent\".")
  }


  # set threshold method
  # 0 lcc
  # 1 average degree
  # 2 raw similarity
  if(is.null(threshold_method)){
    threshold_method = as.integer(0)
  }else if(threshold_method == "target_lcc"){
    threshold_method = as.integer(0)
  }else if(threshold_method == "target_ad"){
    threshold_method = as.integer(1)
  }else if(threshold_method == "raw_similarity"){
    threshold_method = as.integer(2)
  }else{
    threshold_method = as.integer(0)
    message("Warning: `threshold_method` needs to be either \"target_lcc\", \"target_ad\" or \"raw_similarity\". Defaulting to \"target_lcc\".")
  }


  # method value
  if(is.null(method_value)){
    if(threshold_method == 0){
      method_value <- as.numeric(1)
    }else if(threshold_method == 1){
      method_value <- as.numeric(1)
    }else{
      method_value <- as.numeric(1)
    }
  }


  # Centre edge weights about zero, default to false.
  if(is.null(centre)){
    centre <- as.integer(0)
  }else if(centre == TRUE){
    centre <- as.integer(1)
  }else if(centre == FALSE){
    centre <- as.integer(0)
  }else{
    warning("`centre` must either be TRUE or FALSE. Defaulting to FALSE.")
  }


  # dummycode
  if(is.null(dummycode)){
    dummycode = as.integer(0)
  }else if(dummycode == 0){
    dummycode = as.integer(0)
  }else{
    dummycode = as.integer(1)
  }


  # check the likert flag
  if(is.null(likert)){
    # TODO if dummycode is set, dummy code everything
    if(dummycode == 0){
      likert <- data.frame(
        # TODO need to make sure data has already been coerced to numeric
        apply(data, 2, min, na.rm = TRUE),
        apply(data, 2, max, na.rm = TRUE)
      )
    }else{
      likert <- NA
    }
  }else{
    # dimension of likert must match data
    if(nrow(likert) != ncol(data))
      stop("Likert and data dimensions incompatible (likert ", nrow(likert), ", data ", ncol(data), ")")

    if(!is.data.frame(likert))
      stop("Likert must be a data frame.")

    if(ncol(likert) != 2)
      stop("Likert must contain two columns, the min and max respectively of each data column.")

    # TODO check that first column is min and second max.

    if(!all(sapply(likert, is.numeric))){
      # print out this info?
      #if(any(is.character(likert)))
      #if(any(is.na(likert)))
      #if(any(is.logical(likert)))
      stop("Likert must only contain numerical values.")
    }

    tmplikert <- data.frame(
      minval = apply(data, 2, min, na.rm = TRUE),
      maxval = apply(data, 2, max, na.rm = TRUE)
    )

    outlt <- which(tmplikert$minval < likert[[1]])
    outgt <- which(tmplikert$maxval > likert[[2]])
    outofrangeind <- unique(c(outlt, outgt))

    outofrange <- as.integer(0)
    if(length(outofrangeind) > 0)
      outofrange <- as.integer(1)

    # if outofrange and not dummycode, report that we're setting outliers to NA.
    if(outofrange && !dummycode){
      message("Note: columns ", paste(outofrangeind, collapse = ", ")," contain entries outside the specified range. Setting them to NA.")
      lapply(outofrangeind, function(col){
        outofrangevals <- data[
          data[, col] < likert[[1]][col] | data[, col] > likert[[2]][col], col
        ]
        message("  ", col, ": ", paste(outofrangevals, collapse = ", "))
      })
      data[data < likert[[1]][col(data)] | data > likert[[2]][col(data)]] <- NA
    }

    # if outofrange and dummycode, report which columns and entries will be affected.
    if(outofrange && dummycode){
      message("Columns ", paste(outofrangeind, collapse = ", ")," contain entries outside the specified range, and will be dummy-coded.")
      lapply(outofrangeind, function(col){
        outofrangevals <- data[
          data[, col] < likert[[1]][col] | data[, col] > likert[[2]][col], col
        ]
        message(col, ": ", paste(outofrangevals, collapse = ", "))
      })
    }

    # if not outofrange and dummycode, report that no dummy coding will take place.
    if(!outofrange && dummycode){
      warning("No dummy coding will take place, as no data lies outside the acceptable range.")
    }
  }


  if(!is.null(verbose)){
    if(verbose)
      print(data)
  }


  # minimum comparisons
  if(is.null(mincomps)){
    mincomps = as.integer(ncol(data) / 2)
  }


  # check the value of similarity_matric
  if(is.null(similarity_metric)){
    if(layer == 0){
      similarity_metric <- as.integer(0)
    }else if(layer == 1){
      similarity_metric <- as.integer(0)
    }
  }else if(similarity_metric == "manhattan"){
    similarity_metric <- as.integer(0)
  }else{
    message("overriding similarity metric to manhattan distance") 
    similarity_metric <- as.integer(0)
  }


  e <- .Call(
    "rmake_projection", 
    data, 
    layer,
    threshold_method,
    method_value, 
    likert,     ############## shouldn't require this
    dummycode,  ############## shouldn't require this
    mincomps,
    similarity_metric,
    centre)
}
