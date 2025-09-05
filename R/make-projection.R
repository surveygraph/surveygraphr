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
#' @param mincompare The minimum number of valid comparisons that must be
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
  mincompare = NULL,
  metric = NULL,
  likert = NULL,
  dummycode = NULL,
  #bootreps = NULL,
  #bootval = NULL,
  ...
){

  dots <- list(...)

  if(!is.null(dots$threshold_method)){
    warning("Argument `threshold_method` is deprecated and will be removed in future versions; use `method` instead.", call. = F)
    method <- dots$threshold_method
    dots$threshold_method <- NULL
  }

  if(!is.null(dots$method_value)){
    warning("Argument `method_value` is deprecated and will be removed future versions; use `methodval` instead.", call. = F)
    methodval <- dots$method_value
    dots$method_value <- NULL
  }

  if(!is.null(dots$similarity_metric)){
    warning("Argument `similarity_metric` is deprecated and will be removed in future versions; use `methodval` instead.", call. = F)
    metric <- dots$similarity_metric
    dots$similarity_metric <- NULL
  }

  if(!is.null(dots$centre)){
    warning("Argument `centre` is deprecated; outputting edge weights in range 0 to 1.", call. = F)
    dots$centre <- NULL
  }

  if(length(dots) > 0){
    warning("Unused arguments in ...: ", paste(names(dots), collapse = ", "), call. = F)
  }

  # Checking of data, likert and dummycode is done in data_preprocess()
  data <- data_preprocess(data, likert, dummycode)


  # Check that layer is either "agent" or "symbolic", mapping to
  # 0 for agent
  # 1 for symbolic
  if(is.null(layer)){
    layer <- as.integer(0)
  }else if(!is.character(layer)){
    warning("`layer` must be a character string; defaulting to \"agent\".")
		layer <- as.integer(0)
	}else if(layer == "agent"){
    layer <- as.integer(0)
  }else if(layer == "symbolic"){
    layer <- as.integer(1)
  }else{
    warning("`layer` option \"", layer, "\" unrecognised; defaulting to \"agent\".")
    layer <- as.integer(0)
  }


  # Check that method is either lcc, avgdegree or similarity, mapping to
  # 0 for lcc
  # 1 for avgdegree
  # 2 for similarity
  # TODO check that it's a character string!!!!!!!
  if(is.null(method)){
    method <- as.integer(0)
	}else if (!is.character(method)){
    warning("`method` must be a character string; defaulting to \"lcc\".")
    method <- as.integer(0)
  }else if(method %in% c("lcc", "target_lcc")){
    method <- as.integer(0)
  }else if(method %in% c("avgdegree", "target_avgdegree", "target_ad")){
    method <- as.integer(1)
  }else if(method %in% c("similarity", "rawsimilarity", "raw_similarity")){
    method <- as.integer(2)
  }else{
    warning("`method` option \"", method, "\" unrecognised; defaulting to \"lcc\".")
    method <- as.integer(0)
  }


  # Check methodval, a utility parameter whose interpretation depends on the
  # value of method. For all sparsification methods, methodval must be between
  # 0 and 1.

  # default values for methods 0, 1 and 2	
	defaults <- c(1, 0, 1)
	if(is.null(methodval)){
		methodval <- defaults[method + 1]
	}else if(!is.numeric(methodval)){
		warning("`methodval` must be a numerical value between 0 and 1, inclusive.", call. = F)
		methodval <- defaults[method + 1]
	}else if(methodval < 0 || methodval > 1){
		warningtext <- c(
			"`methodval` must be between 0 and 1; defaulting to 1 for lcc method.",
			"`methodval` must be between 0 and 1; defaulting to 0 for avgdegree method.",
			"`methodval` must be between 0 and 1; defaulting to 1 for similarity method."
		)
		warning(warningtext[method + 1], call. = F)
		methodval <- defaults[method + 1]
	}


  # Check mincompare, the minimum number of numerical pairwise comparisons for
  # computing similarity.
	defaults <- c(ceiling(ncol(data) / 2), ceiling(nrow(data) / 2))
	defaults <- as.integer(defaults)
	bounds <- c(ncol(data), nrow(data))

  if(is.null(mincompare)){
		mincompare <- defaults[layer + 1]
	}else if(!is.numeric(mincompare)){	
		warningtext <- c(
			"Expecting an integer for `mincompare`; defaulting to ceiling(ncol(data) / 2) for agent layer.",
			"Expecting an integer for `mincompare`; defaulting to ceiling(nrow(data) / 2) for symbolic layer."
		)
		warning(warningtext[layer + 1], call. = F)
		mincompare <- defaults[layer + 1]
  }else if(mincompare < 1 || mincompare > bounds[layer + 1]){
		warningtext <- c(
			"Expecting `mincompare` between 1 and ncol(data) for agent layer; defaulting to ceiling(ncol(data) / 2).",
			"Expecting `mincompare` between 1 and nrow(data) for symbolic layer; defaulting to ceiling(nrow(data) / 2)."
		)
		warning(warningtext[layer + 1], call. = F)
		mincompare <- defaults[layer + 1]
  }else{
		mincompare <- as.integer(mincompare)
	}


  # Check the value of the similarity metric.
  # 0 Manhattan distance
  # 1 Euclidean distance
  if(is.null(metric)){
    metric <- as.integer(0)
	}else if(!is.character(metric)){
    warning("`metric` must be a character string; defaulting to \"Manhattan\".")
    metric <- as.integer(0)
  }else if(metric %in% c("manhattan", "Manhattan")){
    metric <- as.integer(0)
  }else if(metric %in% c("euclidean", "Euclidean")){
    metric <- as.integer(0)
  }else{
    warning("`metric` option \"", metric, "\" unrecognised; defaulting to \"Manhattan\".")
    metric <- as.integer(0)
  }

	print("mincompare is : ")
	print(mincompare)

  e <- .Call(
    "rmake_projection", 
    data, 
    layer,
    method,
    methodval, 
    mincompare,
    metric
  )
}
