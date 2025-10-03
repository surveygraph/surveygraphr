#' Outputs the survey projection onto the agent or symbolic layer
#'
#' @description
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
#'
#' @param comparisons The minimum number of valid comparisons that must be
#' made when computing the similarity between rows or columns in the `data`. If at
#' least one of the entries in the fields being compared is NA, then the
#' comparison is invalid.
#'
#' @param metric This currently has just one allowed value, namely the
#'   Manhattan distance, which is the default.
#'
#' @param limits Specifies the limits of the Likert scale contained in `data`.
#'
#' @param dummycode flag that indicates whether we dummycode data.
#'
#' @param bootreps The number of bootstrap realisations to perform. If not
#'   specified, bootstrapping is not carried out.
#'
#' @param bootval A sampling probability used when bootstraping. In particular,
#' it provides the probability of sampling a given survey entry in a given
#' bootstrapping step. With probability 1 - bootval, that entry is set to NA.
#'
#' @param bootseed A random number generator seed used when bootstrapping.
#'   Mainly used for testing, but maybe useful for reproducibility in general.
#'
#' @param centre If `TRUE`, we shift edge weights from `[0, 1]` to `[-1, 1]`.
#'   Defaults to FALSE, as most network analysis applications require positive
#'   edge weights.
#'
#' @param ... Mostly used to handle deprecated arguments, and arguments with
#'   alternative spellings.
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
  limits = NULL,
  dummycode = NULL,
  bootreps = NULL,
  bootval = NULL,
  bootseed = NULL,
  centre = NULL,
  ...
){
  dots <- list(...)

  if(!is.null(dots$threshold_method)){
    warning("`threshold_method` is deprecated and will be removed in future versions; use `method`.", call. = F)
    method <- dots$threshold_method
    dots$threshold_method <- NULL
  }

  if(!is.null(dots$method_value)){
    warning("`method_value` is deprecated and will be removed future versions; use `methodval`.", call. = F)
    methodval <- dots$method_value
    dots$method_value <- NULL
  }

  if(!is.null(dots$similarity_metric)){
    warning("`similarity_metric` is deprecated and will be removed in future versions; use `metric`.", call. = F)
    metric <- dots$similarity_metric
    dots$similarity_metric <- NULL
  }

  if(!is.null(dots$likert)){
    warning("`likert` is deprecated and will be removed in future versions; use `limits`.", call. = F)
    limits <- dots$likert
    dots$likert <- NULL
  }

  if(!is.null(dots$center)){
    centre <- dots$center
    dots$center <- NULL
  }

  if(length(dots) > 0){
    warning("Unused arguments in ...: ", paste(names(dots), collapse = ", "), call. = F)
  }


  # Checking of data, limits and dummycode is done in data_preprocess()
  data <- data_preprocess(data, limits, dummycode)


  # Check that layer is either "agent" or "symbolic", mapping to
  # 0 for agent
  # 1 for symbolic
  if(is.null(layer))
    layer <- "agent"

  if(!is.character(layer))
    stop("`layer` must be a character string.", call. = F)

  if(length(layer) != 1)
    stop("`layer` must be of length 1.", call. = F)

  if(is.na(layer))
    stop("`layer` cannot be NA.", call. = F)

  if(layer %in% c("agent", "Agent", "a"))
    layer <- as.integer(0)
  else if(layer %in% c("symbolic", "Symbolic", "s"))
    layer <- as.integer(1)
  else
    stop("`layer` option \"", layer, "\" unrecognised.", call. = F)


  # Check that method is either lcc, avgdegree or similarity, mapping to
  # 0 for lcc
  # 1 for avgdegree
  # 2 for similarity
  if(is.null(method))
    method <- "lcc"

  if(!is.character(method))
    stop("`method` must be a character string.", call. = F)

  if(length(method) != 1)
    stop("`method` must be of length 1.", call. = F)

  if(is.na(method))
    stop("`method` cannot be NA.", call. = F)

  if(method %in% c("lcc", "target_lcc", "l"))
    method <- as.integer(0)
  else if(method %in% c("avgdegree", "target_avgdegree", "target_ad", "a"))
    method <- as.integer(1)
  else if(method %in% c("similarity", "raw_similarity", "s"))
    method <- as.integer(2)
  else
    stop("`method` option \"", method, "\" unrecognised.", call. = F)


  # Check methodval, a utility parameter whose interpretation depends on the
  # value of method. For all sparsification methods, methodval must be between
  # 0 and 1.
  if(is.null(methodval)){
    if(method == 0) methodval <- 1
    if(method == 1) methodval <- 0
    if(method == 2) methodval <- 1
  }

  if(!is.numeric(methodval))
    stop("`methodval` must be a numerical value.", call. = F)

  if(length(methodval) != 1)
    stop("`methodval` must be of length 1.", call. = F)

  if(!is.finite(methodval))
    stop("`methodval` must be finite (not NA, NaN, Inf or -Inf).", call. = F)

  if(method == 0){
    if(methodval < 0 || methodval > 1){
      stop("`methodval` must be between 0 and 1 inclusive for `lcc` method.", call. = F);
    }
  }else if(method == 1){
    if(methodval < 0 || methodval > 1){
      stop("`methodval` must be between 0 and 1 inclusive for `avgdegree` method.", call. = F);
    }
  }


  # Check comparisons, the minimum number of numerical pairwise comparisons for
  # computing similarity.
  if(is.null(comparisons)){
    if(layer == 0) comparisons <- as.integer(ceiling(ncol(data) / 2))
    if(layer == 1) comparisons <- as.integer(ceiling(nrow(data) / 2))
  }

  if(!is.numeric(comparisons))
    stop("`comparisons` must be an integer.", call. = F)

  if(length(comparisons) != 1)
    stop("`comparisons` must be of length 1.", call. = F)

  if(!is.finite(comparisons))
    stop("`comparisons` must be finite (not NA, NaN, Inf or -Inf).", call. = F)

  if(comparisons != as.integer(comparisons))
    stop("`comparisons` must be an integer.", call. = F)

  if(layer == 0 && (comparisons < 1 || comparisons > ncol(data)))
    stop("`comparisons` must be between 1 and ncol(data) for agent layer.", call. = F)

  if(layer == 1 && (comparisons < 1 || comparisons > nrow(data)))
    stop("`comparisons` must be between 1 and nrow(data) for symbolic layer.", call. = F)

  comparisons <- as.integer(comparisons)


  # Check the value of the similarity metric.
  # 0 Manhattan distance
  # 1 Euclidean distance
  if(is.null(metric))
    metric <- "manhattan"

  if(!is.character(metric))
    stop("`metric` must be a character string.", call. = F)

  if(length(metric) != 1)
    stop("`metric` must be of length 1.", call. = F)

  if(is.na(metric))
    stop("`metric` cannot be NA.", call. = F)

  if(metric %in% c("manhattan", "Manhattan", "mae", "MAE"))
    metric <- as.integer(0)
  else if(metric %in% c("euclidean", "Euclidean", "rmse", "RMSE"))
    metric <- as.integer(1)
  else
    stop("`metric` option \"", metric, "\" unrecognised; expecting \"Manhattan\" or \"Euclidean\".", call. = F)


  # Check that correct combination of bootstrapping arguments are set.
  if(is.null(bootseed)){
    if(is.null(bootreps) && !is.null(bootval))
      stop("`bootreps` must be set if `bootval` is set.", call. = F)

    if(!is.null(bootreps) && is.null(bootval))
      stop("`bootval` must be set if `bootreps` is set.", call. = F)
  }else{
    if(is.null(bootreps) && is.null(bootval))
      stop("`bootval` must be set if `bootseed` is set.", call. = F)

    if(!is.null(bootreps) && is.null(bootval))
      stop("`bootval` must be set if `bootseed` is set.", call. = F)

    if(!is.null(bootreps) && !is.null(bootval))
      stop("`bootreps` must be NULL if `bootseed` is set.", call. = F)
  }


  # Check bootreps, the number of bootstrap realisations.
  if(is.null(bootreps))
    bootreps <- as.integer(1)

  if(!is.numeric(bootreps))
    stop("`bootreps` must be a positive integer.", call. = F)

  if(length(bootreps) != 1)
    stop("`bootreps` must be of length 1.", call. = F)

  if(!is.finite(bootreps))
    stop("`bootreps` must be finite (not NA, NaN, Inf or -Inf).", call. = F)

  if(bootreps < 1)
    stop("`bootreps` must be a positive integer.", call. = F)

  if(bootreps != as.integer(bootreps))
    stop("`bootreps` must be a positive integer.", call. = F)

  bootreps <- as.integer(bootreps)


  # Check bootval, a utility bootstrapping parameter, used as a probability.
  if(is.null(bootval))
    bootval <- as.double(1)

  if(!is.numeric(bootval))
    stop("`bootval` must be between 0 and 1, inclusive.", call. = F)

  if(length(bootval) != 1)
    stop("`bootval` must be of length 1.", call. = F)

  if(!is.finite(bootval))
    stop("`bootval` must be finite (not NA, NaN, Inf or -Inf).", call. = F)

  if(bootval < 0 || bootval > 1)
    stop("`bootval` must be between 0 and 1, inclusive.", call. = F)

  bootval <- as.double(bootval)


  # Check bootseed, the seed value or values used in bootstrapping.
  if(is.null(bootseed)){
    bootseed <- integer()
  }else{
    if(!is.numeric(bootseed))
      stop("`bootseed` must be an integer vector.", call. = F)

    if(!all(is.finite(bootseed)))
      stop("`bootseed` must be finite (not NA, NaN, Inf or -Inf).", call. = F)

    if(length(bootseed) == 0)
      stop("`bootseed`, if provided, must contain at least one integer.", call. = F)

    if(!all(bootseed == as.integer(bootseed)))
      stop("`bootseed` must only contain integers.", call. = F)

    bootseed <- as.integer(bootseed)

    bootreps <- length(bootseed)
  }


  # Check the `centre` agrument, a flag that determines whether weights are in
  # the range [0, 1] or [-1, 1]. We allow numeric and logical types, e.g. 0, 1 or
  # FALSE, TRUE, and coerce to logical otherwise.
  if(is.null(centre))
    centre <- FALSE

  if(length(centre) != 1)
    stop("`centre` must be of length 1.", call. = F)

  if(!is.finite(centre))
    stop("`centre` must be finite after coercion (not NA, NaN, Inf or -Inf).", call. = F)

  if(as.logical(centre) != TRUE && as.logical(centre) != FALSE)
    stop("`centre` must coerce to TRUE or FALSE.", call. = F)

  if(!is.logical(centre))
    warning("`centre` will be coerced to logical; setting ", centre, " to ", as.logical(centre), ".", call. = F)

  centre <- as.logical(centre)


  # Calls the C++ method rmake_projection, see the files init.cc and
  # make_projection_pilot.cc.
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


  # Centre edge weights around 0; shift and scale from [0, 1] to [-1, 1].
  if(length(e$weight) > 0){
    if(centre){
      e$weight <- e$weight - 0.5
      e$weight <- e$weight * 2
    }
  }
  e
}
