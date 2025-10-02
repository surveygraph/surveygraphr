#' Illustrates how network properties vary with the similarity threshold
#'
#' @description
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
#'
#' @param layer A string flag specifying the type of network to be extracted,
#'
#'   - `"agent"` produces the network corresponding to the agents, which we assume
#'   to be rows in `data`
#'
#'   - `"symbolic"` produces the network corresponding to the symbols, or items,
#'   which we assume to be columns in `data`
#'
#' @param comparisons An integer, minimum number of comparisons for valid distance.
#'
#' @param metric A string option describing the similarity metric to be used.
#'
#' @param count The number of threshold values to include in the description.
#'
#' @param limits Specify the limits of the Likert range in during a data preprocessing step.
#'
#' @param dummycode Specify whether to apply dummycoding during a data preprocessing step.
#'
#' @param ... Used to handle alternative argument spellings.
#'
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
  comparisons = NULL,
  metric = NULL,
  count = NULL,
  limits = NULL,
  dummycode = NULL,
  ...
){
  dots <- list(...)

  if(length(dots) > 0){
    warning("Unused arguments in ...: ", paste(names(dots), collapse = ", "), call. = F)
  }


  # Checking of data, limits and dummycode is done in data_preprocess()
  data <- data_preprocess(data, limits, dummycode)


  # TODO: avoid the redundancy with these checks and make_projection()
  # TODO: rename Euclidean and Manhattan to RMSE and MAE


  # Check `layer`, whose options are "agent" or "symbolic", and map to
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


  # Check `comparisons`, the minimum number of numerical pairwise comparisons for
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


  # Check `metric`, whose options are 'manhattan' and 'euclidean', mapping to
  # 0 for `manhattan`
  # 1 for `euclidean`
  if(is.null(metric))
    metric <- "manhattan"

  if(!is.character(metric))
    stop("`metric` must be a character string.", call. = F)

  if(length(metric) != 1)
    stop("`metric` must be of length 1.", call. = F)

  if(is.na(metric))
    stop("`metric` cannot be NA.", call. = F)

  if(metric %in% c("manhattan", "Manhattan"))
    metric <- as.integer(0)
  else if(metric %in% c("euclidean", "Euclidean"))
    metric <- as.integer(1)
  else
    stop("`metric` option \"", metric, "\" unrecognised; expecting \"Manhattan\" or \"Euclidean\".", call. = F)


  # Check `count`, the number of similarity thresholds over which we compute
  # the size of the lcc and the average degree.
  if(is.null(count))
    count <- as.integer(21)

  if(!is.numeric(count))
    stop("`count` must be an integer.", call. = F)

  if(length(count) != 1)
    stop("`count` must be of length 1.", call. = F)

  if(!is.finite(count))
    stop("`count` must be finite (not NA, NaN, Inf or -Inf).", call. = F)

  if(count != as.integer(count))
    stop("`count` must be an integer.", call. = F)

  if(count < 3){
    warning("Setting `count` to 3.", call. = F)
    count <- as.integer(3)
  }

  count <- as.integer(count)


  profile <- .Call(
    "rmake_threshold_profile",
    data,
    layer,
    comparisons,
    metric,
    count
  )
}
