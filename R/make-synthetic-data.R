#' Outputs a synthetic survey using a simple model
#'
#' @description
#' **Note that a major update to the surveygraph package, as well as documentation on this page, is planned for July 2025.**
#'
#' `make_synthetic_data()` outputs a synthetic survey, generated using a simple, stochastic
#'   model of polarisation.
#' 
#' @return
#' A data frame corresponding to a survey.
#' 
#' @param nrow The number of rows in the survey
#' @param ncol The number of columns in the survey
#' @param minority The fraction of nodes in the smaller of the two polarised groups
#' @param polarisation The degree of polarisation among the system's agents
#' @param correlation Probability that group item corresponds to polarisation
#' @param scale Range of the Likert scale
#' @param seed Seed value for random number generation.
#' 
#' @export
#' @examples
#' S <- make_synthetic_data(200, 8)
#' @export
make_synthetic_data <- function(
  nrow, 
  ncol,
  minority = NULL,
  polarisation = NULL,
  correlation = NULL,
  scale = NULL,
  seed = NULL,
  ...
){
  # TODO:
  # rename `scale` to `likert`, deprecation warning, use ... argument for this
  # accept both UK and US spelling, polarisation and polarization. use ... argument for this.

  # Validate `nrow` argument.
  if(!is.numeric(nrow))
    stop("`nrow` argument must be a non-negative integer.", call. = F)

  if(length(nrow) != 1)
    stop("`nrow` argument must be of length 1.", call. = F)

  if(is.na(nrow))
    stop("`nrow` argument cannot be NA.", call. = F)

  if(nrow < 0)
    stop("`nrow` argument must be a non-negative integer.", call. = F)

  if(nrow != as.integer(nrow))
    stop("`nrow` argument must be a non-negative integer.", call. = F)

  nrow <- as.integer(nrow)


  # Validate `ncol` argument.
  if(!is.numeric(ncol))
    stop("`ncol` argument must be a non-negative integer.", call. = F)

  if(length(ncol) != 1)
    stop("`ncol` argument must be of length 1.", call. = F)

  if(is.na(ncol))
    stop("`ncol` argument cannot be NA.", call. = F)

  if(ncol < 0)
    stop("`ncol` argument must be a non-negative integer.", call. = F)

  if(ncol != as.integer(ncol))
    stop("`ncol` argument must be a non-negative integer.", call. = F)

  ncol <- as.integer(ncol)


  # Validate `minority` argument.
  if(is.null(minority))
    minority <- 0.5

  if(!is.numeric(minority))
    stop("`minority` argument must be between 0 and 0.5, inclusive.", call. = F)

  if(length(minority) != 1)
    stop("`minority` argument must be of length 1.", call. = F)

  if(is.na(minority))
    stop("`minority` argument cannot be NA.", call. = F)

  if(minority < 0 || minority > 1)
    stop("`minority` argument must be between 0 and 0.5, inclusive.", call. = F)

  if(minority > 0.5){
    warning("'minority' argument must be between 0 and 0.5 inclusive, taking 1 - minority.")
    minority <- 1 - minority
  }

  minority <- as.numeric(minority)


  # Validate `polarisation` argument.
  if(is.null(polarisation))
    polarisation <- 0

  if(!is.numeric(polarisation))
    stop("`polarisation` argument must be between 0 and 1, inclusive.", call. = F)

  if(length(polarisation) != 1)
    stop("`polarisation` argument must be of length 1.", call. = F)

  if(is.na(polarisation))
    stop("`polarisation` argument cannot be NA.", call. = F)

  if(polarisation < 0 || polarisation > 1)
    stop("`polarisation` argument must be between 0 and 1, inclusive.", call. = F)

  polarisation <- as.numeric(polarisation)


  # Validate `correlation` argument.
  if(is.null(correlation))
    correlation <- 0.85

  if(!is.numeric(correlation))
    stop("`correlation` argument must be between 0 and 1, inclusive.", call. = F)

  if(length(correlation) != 1)
    stop("`correlation` argument must be of length 1.", call. = F)

  if(is.na(correlation))
    stop("`correlation` argument cannot be NA.", call. = F)

  if(correlation < 0 || correlation > 1)
    stop("`correlation` argument must be between 0 and 1, inclusive.", call. = F)

  correlation <- as.numeric(correlation)


  # Validate `scale` argument.
  if(is.null(scale))
    scale <- 10

  if(!is.numeric(scale))
    stop("`scale` argument must be a positive integer.", call. = F)

  if(length(scale) != 1)
    stop("`scale` argument must be of length 1.", call. = F)

  if(is.na(scale))
    stop("`scale` argument cannot be NA.", call. = F)

  if(scale < 0)
    stop("`scale` argument must be a positive integer.", call. = F)

  if(scale != as.integer(scale))
    stop("`scale` argument must be a positive integer.", call. = F)

  scale <- as.integer(scale)


  # Construct survey.
  data <- data.frame(matrix(NA, nrow = nrow, ncol = ncol))

  # TODO: if ncol = 1, drop the `group` column.
  # TODO: remember you're currently allowing zero for nrow and ncol...

  # avgresponse is between 0 and (scale - 1) / 2
  avgresponse <- (scale - 1) / 2 - ((scale - 1) / 2) * polarisation

  for(i in 1:nrow){
    # Group membership column, takes a binary value.
    # TODO: need to fix correlation, corr of 0 needs to be random, not anticorrelated
    if(runif(1) < correlation)
      data[i, 1] <- 0
    else
      data[i, 1] <- 1

    if(i > minority * nrow + 1e-6)
      data[i, 1] <- 1 - data[i, 1]

    # Likert columns, take on values between 1 and `scale`.
    for(j in 2:ncol){
      repeat{
        data[i, j] <- 1 + as.numeric(rpois(1, avgresponse))
        if(data[i, j] >= 1 && data[i, j] <= scale)
          break
      }
      
      if(i > minority * nrow + 1e-6)
        data[i, j] <- scale - data[i, j] + 1
    }
  }

  # colnames "group" "item_1" "item_2" ... "item_ncol"
  cnames <- c("group")
  for(i in 1:(ncol - 1))
    cnames <- append(cnames, paste(c("item_", i), collapse="")) 

  colnames(data) <- cnames

  return(data)
}
