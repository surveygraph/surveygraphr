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
#' @param correlation Probability that group item corresponds to polarisation
#' @param polarisation The degree of polarisation among the system's agents
#' @param likert Range of the Likert scale
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
  correlation = NULL,
  polarisation = NULL,
  likert = NULL,
  seed = NULL,
  ...
){
  # TODO:
  # rename `scale` to `likert`, deprecation warning, use ... argument for this
  # accept both UK and US spelling, polarisation and polarization. use ... argument for this.
  # your validations errors and warnings currently don't catch Infs... use
  #     is.finite()? if Inf is passed, it's as a double, since integers can't be Inf
  dots <- list(...)

  if(!is.null(dots$scale)){
    warning("`scale` argument is deprecated and will be removed in future versions; use `likert`.", call. = F)
    likert <- dots$scale
    dots$scale <- NULL
  }

  if(!is.null(dots$polarization)){
    polarisation <- dots$polarization
    dots$polarization <- NULL
  }

  if(length(dots) > 0)
    warning("Unused arguments in ...: ", paste(names(dots), collapse = ", "), ".", call. = F)


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
    warning("`minority` argument must be between 0 and 0.5, inclusive; taking 1 - minority.")
    minority <- 1 - minority
  }

  minority <- as.numeric(minority)


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


  # Validate `likert` argument.
  if(is.null(likert))
    likert <- 10

  if(!is.numeric(likert))
    stop("`likert` argument must be a positive integer.", call. = F)

  if(length(likert) != 1)
    stop("`likert` argument must be of length 1.", call. = F)

  if(is.na(likert))
    stop("`likert` argument cannot be NA.", call. = F)

  if(likert < 1)
    stop("`likert` argument must be a positive integer.", call. = F)

  if(likert != as.integer(likert))
    stop("`likert` argument must be a positive integer.", call. = F)

  likert <- as.integer(likert)


  # Validate `seed` argument.
  seedflag <- FALSE
  if(is.null(seed))
    seed <- 0 
  else
    seedflag <- TRUE

  if(!is.numeric(seed))
    stop("`seed` argument must be an integer.", call. = F)

  if(length(seed) != 1)
    stop("`seed` argument must be of length 1.", call. = F)

  if(is.na(seed))
    stop("`seed` argument cannot be NA.", call. = F)

  if(seed != as.integer(seed))
    stop("`seed` argument must be an integer.", call. = F)

  seed <- as.integer(seed)

  if(seedflag) set.seed(seed)

  # Construct survey.
  data <- data.frame(matrix(0, nrow = nrow, ncol = ncol))

  if(nrow > 0){
    for(i in 1:nrow){
      if(ncol > 1){
        # Group membership column, takes a binary value.
        var <- runif(1)
        if(var < (0.5 * (correlation + 1)))
          data[i, 1] <- 0
        else
          data[i, 1] <- 1

        if(i > minority * nrow + 1e-6)
          data[i, 1] <- 1 - data[i, 1]

        # Likert columns, take on values between 1 and `likert`.
        for(j in 2:ncol){
          shape <- 5  # Leftover shape parameter in beta distribution.
          mu <- 0.5 * (polarisation + 1)
          alpha <- mu * shape 
          beta <- (1 - mu) * shape
          val <- 1 + (likert - 1) * rbeta(1, alpha, beta)
          data[i, j] <- 1 + likert - round(val)
          
          if(i > minority * nrow + 1e-6)
            data[i, j] <- 1 + likert - data[i, j]
        }
      }
    }
  }

  # colnames "group", "item_1", "item_2", ..., "item_ncol"
  cnames <- character()
  if(ncol > 0){
    cnames <- c("group")
    if(ncol > 1){
      for(i in 1:(ncol - 1))
        cnames <- append(cnames, paste(c("item_", i), collapse="")) 
    }
    colnames(data) <- cnames
  }

  data
}
