#' Implements simple pre-processing of survey data. These routines are for
#' convenience, as everything here can be easily implemented even by beginner R
#' users.
#'
#' @description
#' **Note that a major update to the surveygraph package, as well as documentation on this page, is planned for July 2025.**
#'
#' `data_handling()` outputs the survey formatted as required.
#' 
#' @return
#' A data frame corresponding to the edge list of the specified network. It
#' contains three columns named 
#' 
#' @param data A data frame corresponding to a survey
#' @param dummycode flag that indicates whether we dummycode data.
#' @param likert Specifies the range of the Likert scale contained in `data`.  
#' @param verbose This is a debugging flag that prints out survey data after a
#' pre-processing step, but before being supplied to the C++ routines that compute
#' the network representation.
#' 
#' @export
#' @examples
#' S <- make_synthetic_data(20, 5)
data_handling <- function(
  data, 
  likert = NULL,
  dummycode = NULL,
  verbose = NULL
){

  # Output error if data not a dataframe, important because we assume uniform
  # column types in the following.
  if(!is.data.frame(data)){
    stop("Input data must be a dataframe.")
  }
  
  # Output warning if dataframe is empty, and proceed.
  if(ncol(data) == 0 || nrow(data) == 0){
    stop("Input dataframe cannot be empty.")
  }

  # Set infinite value entries to NA.
  colsnumeric <- sapply(data, is.numeric)
  if(length(colsnumeric) > 0){
    data[colsnumeric] <- lapply(
      data[colsnumeric], 
      function(col){
        col[is.infinite(col)] <- NA
        col
      }
    )
  }

  # Verify likert input.
  if(!is.null(likert)){
    # Must be a dataframe
    if(!is.data.frame(likert))
      stop("likert must be a dataframe")

    # Dimensions must match dataframe
    if(nrow(likert) != 2)
      stop("likert must have two rows, the min and max value of each column")

    if(ncol(likert) != ncol(data))
      stop("likert must have as many columns as the survey dataframe")

    # Columns must be numerical or logical
    if(!all(sapply(likert, function(col) is.numeric(col) || is.logical(col)))){
      warning("setting likert columns that aren't numeric or logical to logical NA")

      likert[] <- lapply(likert, function(col){
        if(is.numeric(col) || is.logical(col)) 
          col 
        else 
          rep(NA, nrow(likert))
      })
    }

    # Numeric columns must be finite, i.e. no NA, NaN or Inf
    if(any(sapply(likert, function(col) is.numeric(col) && any(!is.finite(col))))){
      warning("at least one numerical likert column invalid, setting to logical NA")

      likert[] <- lapply(likert, function(col){
        if(is.numeric(col)){
          if(any(!is.finite(col))) 
            rep(NA, nrow(likert))
          else 
            col
        }else{
          col
        }
      })
    }

    # Logical columns must be c(NA, NA)
    if(any(sapply(likert, function(col) is.logical(col) && any(!is.na(col))))){
      warning("at least one logical likert column contains non NA entries, setting to NA")

      likert[] <- lapply(likert, function(col){
        if(is.logical(col)){
          rep(NA, nrow(likert))
        }else{
          col
        }
      })
    }

    if(any(sapply(likert, function(col) if(is.numeric(col)){col[1] > col[2]}else{FALSE}))){
      warning("each numerical column in likert should be non-decreasing, setting to NA")
      # TODO just reverse instead of making NA?

      likert[] <- lapply(likert, function(col){
        if(is.numeric(col)){
          if(col[1] > col[2])
            rep(NA, nrow(likert))
          else
            col
        }else{
          col
        }
      })
    }
  }


  # Verify dummycode input.
  if(!is.null(dummycode)){
    # Verify that dummycode is a vector
    if(!is.vector(dummycode))
      stop("dummycode must be a vector")

    # Verify that dimensions match survey
    if(length(dummycode) != ncol(data))
      stop("dummycode length must equal number of columns in survey dataframe")
  
    # Verify that dummycode is logical
    if(!is.logical(dummycode))
      stop("dummycode must be logical")
  }

  data

  #datacopy <- as.data.frame(
  #  lapply(
  #    seq_along(data),
  #    function(i){
  #      if(is.numeric(data[[i]])){
  #        if(is.null(likert) && is.null(dummycode))
  #          numeric_handling(data[[i]])

  #        if(!is.null(likert) && is.null(dummycode))
  #          numeric_handling(data[[i]], likert = likert[[i]])

  #        if(is.null(likert) && !is.null(dummycode))
  #          numeric_handling(data[[i]], dummycode = dummycode[[i]])

  #        if(!is.null(likert) && !is.null(dummycode))
  #          numeric_handling(data[[i]], likert = likert[[i]], dummycode = dummycode[[i]])
  #      }
  #    }
  #  )
  #)

  #datacopy
}


numeric_handling <- function(
  data, 
  likert = NULL,
  dummycode = NULL
){
  if(!is.vector(data) || !is.numeric(data)) 
    stop("data here needs to be a vector, and it needs to be numeric")

  if(is.null(dummycode) && !is.null(likert)){
    if(!is.vector(likert)) 
      stop("likert here needs to be a vector")

    data[data < likert[1] | data > likert[2]] <- NA
  }

  #if(!is.null(dummycode) && is.null(likert)){
  # # warn about decimal points if there are any
  # #model.matrix(~ f - 1)
  #}

  data <- data.frame(as.numeric(data))
}
