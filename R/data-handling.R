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
  dummycode = NULL,
  likert = NULL,
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

	# Verify dummycode input.
  if(!is.null(dummycode)){
		if(as.numeric(dummycode) != 0 && as.numeric(dummycode) != 1){
			stop("dummycode should equal 0 or 1, or be coercible to those values.")
		}
		dummycode <- as.numeric(dummycode)
	}

	# Verify likert input.
  if(!is.null(likert)){
		# Must be a dataframe with dimensions 2 x ncol(data)
		if(!is.data.frame(likert))
			stop("likert must be a dataframe")

		# Dimensions must match dataframe
		if(nrow(likert) != 2)
			stop("likert must have two rows, the min and max value of each column")

		if(ncol(likert) != ncol(data))
			stop("likert must have as many columns as the survey dataframe")

		# Must be numerical.
		if(!all(sapply(likert, is.numeric)))
			warning("each column in likert should be numeric")

		# Each column should be non-decreasing.
		if(!all(likert[1, ] <= likert[2, ]))
			warning("each column in likert should be non-decreasing")
	}


  datacopy <- as.data.frame(
    lapply(
      seq_along(data),
      function(i){
        if(is.numeric(data[[i]])) numeric_handling(data[[i]], likert = likert[[i]])
      }
    )
  )

	datacopy
}


numeric_handling <- function(
  data, 
  dummycode = NULL,
  likert = NULL
){
	if(!is.vector(data) || !is.numeric(data)) 
		stop("data here needs to be a vector, and it needs to be numeric")

	if(is.null(dummycode) && !is.null(likert)){
		if(!is.vector(likert)) 
			stop("likert here needs to be a vector")

		data[data < likert[1] | data > likert[2]] <- NA
	}

	#if(!is.null(dummycode) && is.null(likert)){
	#	# warn about decimal points if there are any
	#	#model.matrix(~ f - 1)
	#}

	data <- data.frame(as.numeric(data))
}
