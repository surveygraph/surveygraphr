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
  if(is.null(likert)){
		likert <- as.data.frame(matrix(NA_real_, nrow = 2, ncol = ncol(data)))
	}else{
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
  if(is.null(dummycode)){
		dummycode <- logical(ncol(data))
	}else{
    # Verify that dummycode is a vector
    if(!is.atomic(dummycode))
      stop("dummycode must be an atomic vector")

    if(!(is.numeric(dummycode) || is.logical(dummycode)))
      stop("dummycode must be numeric or logical")

    # Verify that dimensions match survey
    if(length(dummycode) != ncol(data))
      stop("dummycode length must equal number of columns in survey dataframe")
  
    # If dummycode is logical and NA entries present, set them to false
    if(is.logical(dummycode) && anyNA(dummycode)){
      warning("setting NA entries of dummycode to FALSE")
      dummycode[is.na(dummycode)] <- FALSE
    }

    # If dummycode is numeric, set any entry that is not 1 to 0
    if(is.numeric(dummycode) && any(!(dummycode %in% c(0, 1)))){
      warning("setting entries of dummycode that aren't 1 or 0 to 0")
      dummycode[dummycode != 1] <- 0

      # Coerce to logical vector.
      dummycode <- as.logical(dummycode)
    }
	}

	datacopy <- do.call(
		cbind,
		lapply(seq_along(data), function(i){
			if(is.numeric(data[[i]]))
				numeric_handling(data[i], likert[[i]], dummycode[[i]])
		})
	)
	datacopy
}


numeric_handling <- function(data, likert, dummycode){
	if(ncol(data) != 1)
		stop("need to pass a data frame consisting of one column")
	
  datacopy <- data
  dummyset <- character(0)
  for(i in seq_along(data[[1]])){
    if(!is.na(data[[1]][[i]])){
      if(!is.na(likert[[1]])){
        if(data[[1]][[i]] < likert[[1]] || data[[1]][[i]] > likert[[2]]){
          datacopy[[1]][[i]] <- NA
          if(dummycode)
            dummyset <- c(dummyset, as.character(data[[1]][[i]]))
        }else{
          if(dummycode)
            dummyset <- c(dummyset, as.character("intercept"))
				}
      }else{
        if(dummycode)
          dummyset <- c(dummyset, as.character(data[[1]][[i]]))
      }
    }
  }

	# construct dummy-coding matrix
  # NOTE need to warn about decimal points if there are any
	dcode <- as.data.frame(matrix(nrow = nrow(data), ncol = 0))
	for(i in unique(dummyset)){
		if(i != "intercept"){
			m <- match(dummyset, i)
			m[is.na(m)] <- 0

			dcode <- data.frame(dcode, m)
		}
	}

	#print("dummyset is:")
	#print(dummyset)
	#print("")
	#print("factor is:")
	#print(factor(dummyset))
	#print("dcode is:")
	#print(dcode)

  datareturn <- data.frame(datacopy, dcode)
  datareturn
}
