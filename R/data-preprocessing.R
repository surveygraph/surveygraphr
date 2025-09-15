#' Implements simple pre-processing of survey data. These routines are for
#' convenience, as everything here can be easily implemented even by beginner R
#' users.
#'
#' @description
#' **Note that a major update to the surveygraph package, as well as documentation on this page, is planned for July 2025.**
#'
#' `data_preprocess()` outputs the survey formatted as required.
#' 
#' @return
#' A data frame corresponding to the edge list of the specified network. It
#' contains three columns named 
#' 
#' @param data A data frame corresponding to a survey
#' @param likert Specifies the range of the Likert scale contained in `data`.  
#' @param dummycode flag that indicates whether we dummycode data.
#' 
#' @export
#' @examples
#' S <- make_synthetic_data(20, 5)
data_preprocess <- function(
  data, 
  likert = NULL,
  dummycode = NULL
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


  # Set infinite value entries of numerical columns to NA.
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


  # Verify `likert` argument.
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
      stop("`dummycode` must be an atomic vector")

    if(!(is.numeric(dummycode) || is.logical(dummycode)))
      stop("`dummycode` must be numeric or logical")

    # Verify that dimensions match survey
    if(length(dummycode) != ncol(data))
      stop("`dummycode` length must equal number of columns in survey dataframe")
  
    # If dummycode is logical and NA entries present, set them to false
    if(is.logical(dummycode) && anyNA(dummycode)){
      warning("Setting NA entries of `dummycode` to FALSE")
      dummycode[is.na(dummycode)] <- FALSE
    }

    # If dummycode is numeric, set any entry that is not 1 to 0
    if(is.numeric(dummycode) && any(!(dummycode %in% c(0, 1)))){
      warning("Setting entries of `dummycode` that aren't 1 or 0 to 0")
      dummycode[dummycode != 1] <- 0

      # Coerce to logical vector.
      dummycode <- as.logical(dummycode)
    }
	}


  # Now we can process each column in `data` according to its type, and the
  # `likert` and `dummycode` arguments provided.
	datacopy <- do.call(
		cbind,
		lapply(seq_along(data), function(i){
			if(is.numeric(data[[i]])){
				numeric_handling(data[i], likert[[i]], dummycode[[i]])
			}else if(is.character(data[[i]])){
				character_handling(data[i], likert[[i]], dummycode[[i]])
			}else if(is.logical(data[[i]])){
				logical_handling(data[i], likert[[i]], dummycode[[i]])
			}else{
				other_handling(data[i], likert[[i]], dummycode[[i]])
			}
		})
	)
	datacopy
}


# Dummy-coding utility function, used in numeric_handling() and character_handling().
dummycoding <- function(c, vals){
	uniquevals <- unique(vals)
	dcode <- as.data.frame(matrix(nrow = length(vals), ncol = 0))
	dcodenames <- character(0)
	for(i in uniquevals){
		if(!is.na(i)){
			m <- match(vals, i)
			m[is.na(m)] <- 0

			dcode <- data.frame(dcode, m)
			dcodenames <- c(dcodenames, paste(paste(c, "_", sep = ""), i, sep = ""))
		}
	}
	colnames(dcode) <- dcodenames
	dcode
}


# Normalising utility function, used in numeric_handling(). Maps data to the
# interval [0, 1], depending on whether a Likert range is supplied. If there's only 
# a single finite value, set it to 0.5.
#
# Recall that either both values in a column of `likert` are finite, or neither
# are.
normalise <- function(data, likert){
  dmin <- NA
  dmax <- NA

  # Get upper and lower bounds of column, depends on whether finite `likert`
  # values are provided.
  if(!is.na(likert[[1]])){
    dmin <- likert[[1]]
    dmax <- likert[[2]]
  }else{
    foundfinite <- FALSE
    for(i in seq_along(data[[1]])){
      if(!is.na(data[[1]][[i]])){
        if(!foundfinite){
          dmin <- data[[1]][[i]]
          dmax <- data[[1]][[i]]
          foundfinite <- TRUE
        }else{
          if(data[[1]][[i]] < dmin) dmin <- data[[1]][[i]]
          if(data[[1]][[i]] > dmax) dmax <- data[[1]][[i]]
        }
      }
    }
  }

  # Map the interval [dmin, dmax] to [0, 1].
  if(!is.na(dmin)){
    if(dmin < dmax){
      for(i in seq_along(data[[1]])){
        if(!is.na(data[[1]][[i]]))
          data[[1]][[i]] <- (data[[1]][[i]] - dmin) / abs(dmax - dmin)
      }
    }else{
      for(i in seq_along(data[[1]])){
        if(!is.na(data[[1]][[i]]))
          data[[1]][[i]] <- 0.5
      }
    }
  }
  data
}


# Pre-processing of numeric dataframe columns. data, likert and dummycode are
# single-columns. Recall data is a dataframe, likert and dummycode are atomic
# vectors.
numeric_handling <- function(data, likert, dummycode){
  datacopy <- data
  dummyvals <- character(nrow(data))
	dummyvals[] <- NA

	if(dummycode){
		if(any(data[[1]] != floor(data[[1]]), na.rm = TRUE))
			warning("dummycoding a numeric column that contains non-integer values")
  }

  for(i in seq_along(data[[1]])){
    if(!is.na(data[[1]][[i]])){
      if(!is.na(likert[[1]])){
        if(data[[1]][[i]] < likert[[1]] || data[[1]][[i]] > likert[[2]]){
          datacopy[[1]][[i]] <- NA
          if(dummycode) dummyvals[i] <- as.character(data[[1]][[i]])
        }
      }else{
        if(dummycode) dummyvals[i] <- as.character(data[[1]][[i]])
      }
    }
  }

  datacopy <- normalise(datacopy, likert)
	dcode <- dummycoding(colnames(data[1]), dummyvals)

	datareturn <- as.data.frame(matrix(nrow = nrow(data), ncol = 0))
	if(is.na(likert[[1]]) && dummycode)
  	datareturn <- data.frame(dcode)
	else
  	datareturn <- data.frame(datacopy, dcode)

  datareturn
}


# Pre-processing of character dataframe columns. data, likert and dummycode are
# single-columns.
character_handling <- function(data, likert, dummycode){
  datacopy <- data
  dummyvals <- character(nrow(data))
	dummyvals[] <- NA

	if(!is.na(likert[[1]]))
		warning("ignoring likert flag for character vector")

	if(!dummycode){
		warning("dummycode flag not set for character vector, coercing to numeric")
		datacopy[[1]] <- as.numeric(datacopy[[1]])
	}else{
		for(i in seq_along(data[[1]])){
			if(!is.na(data[[1]][[i]]))
				dummyvals[i] <- as.character(data[[1]][[i]])
		}
	}

	dcode <- dummycoding(colnames(data[1]), dummyvals)

	datareturn <- as.data.frame(matrix(nrow = nrow(data), ncol = 0))
	if(dummycode){
  	datareturn <- dcode
	}else{
  	datareturn <- datacopy
	}

  datareturn
}


# Pre-processing of logical dataframe columns.
logical_handling <- function(data, likert, dummycode){
  datacopy <- data

	if(!is.na(likert[[1]]))
		warning("ignoring likert flag for logical vector")

	if(dummycode)
		warning("ignoring dummycode flag for logical vector")

	datacopy[[1]] <- as.numeric(datacopy[[1]])
	datacopy
}


# Pre-processing of dataframe columns of miscellaneous types.
other_handling <- function(data, likert, dummycode){
  datacopy <- data

	warning("package doesn't specifically handly this type, simply coercing to numeric")

	datacopy[[1]] <- as.numeric(datacopy[[1]])
	datacopy
}
