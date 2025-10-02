#' Outputs a synthetic survey using a simple model
#'
#' @description
#' `data_preprocess()` outputs a synthetic survey, generated using a simple, stochastic
#'   model of polarisation.
#'
#' @return
#' A data frame corresponding to a survey.
#'
#' @param data The number of rows in the survey
#' @param limits The number of columns in the survey
#' @param dummycode The fraction of nodes in the smaller of the two polarised groups
#'
#' @export
#' @examples
#' S <- make_synthetic_data(200, 8)
data_preprocess <- function(
  data,
  limits = NULL,
  dummycode = NULL
){

  # TODO: stress in documentation that columns are handled independently.
  # TODO: put call. = F in all error and warning messages

  # TODO:
  # add ... ellipses
  # add a verbose argument for pre normalisation

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
  # TODO: also NaN
  #colsnumeric <- sapply(data, is.numeric)
  #if(length(colsnumeric) > 0){
  #  data[colsnumeric] <- lapply(
  #    data[colsnumeric],
  #    function(col){
  #      col[is.infinite(col)] <- NA
  #      col
  #    }
  #  )
  #}


  # Verify `limits` argument.
  if(is.null(limits)){
    limits <- as.data.frame(matrix(NA_real_, nrow = 2, ncol = ncol(data)))
  }else{
    # Must be a dataframe.
    if(!is.data.frame(limits))
      stop("limits must be a dataframe")

    # Dimensions must match `data` dataframe.
    if(nrow(limits) != 2)
      stop("limits must have two rows, the min and max value of each column")

    if(ncol(limits) != ncol(data))
      stop("limits must have as many columns as the survey dataframe")

    # Columns must be numerical or logical.
    if(!all(sapply(limits, function(col) is.numeric(col) || is.logical(col)))){
      warning("setting limits columns that aren't numeric or logical to logical NA")

      limits[] <- lapply(limits, function(col){
        if(is.numeric(col) || is.logical(col))
          col
        else
          rep(NA, nrow(limits))
      })
    }

    # Numeric columns must be finite, i.e. no NA, NaN or Inf
    if(any(sapply(limits, function(col) is.numeric(col) && any(!is.finite(col))))){
      warning("at least one numerical limits column invalid, setting to logical NA")

      limits[] <- lapply(limits, function(col){
        if(is.numeric(col)){
          if(any(!is.finite(col)))
            rep(NA, nrow(limits))
          else
            col
        }else{
          col
        }
      })
    }

    # Logical columns must be c(NA, NA)
    if(any(sapply(limits, function(col) is.logical(col) && any(!is.na(col))))){
      warning("at least one logical limits column contains non NA entries, setting to NA")

      limits[] <- lapply(limits, function(col){
        if(is.logical(col)){
          rep(NA, nrow(limits))
        }else{
          col
        }
      })
    }

    if(any(sapply(limits, function(col) if(is.numeric(col)){col[1] > col[2]}else{FALSE}))){
      warning("each numerical column in limits should be non-decreasing, setting to NA")
      # TODO just reverse instead of making NA?

      limits[] <- lapply(limits, function(col){
        if(is.numeric(col)){
          if(col[1] > col[2])
            rep(NA, nrow(limits))
          else
            col
        }else{
          col
        }
      })
    }
  }


  # Check `dummycode`.
  if(is.null(dummycode)){
    # Default to FALSE.
    dummycode <- logical(ncol(data))
  }else{
    # Verify that dummycode is a vector
    if(!is.atomic(dummycode))
      stop("`dummycode` must be an atomic vector")

    # TODO: only accept logical
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
  # `limits` and `dummycode` arguments provided.
  datacopy <- do.call(
    cbind,
    lapply(seq_along(data), function(i){
      if(is.numeric(data[[i]])){
        processing_numeric(data[i], limits[[i]], dummycode[[i]])
      }else if(is.character(data[[i]])){
        processing_character(data[i], limits[[i]], dummycode[[i]])
      }else if(is.logical(data[[i]])){
        processing_logical(data[i], limits[[i]], dummycode[[i]])
      }else{
        processing_other(data[i], limits[[i]], dummycode[[i]])
      }
    })
  )
  datacopy
}


# Dummy-coding utility function. `c` is a character vector of length one, and
# `vals` is a character vector of
dummycoding <- function(c, vals){
  uniquevals <- sort(unique(vals))
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


# Normalising utility function, used in processing_numeric(). Maps data to the
# interval [0, 1], depending on whether a Likert range is supplied. If there's only
# a single finite value, set it to 0.5.
#
# Recall that either both values in a column of `limits` are finite, or neither
# are.
normalise <- function(data, limits){
  dmin <- NA
  dmax <- NA

  # Get upper and lower bounds of column, depends on whether finite `limits`
  # values are provided.
  if(!is.na(limits[[1]])){
    dmin <- limits[[1]]
    dmax <- limits[[2]]
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
        if(!is.na(data[[1]][[i]])){
          data[[1]][[i]] <- (data[[1]][[i]] - dmin) / abs(dmax - dmin)
        }
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


# Pre-processing of numeric dataframe columns. `data`, `limits` and `dummycode`
# are single-columns. Here was assume that `data` is a one-column dataframe,
# and therefore has a column name, and that `limits` and `dummycode` are atomic
# vectors.
#
# Dummycodes non-finite numerical values; NA, NaN and Inf, or sets them to NA.
processing_numeric <- function(data, limits, dummycode){
  datacopy <- data
  dummyvals <- character(nrow(data))
  dummyvals[] <- NA_character_

  if(dummycode){
    if(any(data[[1]] != floor(data[[1]]), na.rm = TRUE))
      warning("Dummycoding a numeric column that contains non-integer values.", call. = F)
  }

  for(i in seq_along(data[[1]])){
    if(is.finite(data[[1]][[i]])){
      if(!is.na(limits[[1]])){
        if(data[[1]][[i]] < limits[[1]] || data[[1]][[i]] > limits[[2]]){
          datacopy[[1]][[i]] <- NA_real_
          if(dummycode) dummyvals[i] <- as.character(data[[1]][[i]])
        }
      }else{
        if(dummycode) dummyvals[i] <- as.character(data[[1]][[i]])
      }
    }else{
      if(dummycode){
        if(is.nan(data[[1]][[i]]))
          dummyvals[i] <- "NaN"
        else if(is.na(data[[1]][[i]]))
          dummyvals[i] <- "NA"
        else
          dummyvals[i] <- as.character(data[[1]][[i]])
      }
      datacopy[[1]][[i]] <- NA_real_
    }
  }

  datacopy <- normalise(datacopy, limits)
  dcode <- dummycoding(colnames(data[1]), dummyvals)

  datareturn <- as.data.frame(matrix(nrow = nrow(data), ncol = 0))
  if(is.na(limits[[1]]) && dummycode)
    datareturn <- data.frame(dcode)
  else
    datareturn <- data.frame(datacopy, dcode)

  datareturn
}


# Pre-processing of character dataframe columns. data, limits and dummycode are
# single-columns.
processing_character <- function(data, limits, dummycode){
  datacopy <- data
  dummyvals <- character(nrow(data))
  dummyvals[] <- NA

  if(!is.na(limits[[1]]))
    warning("Ignoring `limits` flag for character vector.", call. = F)

  if(!dummycode)
    warning("Dummycode flag not set for character vector; coercing to numeric.", call. = F)

  if(!dummycode){
    datacopy[[1]] <- suppressWarnings(as.numeric(datacopy[[1]]))
    datacopy <- normalise(datacopy, limits)
  }else{
    for(i in seq_along(data[[1]])){
      if(!is.na(data[[1]][[i]]))
        dummyvals[i] <- as.character(data[[1]][[i]])
      else
        dummyvals[i] <- "NA"
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
processing_logical <- function(data, limits, dummycode){
  datacopy <- data

  if(!is.na(limits[[1]]))
    warning("ignoring limits flag for logical vector")

  if(dummycode)
    warning("ignoring dummycode flag for logical vector")

  datacopy[[1]] <- as.numeric(datacopy[[1]])
  datacopy
}


# Pre-processing of dataframe columns of miscellaneous types.
processing_other <- function(data, limits, dummycode){
  datacopy <- data

  warning("package doesn't specifically handly this type, simply coercing to numeric")

  datacopy[[1]] <- as.numeric(datacopy[[1]])
  datacopy
}
