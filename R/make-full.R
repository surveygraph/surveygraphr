#' @export
make_full <- function(data, 
                      layer = NULL,
                      threshold_method = NULL, 
                      method_value = NULL, 
                      centre = NULL, 
                      likert = NULL,
                      dummycode = NULL,
                      mincomps = NULL,
                      similarity_metric = NULL){

  # TODO do cleaning of data, ie coerce to numeric etc, set strings to NA
  if(!is.data.frame(data)){
    stop("Data must be in a data frame.")
  }else if(ncol(data) == 0 || nrow(data) == 0){
    stop("Dataframe must contain some data.")
  }

  # check that layer is either agent or symbolic
  # 0 agent
  # 1 symbolic
  if(is.null(layer)){
    layer = as.integer(0)
  }else if(layer == "agent"){
    layer = as.integer(0)
  }else if(layer == "symbolic"){
    layer = as.integer(1)
  }else if(layer != "agent" && layer != "symbolic"){
    layer = as.integer(0)
    message("Warning: `layer` needs to be either \"agent\" or \"symbolic\". Defaulting to \"agent\".")
  }

  # set threshold method
  # 0 lcc
  # 1 average degree
  # 2 raw similarity
  if(is.null(threshold_method)){
    threshold_method = as.integer(0)
  }else if(threshold_method == "target_lcc"){
    threshold_method = as.integer(0)
  }else if(threshold_method == "target_ad"){
    threshold_method = as.integer(1)
  }else if(threshold_method == "raw_similarity"){
    threshold_method = as.integer(0)
    message("Warning: `threshold_method` needs to be either \"target_lcc\", \"target_ad\" or \"raw_similarity\". Defaulting to \"target_lcc\".")
  }

  # method value
  if(is.null(method_value)){
    if(threshold_method == 0){
      method_value <- as.numeric(1)
    }else if(threshold_method == 1){
      method_value <- as.numeric(1)
    }else{
      method_value <- as.numeric(1)
    }
  }

  # check the value of centre
  if(is.null(centre)){
    if(layer == 0){
      centre <- as.integer(1)
    }else if(layer == 1){
      centre <- as.integer(0)
    }
  }else if(centre == TRUE){
    centre <- as.integer(1)
  }else if(centre == FALSE){
    centre <- as.integer(0)
  }

  # dummycode
  if(is.null(dummycode)){
    dummycode = as.integer(0)
  }else if(dummycode == 0){
    dummycode = as.integer(0)
  }else{
    dummycode = as.integer(1)
  }

  # check the likert flag
  if(is.null(likert)){
    # TODO if dummycode is set, dummy code everything
    if(dummycode == 0){
      likert <- data.frame(
        # TODO need to make sure data has already been coerced to numeric
        apply(data, 2, min, na.rm = TRUE),
        apply(data, 2, max, na.rm = TRUE)
      )
    }else{
      likert <- NA
    }
  }else{
    # dimension of likert must match data
    if(nrow(likert) != ncol(data))
      stop("Likert and data dimensions incompatible (likert ", nrow(likert), ", data ", ncol(data), ")")

    if(!is.data.frame(likert))
      stop("Likert must be a data frame.")

    if(ncol(likert) != 2)
      stop("Likert must contain two columns, the min and max respectively of each data column.")

    # TODO check that first column is min and second max.

    if(!all(sapply(l, is.numeric))){
      # print out this info?
      #if(any(is.character(likert)))
      #if(any(is.na(likert)))
      #if(any(is.logical(likert)))
      stop("Likert must only contain numerical values.")
    }
     
    tmplikert <- data.frame(
      minval = apply(data, 2, min, na.rm = TRUE),
      maxval = apply(data, 2, max, na.rm = TRUE)
    )

    outlt <- which(tmplikert$minval < likert[[1]])
    outgt <- which(tmplikert$maxval > likert[[2]])
    outofrangeind <- unique(c(outlt, outgt))

    outofrange <- as.integer(0)
    if(length(outofrangeind) > 0)
      outofrange <- as.integer(1)

    # if outofrange and not dummycode, report that we're setting outliers to NA.
    if(outofrange && !dummycode){
      message("Columns ", paste(outofrangeind, collapse = ", ")," contain entries outside the specified range. Setting them to NA.")
      lapply(outofrangeind, function(col){
        outofrangevals <- data[
          data[, col] < likert[[1]][col] | data[, col] > likert[[2]][col], col
        ]
        message(col, ": ", paste(outofrangevals, collapse = ", "))
      })

      lapply(outofrangeind, function(col){
        data[data[, col] < likert[[1]][col] | data[, col] > likert[[2]][col], col] <- NA
      })
    }

    # if outofrange and dummycode, report which columns and entries will be affected.
    if(outofrange && dummycode){
      message("Columns ", paste(outofrangeind, collapse = ", ")," contain entries outside the specified range, and will be dummy-coded.")
      lapply(outofrangeind, function(col){
        outofrangevals <- data[
          data[, col] < likert[[1]][col] | data[, col] > likert[[2]][col], col
        ]
        message(col, ": ", paste(outofrangevals, collapse = ", "))
      })
    }

    # if not outofrange and dummycode, report that no dummy coding will take place.
    if(!outofrange && dummycode){
      warning("No dummy coding will take place, as no data lies outside the acceptable range.")
    }
  }

  # minimum comparisons
  if(is.null(mincomps)){
    mincomps = as.integer(ncol(data) / 2)
  }

  # check the value of similarity_matric
  if(is.null(similarity_metric)){
    if(layer == 0){
      similarity_metric <- as.integer(0)
    }else if(layer == 1){
      similarity_metric <- as.integer(0)
    }
  }else if(similarity_metric == "manhattan"){
    similarity_metric <- as.integer(0)
  }else{
    message("overriding similarity metric to manhattan distance") 
    similarity_metric <- as.integer(0)
  }

  e <- .Call("rmake_projection", 
             data, 
             layer,
             threshold_method,
             method_value, 
             centre, 
             likert,
             dummycode,
             mincomps,
             similarity_metric)
}
