helloWorldNum <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  #print("you should do some type checking here")
  invisible(.Call(`rsurveygraph_hw_num`, a, b))
}

helloWorldInt <- function(a, b) {
  a <- as.integer(a)
  b <- as.integer(b)
  #print("you should do some type checking here")
  invisible(.Call(`rsurveygraph_hw_int`, a, b))
}

dataframeCheck <- function(x) {
  #print("you should do some type checking here")
  invisible(.Call(`rsurveygraph_df_check`, x))
}

