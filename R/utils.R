# dummy helper functions only called during development

helloooWorldNum <- function() {
  a <- as.numeric(1)
  b <- as.numeric(2)
  #print("you should do some type checking here")
  invisible(.Call(`surveygraphr_hw_num`, a, b))
}
