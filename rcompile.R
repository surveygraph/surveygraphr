library("Rcpp")

Rcpp::compileAttributes("../rsgraph")

install.packages("../rsgraph", repos=NULL, type="source")
