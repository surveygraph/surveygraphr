## rsgraph, the surveygraph package for R

To build, we first use the Rcpp::compileAttributes method, which automatically
generates the file `R/RcppExports.R`. It would be nice to not have to depend on 
this.

Once R/RcppExports.R has been built, we can carry out a local build, by running
the shell command `$ Rscript -e 'package.install("rsgraph", repos=NULL, type="source")'`
which can also be run from the R interpreter.

`rcompile.R` combines the above, which we run using the shell command `$ Rscript rcompile.R`
