The basic development commands are

* `R CMD install .`
* `R CMD build .`
* `R CMD check surveygraph_0.1.3.tar.gz` 
* `R CMD check --as-cran surveygraph_0.1.3.tar.gz` 

Statically generate \*.Rd files by running `roxygen2::roxygenise()` in an R
session. The resulting files will be in `man/`. 

On my machine (2021 MBP), R header files such as R.h and Rinternals.h can be
found in /opt/homebrew/Cellar/R/4.4.1/lib/R/include.
