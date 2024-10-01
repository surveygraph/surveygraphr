# Notes on package development

In the following I outline the basic workflow that I follow when developing the
surveygraph package. The basic development commands as contained in `Makefile`
are

* `R CMD install .`
* `R CMD build .`
* `R CMD check surveygraph_0.1.3.tar.gz` 
* `R CMD check --as-cran surveygraph_0.1.3.tar.gz` 
* `Rscript -e "library('roxygen2'); roxygenise()"`

The latter statically generates \*.Rd files from `roxygen` comments in the
routines defined in R/\*.R. This is the same as running
`roxygen2::roxygenise()` in an R session. The resulting files will be in
`man/`. 

The README file in `src/` gives additional comments on the C++ source files.

On my machine (2021 MBP), R header files such as R.h and Rinternals.h can be
found in /opt/homebrew/Cellar/R/4.4.1/lib/R/include.
