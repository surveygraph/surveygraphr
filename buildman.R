# generates *.Rd files in man/ using doxygen2 via devtools
library("devtools")
library("pkgdown")

devtools::document()
devtools::build_vignettes()
devtools::build()

pkgdown::build_site()
