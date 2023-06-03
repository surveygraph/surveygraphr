# statically generates *.Rd files in man/ using doxygen2 via devtools
library("devtools")

devtools::document()
devtools::build_vignettes()
devtools::build()
