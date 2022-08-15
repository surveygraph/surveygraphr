# rsurveygraph, the surveygraph package for R

## Development notes

These comments describe my own workflow when working on this package. Some of 
the tools that I use, such as roxygen2 for documentation, of course will be 
absent from the final package. They're used locally only. In this document, commands preceded by `$` are to be run in a terminal, and `>` are to be run in an R interpreter. For me, that is the macOS interpreter for R, but I assume it's the same in RStudio.

#### Download

The package can be downloaded by running `> devtools::blahblah github("blah")`. When it is up on CRAN, of course there will be the option to install using the `install.packages` command as usual.

#### Install

In a terminal, we can install the package locally by running `$ R CMD INSTALL .`, assuming we're in the root directory of the project. Using absolute paths, on my machine this would be `$ R CMD INSTALL /Users/sam/rsurveygraph`.

#### Documentation

To build object documentation, we run `> devtools::document()`, which in turn calls `> roxygen2::roxygenise()`. As such, anyone contributing to this package must have roxygen2 installed. As usual, this can be done by running `> install.packages("roxygen2")`. A detailed guide on building documentation using roxygen2 can be found in Wickham, below, but in short, we comment the entrypoints (which amounts to the rsuveygraph API, if you like) in the `R` directory. These comments are made in markdown, then read by roxygen2 to produce \*.Rd files.

Further, roxygen2 will also build the NAMESPACE file based on your annotations of the source code in the `R` and `src` directories.

Note that the `data` directory should be reserved for R formatted data. In the words of Wickham, each file in this directory should be an .RData file created by `save()` containing a single object (with the same name as the file). The easiest way to adhere to these rules is by going `x <- sample(1000)`, then `usethis::use_data(x, egsample)`.

#### Interfacing with C/C++

I'd prefer to steer well clear of the Rcpp library, and interface using the basic API that R has for C/C++ extensions. These are laid out in the headers Rdefine.h, Rinternals.h, R.h, and so on. The location of these is given by `$ R RHOME`. Some helpful resources for learning the basics of  

## Bibliography

I have found the following resources to be extremely helpful

* CRAN documentation, "Writing R extensions" is an absolute must read
* writing R packages by Hadley Wickham
* anything about R development by Hadley Wickham in general
* the so-called tidyverse packages demonstrate R package best practice in my opinion. There are plenty of beautiful packages in there that we can model ours after, in that they 
