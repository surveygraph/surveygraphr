# surveygraphr, the surveygraph package for R

## Development workflow

These notes describe my personal workflow when working on this package. Some of the tools that I use, such as roxygen2 for documentation, of course will be absent from the final package. They're used locally only. In this document, commands are to be run either in a terminal, or an R interpreter, and I hope it's clear based on the context. For me, that is the macOS interpreter for R, but I assume it's the same in RStudio.

### Download

During development, the package can be downloaded from [GitHub](https://github.com/surveygraph/surveygraphr) organisation page by running the following commands in an R interpreter.

```
library(devtools)
install_github(surveygraph/surveygraphr)
```

When it is made available on [CRAN](https://cran.r-project.org/), of course there will be the option to install by running 

```
install.packages("surveygraph")
```

or similar, in the usual way. This is important since most R users won't be familiar with a GitHub install.

### Install

In a terminal, we can install the package locally by running `R CMD INSTALL .`, assuming we're in the root directory of the project. Using absolute paths, on my machine this would be `R CMD INSTALL /Users/sam/surveygraphr`.

### Documentation workflow

#### Object documentation

To build object documentation, we run `devtools::document()`, which in turn calls `roxygen2::roxygenise()`. As such, anyone contributing to this package must have roxygen2 installed. As usual, this can be done by running `install.packages("roxygen2")`. A detailed guide on building documentation using roxygen2 can be found in Wickham, below, but in short, we comment the entrypoints (which amounts to the rsuveygraph API, if you like) in the `R` directory. These comments are made in markdown, then read by roxygen2 to produce `*.Rd` files.

Further, roxygen2 will also build the NAMESPACE file based on your annotations of the source code in the `R` and `src` directories.

Note that the `data` directory should be reserved for R formatted data. In the words of Wickham, each file in this directory should be an .RData file created by `save()` containing a single object (with the same name as the file). The easiest way to adhere to these rules is by going `x <- sample(1000)`, then `usethis::use_data(x, egsample)`.

In summary, documentation is built by running

```
library("devtools")

devtools::document()
devtools::build_vignettes()
devtools::build()
```

The script `build-docs.r` contains these commands. Probably there's a cleaner way of automating this.

#### Vignettes

To build vignettes, we follow Wickham and use `knitr`, a markdown vignette engine. knitr is piloted by rmarkdown, which uses pandoc to convert between markdown and HTML. Note that pandoc is not an R library, check the [pandoc](https://pandoc.org/installing.html) page for installation instructions. For the minute, it seems like everything in the `doc` directory is build automatically based on the *.Rmd files in `vignettes`. Cycling through the build process a few more times should help with this.

### Interfacing with C/C++

I'd prefer to steer well clear of the Rcpp package, for a number of reasons that I won't discuss here. And interface using the basic API that R has for C/C++ extensions. These are laid out in the headers Rdefine.h, Rinternals.h, R.h, and so on. The location of these is given by `R RHOME`. Some helpful resources for learning the basics of the R API for C are given below. 

## Resources

In addition to the [CRAN](https://cran.r-project.org/) home page, which contains a handful of incredibly useful manuals in the documents section, I have found the following to be helpful

* the section [Writing R extensions](https://cran.r-project.org/) in the CRAN documentation is an absolute must read. However, there's a steep learning curve if you're not yet familiar with R's API for C
* [R packages](https://r-pkgs.org/) by Hadley Wickham is much more readable than hardcore CRAN documentation
* indeed anything about R development by [Hadley Wickham](https://hadley.nz/) I would strongly recommend, he seems to be one of the stars of the R community
* the so-called [tidyverse](https://github.com/tidyverse) suite of packages demonstrates R best practice in my opinion. There are plenty of beautiful packages in there on which we can model ours. Several include sophisticated C and C++ source libraries (Hadley Wickham is involved in a bunch of these... does he do freelance code review?) 


