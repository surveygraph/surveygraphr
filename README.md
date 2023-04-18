# surveygraphr, the _surveygraph_ package for R

## Development workflow

These notes describe my personal workflow when working on this package, and is subject to change.

### Download

A local version of the package can be obtained by downloading the zipped repository from our [GitHub](https://github.com/surveygraph/surveygraphr) organisation page, by selecting Code > Download ZIP. This downloads the entire repository, including files related to development that won't be included in the package available to end users. Or, if you use `git`, clone the repository in the usual way,

```sh
git clone git@github.com:surveygraph/surveygraphr.git
```

When it is made available on [CRAN](https://cran.r-project.org/), we'll be able to download and install in a single step by running 

```r
install.packages("surveygraph")
```

This is important for discoverability, and because most R users won't be familiar with a `git` workflow.

### Build and install from source


Note that neither of the download options (downloading a zip and git cloning) in the previous section actually installs the package. To build from the downloaded source (after unzipping if necessary), run

```sh
R CMD BUILD .
```

This compiles the `*.cc` source files to produce `*.o` object files, then bundles them to create a `*.so` shared object file. (The `build` command does a few other things too...). _[Say something here about ddl files, the NAMESPACE doc, R files etc etc.]_ To install the [?] files, we run

```sh
R CMD INSTALL .
```

and the package is ready to use in an interpreter. Alternatively, the package can be installed from GitHub by running the following commands in an R interpreter.

```r
library(devtools)
devtools::install_github(surveygraph/surveygraphr)
```

 Note that it remains to solve the awkward problem of overloading the term "surveygraph", which is the ideal final name of both the R and Python packages, but which necessitate different repository names in our organisation.

### Testing

A very handy R command is

```sh
R CMD check .           # run especially before CRAN submissions
R CMD SHLIB src/*.cc    # for building shared object files
```

Hadley Wickham has a great [chapter](https://r-pkgs.org/testing-basics.html) on testing. He recommends the third party package `testthat`.

### Debugging

Loading a shared object using `dyn.load` and running `.Call` directly

### Documentation workflow

#### Object documentation

To build object documentation, we run `devtools::document()`, which in turn calls `roxygen2::roxygenise()`. As such, anyone contributing to this package must have roxygen2 installed. As usual, this can be done by running `install.packages("roxygen2")`. A detailed guide on building documentation using `roxygen2` can be found in Wickham, below, but in short, we comment the entrypoints (which amounts to the rsuveygraph API, if you like) in the `R` directory. These comments are made in markdown, then read by `roxygen2` to produce `*.Rd` files.

Further, roxygen2 will also build the `NAMESPACE` file based on your annotations of the source code in the `R` and `src` directories.

Note that the `data` directory should be reserved for R formatted data. In the words of Wickham, each file in this directory should be an .RData file created by `save()` containing a single object (with the same name as the file). The easiest way to adhere to these rules is by going `x <- sample(1000)`, then `usethis::use_data(x, egsample)`.

In summary, documentation is built by running

```r
library("devtools")

devtools::document()
devtools::build_vignettes()
devtools::build()
```

The script `build-docs.r` contains these commands. There is surely a cleaner way of automating this.

#### Vignettes

To build vignettes, we follow Wickham and use `knitr`, a markdown vignette engine. `knitr` is piloted by `rmarkdown`, which uses `pandoc` to convert between markdown and HTML. Note that `pandoc` is not an R library, check the [pandoc](https://pandoc.org/installing.html) page for installation instructions. For the minute, it seems like everything in the `doc` directory is built automatically based on the `*.Rmd` files in `vignettes`. Iterating through the build process a few more times should help clarify this.

### R internals and C/C++

C and C++ extensions to R are often bound using the `Rcpp` package. Since our package is relatively simple, we avoid incurring the additional dependancy that `Rcpp` incurs, and instead perform bindings using the basic API that R provides for C/C++ extensions. Technically this binding is implemented in the header file `Rinternals.h`, with some additional material in `Rdefine.h` and `R.h`. The location of these is given by `R RHOME`, with the full paths on macOS being

```
/usr/local/Cellar/r/4.2.3/include/Rdefines.h
/usr/local/Cellar/r/4.2.3/include/Rdefines.h
/usr/local/Cellar/r/4.2.3/include/R.h
```

Some helpful resources for learning the basics of the R API for C are given below. 

#### Resources

I found the following resources to be most helpful in learning R's C API

* a gentle [overview](http://adv-r.had.co.nz/C-interface.html) by Hadley Wickham
* the authoritative [R internals](https://cran.r-project.org/doc/manuals/r-release/R-ints.html) manual by the core R team
* the above has been parsed and simplified in [R internals](https://github.com/hadley/r-internals) by Hadley Wickham

In addition these blogs by Jonathan Callahan helped me in the very beginning. An overview of the C interface can be found [here](https://www.r-bloggers.com/2012/11/using-r-calling-c-code-hello-world/) and [here](https://www.r-bloggers.com/2012/11/using-r-callhello/
), and a packaging walkthrough [here](https://www.r-bloggers.com/2012/11/using-r-packaging-a-c-library-in-15-minutes/).

## Usage

Here is a minimal working example of the package in its current version. The method `dummy(m, n)` generates a synthetic survey consisting of `n` questions to which `m` respondents answer on a one to five scale. Their responses are drawn at random from a uniform distribution. Graphs representing the correlations between respondents and questions are generated, and written to a `results/` directory as `graph1.dat` and `graph2.dat`, respectively. The generated survey is outputted as `survey.dat`. 

```r
library("surveygraph")

surveygraph::dummy(10000, 10)

file1 <- read.csv("results/survey1.dat")
file2 <- read.csv("results/graph1.dat")
file3 <- read.csv("results/graph2.dat")
```

## Resources

In addition to the [CRAN](https://cran.r-project.org/) home page, which contains a handful of incredibly useful manuals in the documents section, I have found the following to be helpful

* the section [Writing R extensions](https://cran.r-project.org/) in the CRAN documentation is an absolute must read. However, there's a steep learning curve if you're not yet familiar with R's API for C
* [R packages](https://r-pkgs.org/) by Hadley Wickham is an excellent guide to packaging best practice, and is much more readable than hardcore CRAN documentation
* indeed anything about R development by [Hadley Wickham](https://hadley.nz/) I would strongly recommend, he seems to be one of the stars of the R community
* the so-called [tidyverse](https://github.com/tidyverse) suite of packages demonstrates R best practice in my opinion. There are plenty of beautiful packages in there on which we can model ours. Several include sophisticated C and C++ source libraries (Hadley Wickham is involved in a bunch of these. Incidentally, it would be wonderful if he is available for code review. Can funding be allocated for this kind of thing?)


