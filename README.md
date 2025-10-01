# surveygraph

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/surveygraph)](https://cran.r-project.org/package=surveygraph)
[![R-CMD-check](https://github.com/surveygraph/surveygraphr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/surveygraph/surveygraphr/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/surveygraph/surveygraphr/graph/badge.svg?token=SHFUE2Z36X)](https://app.codecov.io/gh/surveygraph/surveygraphr)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10887909.svg)](https://doi.org/10.5281/zenodo.10887909)
<!-- badges: end -->

## Overview
surveygraph is a lightweight library used to produce network representations of attitudes, as they emerge in tabular sociological data such as surveys.

We assume tabular data with rows representing agents, such as individuals, and columns representing items, such as survey questions. Table entries represent responses by agents to items. By computing the similarity between each pair of rows and each pair of columns we obtain, respectively,

1. the *agent* network, where nodes represent agents, with weighted edges drawn between similar agents
2. the *symbolic* network, where nodes represent items, with weighted edges drawn between similar items

By plotting these networks with tools such as igraph, we obtain accessible and visually striking representations of structure within the survey data.

## Installation

The surveygraph package can be installed via [CRAN](https://CRAN.R-project.org/package=surveygraph) as follows.

```
install.packages("surveygraph")
```

It can also be installed via [GitHub](https://github.com/surveygraph/surveygraphr/) using the devtools package as follows.

```
library("devtools")
devtools::install_github("surveygraph/surveygraphr")
```

After it has been installed, the package can be loaded in an R session using the command

```
library("surveygraph")
```

## Usage

Generate a synthetic survey dataset

```
S <- make_synthetic_data(500, 9, polarisation=0.2)
```

Compute a network representation of the survey respondents, retaining edges if the corresponding pair of respondents are sufficiently similar

```
e <- make_projection(S, layer="agent")

```

## Gallery

<a><img src="man/figures/eg1.jpg" align="center" height="238" /></a>

## References

Dinkelberg, A., O’Reilly, C., MacCarron, P., Maher, P. J., & Quayle, M. (2021). Multidimensional polarization dynamics in US election data in the long term (2012–2020) and in the 2020 election cycle. Analyses of Social Issues and Public Policy, 21(1), 284–311. https://doi.org/10.1111/asap.12278

MacCarron, P., Maher, P. J., & Quayle, M. (2020). Identifying opinion-based groups from survey data: A bipartite network approach. arXiv. http://arxiv.org/abs/2012.11392

Maher, P. J., MacCarron, P., & Quayle, M. (2020). Mapping public health responses with attitude networks: The emergence of opinion‐based groups in the UK’s early COVID‐19 response phase. British Journal of Social Psychology, 59(3), 641–652. https://doi.org/10.1111/bjso.12396


## Getting help

Please contact samuel.l.unicomb@ul.ie for support, and report bugs with a minimal illustrative example on [GitHub](https://github.com/surveygraph/surveygraphr/issues/). Since the package is in its infancy, we welcome feature requests and general feedback.

|   |  |
| ------------- | ------------- |
| ![ERC Logo](https://erc.europa.eu/sites/default/files/inline-images/LOGO_ERC-FLAG_FP.png) | Development of surveygraph software and training materials was initially funded by the European Union under the ERC Proof-of-concept programme (ERC,  Attitude-Maps-4-All, project number: 101069264). Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the European Research Council Executive Agency. Neither the European Union nor the granting authority can be held responsible for them.   |
|   |  |

