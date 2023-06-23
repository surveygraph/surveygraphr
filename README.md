# surveygraph

<!-- badges: start -->
[![R-CMD-check](https://github.com/surveygraph/surveygraphr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/surveygraph/surveygraphr/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/surveygraph/surveygraphr/branch/main/graph/badge.svg?token=SHFUE2Z36X)](https://codecov.io/gh/surveygraph/surveygraphr)
<!-- badges: end -->

## Overview

surveygraph is a lightweight library used to produce network representations of attitudes, as they emerge in tabular sociological data such as surveys.

We assume tabular data with rows representing agents, such as individuals, and columns representing items, such as survey questions. Table entries represent responses by agents to items. By computing the similarity between each pair of rows and each pair of columns we obtain, respectively,

1. the *agent* network, where nodes represent agents, with weighted edges drawn between similar agents
2. the *symbolic* network, where nodes represent items, with weighted edges drawn between similar items

By plotting these networks with tools such as igraph, we obtain accessible and visually striking representations of structure within the survey data.

## Installation

Still in the development stage, the surveygraph package  is not yet available on CRAN, but can be installed from [GitHub](https://github.com/surveygraph/surveygraphr/) via the devtools package as follows.

```
library("devtools")
devtools::install_github("surveygraph/surveygraphr")
```

The package can then be loaded in an R session as follows.

```
library("surveygraph")
```

## Usage

To do.

## Gallery

To do.

## Getting help

Please contact samuel.l.unicomb@ul.ie for support, and report bugs with a minimal illustrative example on [GitHub](https://github.com/surveygraph/surveygraphr/issues/). Since the package is in its infancy, we are welcoming of feature requests and general feedback.
