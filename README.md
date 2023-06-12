# surveygraph

## Overview

**surveygraph** is a lightweight library used to produce network representations of attitudes, as they emerge in tabular sociological data such as surveys.

We assume tabular data with rows representing agents (such as individuals) and columns representing items (such as survey questions). By computing the similarity between each pair of rows and each pair of columns we obtain, respectively,

1. the *agent* network, where nodes represent agents, and weighted edges encode the similarity between adjacent agents
2. the *symbolic* network, where nodes represent items, and weighted edges measure the similarity of responses elicited across all agents

By plotting these networks with tools such as `igraph`, we obtain easily accessible and visually striking representations of structure within the survey data.

## Installation

Still in the development stage, the surveygraph package  is not yet available on CRAN, but can be installed from [GitHub](https://github.com/surveygraph/surveygraphr/) via the `devtools` package as follows.

```
library("devtools")
devtools::install_github("surveygraph/surveygraphr")
```

## Usage



## Getting help

Please contact samuel.l.unicomb@ul.ie for support, and report bugs with a minimal illustrative example on [GitHub](https://github.com/surveygraph/surveygraphr/issues/). Since the package is in its infancy, we are welcoming of feature requests and general feedback.
